{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Test.Common
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Test.Common
    (
    -- * Test Case Setup
      testVersion

    -- * Request Tests
    , Req
    , req

    -- * Response Tests
    , Res
    , res

    -- * Re-used Imports
    , module Test
    , module Common
    ) where

import           Control.Applicative
import           Data.Aeson                           as Common (Value(..), ToJSON(..), FromJSON(..))
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString.Char8                as BS
import qualified Data.ByteString.Lazy.Char8           as LBS
import           Data.Monoid
import           Network.AWS.Internal                 as Common
import           System.IO.Unsafe                     (unsafePerformIO)
import           Test.Arbitrary                       ()
import           Test.Framework                       as Test
import           Test.Framework.Providers.QuickCheck2 as Test
import           Test.QuickCheck                      as Test
import           Test.TH                              as Test
import           Text.Hastache
import           Text.Hastache.Aeson

testVersion :: ByteString -> [Test] -> Test
testVersion ver = plusTestOptions
    (mempty { topt_maximum_test_size = Just 50 }) . testGroup (BS.unpack ver)


-- rename to encoding.parsing and so on

type Req a = Response a -> Bool

req :: (Eq a, Arbitrary a) => Res a
req (Res _ t x _) = normalise t == normalise x
  where
    normalise = BS.unlines . map (BS.unwords . BS.words) . BS.lines

type Res a = Response a -> Bool

res :: (Eq a, Arbitrary a) => Res a
res (Res d _ _ i) = either (const False) (== d) i

data Response a = Res
    { resResponse :: a
    , resTemplate :: ByteString
    , resXML      :: ByteString
    , resParsed   :: Either String a
    }

instance Show a => Show (Response a) where
    show Res{..} = unlines
        [ "[Response]"
        , show resResponse
        , ""
        , "[Parsed]"
        , show resParsed
        , ""
        , "[Template]"
        , BS.unpack resTemplate
        , "[Encoded]"
        , BS.unpack resXML
        ]

instance (Eq a, Show a, Arbitrary a, Template a, IsXML a, ToJSON a)
         => Arbitrary (Response a) where
    arbitrary = do
        inp <- arbitrary
        let tmpl = unsafePerformIO $ LBS.toStrict <$> render inp
            xml  = toIndentedXML 2 inp
        return . Res inp tmpl xml $ fromXML tmpl
      where
        render x = hastacheStr defaultConfig (readTemplate x)
            (jsonValueContext $ toJSON x)
