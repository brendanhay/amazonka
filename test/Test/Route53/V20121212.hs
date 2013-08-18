{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- |
-- Module      : Test.Route53.V20121212
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Test.Route53.V20121212 (tests) where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString.Lazy.Char8           as LBS
import           Data.Data
import           Data.DeriveTH
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.Text                            (Text)
import           Data.Time
import           Network.AWS.Internal
import           Network.AWS.Route53.V20121212
import           Network.AWS.Route53.V20121212.Types
import           System.Locale
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck
import           Test.QuickCheck.Instances
import           Test.QuickCheck.Monadic

import           System.IO.Unsafe

version :: String
version = "V20121212"

tests :: [Test]
tests =
    [ testGroup version
        [ testGroup "XML Response Parsing"
            [ testProperty "CreateHostedZoneResponse" (res :: Res CreateHostedZoneResponse)
            ]
        ]
    ]

type Res a = Response a -> Bool

data Response a = Res
    { response :: a
    , template :: ByteString
    , xml      :: ByteString
    , parse    :: Either String a
    } deriving (Show)

instance (Eq a, Arbitrary a, Data a, Template a, IsXML a) => Arbitrary (Response a) where
    arbitrary = do
        i <- arbitrary

        let x = unsafePerformIO $ LBS.toStrict <$> render i

        return $ Res i (encodeXML i) x (decodeXML x)

res :: (Eq a, Arbitrary a) => Response a -> Bool
res (Res d _ _ i) = either (const False) (== d) i

$(derives [makeArbitrary]
    [ ''CreateHostedZoneResponse
    , ''DelegationSet
    , ''CallerRef
    , ''ChangeInfo
    , ''ChangeStatus
    , ''Config
    , ''HostedZone
    ])

$(embedTemplates "test/response/Route53/V20121212"
    [ ''CreateHostedZoneResponse
    ])
