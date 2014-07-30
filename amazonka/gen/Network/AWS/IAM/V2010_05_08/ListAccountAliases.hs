{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.ListAccountAliases
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the account aliases associated with the account. For information
-- about using an AWS account alias, see Using an Alias for Your AWS Account
-- ID in the Using IAM guide. You can paginate the results using the MaxItems
-- and Marker parameters. https://iam.amazonaws.com/
-- ?Action=ListAccountAliases &Version=2010-05-08 &AUTHPARAMS false
-- foocorporation c5a076e9-f1b0-11df-8fbe-45274EXAMPLE.
module Network.AWS.IAM.V2010_05_08.ListAccountAliases where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.IAM.V2010_05_08.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

-- | Minimum specification for a 'ListAccountAliases' request.
listAccountAliases :: ListAccountAliases
listAccountAliases = ListAccountAliases
    { _laarMarker = Nothing
    , _laarMaxItems = Nothing
    }

data ListAccountAliases = ListAccountAliases
    { _laarMarker :: Maybe Text
      -- ^ Use this only when paginating results, and only in a subsequent
      -- request after you've received a response where the results are
      -- truncated. Set it to the value of the Marker element in the
      -- response you just received.
    , _laarMaxItems :: Maybe Integer
      -- ^ Use this only when paginating results to indicate the maximum
      -- number of account aliases you want in the response. If there are
      -- additional account aliases beyond the maximum you specify, the
      -- IsTruncated response element is true. This parameter is optional.
      -- If you do not include it, it defaults to 100.
    } deriving (Generic)

instance ToQuery ListAccountAliases where
    toQuery = genericToQuery def

instance AWSRequest ListAccountAliases where
    type Sv ListAccountAliases = IAM
    type Rs ListAccountAliases = ListAccountAliasesResponse

    request = post "ListAccountAliases"
    response _ = xmlResponse

instance AWSPager ListAccountAliases where
    next rq rs
        | not (_laasIsTruncated rs) = Nothing
        | otherwise = Just $ rq
            { _laarMarker = _laasMarker rs
            }

data ListAccountAliasesResponse = ListAccountAliasesResponse
    { _laasAccountAliases :: [Text]
      -- ^ A list of aliases associated with the account.
    , _laasIsTruncated :: Bool
      -- ^ A flag that indicates whether there are more account aliases to
      -- list. If your results were truncated, you can make a subsequent
      -- pagination request using the Marker request parameter to retrieve
      -- more account aliases in the list.
    , _laasMarker :: Maybe Text
      -- ^ Use this only when paginating results, and only in a subsequent
      -- request after you've received a response where the results are
      -- truncated. Set it to the value of the Marker element in the
      -- response you just received.
    } deriving (Generic)

instance FromXML ListAccountAliasesResponse where
    fromXMLOptions = xmlOptions
