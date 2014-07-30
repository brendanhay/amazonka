{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.ListUserPolicies
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the names of the policies associated with the specified user. If
-- there are none, the action returns an empty list. You can paginate the
-- results using the MaxItems and Marker parameters.
-- https://iam.amazonaws.com/ ?Action=ListUserPolicies &UserName=Bob
-- &AUTHPARAMS AllAccessPolicy KeyPolicy false
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.V2010_05_08.ListUserPolicies where

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

-- | Minimum specification for a 'ListUserPolicies' request.
listUserPolicies :: Text -- ^ '_luprUserName'
                 -> ListUserPolicies
listUserPolicies p1 = ListUserPolicies
    { _luprUserName = p1
    , _luprMarker = Nothing
    , _luprMaxItems = Nothing
    }

data ListUserPolicies = ListUserPolicies
    { _luprUserName :: Text
      -- ^ The name of the user to list policies for.
    , _luprMarker :: Maybe Text
      -- ^ Use this only when paginating results, and only in a subsequent
      -- request after you've received a response where the results are
      -- truncated. Set it to the value of the Marker element in the
      -- response you just received.
    , _luprMaxItems :: Maybe Integer
      -- ^ Use this only when paginating results to indicate the maximum
      -- number of policy names you want in the response. If there are
      -- additional policy names beyond the maximum you specify, the
      -- IsTruncated response element is true. This parameter is optional.
      -- If you do not include it, it defaults to 100.
    } deriving (Generic)

instance ToQuery ListUserPolicies where
    toQuery = genericToQuery def

instance AWSRequest ListUserPolicies where
    type Sv ListUserPolicies = IAM
    type Rs ListUserPolicies = ListUserPoliciesResponse

    request = post "ListUserPolicies"
    response _ = xmlResponse

instance AWSPager ListUserPolicies where
    next rq rs
        | not (_lupsIsTruncated rs) = Nothing
        | otherwise = Just $ rq
            { _luprMarker = _lupsMarker rs
            }

data ListUserPoliciesResponse = ListUserPoliciesResponse
    { _lupsIsTruncated :: Bool
      -- ^ A flag that indicates whether there are more policy names to
      -- list. If your results were truncated, you can make a subsequent
      -- pagination request using the Marker request parameter to retrieve
      -- more policy names in the list.
    , _lupsPolicyNames :: [Text]
      -- ^ A list of policy names.
    , _lupsMarker :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the
      -- value to use for the Marker parameter in a subsequent pagination
      -- request.
    } deriving (Generic)

instance FromXML ListUserPoliciesResponse where
    fromXMLOptions = xmlOptions
