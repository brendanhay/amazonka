{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.IAM.V2010_05_08.ListGroupPolicies
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the names of the policies associated with the specified group. If
-- there are none, the action returns an empty list. You can paginate the
-- results using the MaxItems and Marker parameters.
-- https://iam.amazonaws.com/ ?Action=ListGroupPolicies &GroupName=Admins
-- &AUTHPARAMS AdminRoot KeyPolicy false 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.V2010_05_08.ListGroupPolicies where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ListGroupPolicies' request.
listGroupPolicies :: Text -- ^ '_lgprGroupName'
                  -> ListGroupPolicies
listGroupPolicies p1 = ListGroupPolicies
    { _lgprGroupName = p1
    , _lgprMarker = Nothing
    , _lgprMaxItems = Nothing
    }

data ListGroupPolicies = ListGroupPolicies
    { _lgprGroupName :: Text
      -- ^ The name of the group to list policies for.
    , _lgprMarker :: Maybe Text
      -- ^ Use this only when paginating results, and only in a subsequent
      -- request after you've received a response where the results are
      -- truncated. Set it to the value of the Marker element in the
      -- response you just received.
    , _lgprMaxItems :: Maybe Integer
      -- ^ Use this only when paginating results to indicate the maximum
      -- number of policy names you want in the response. If there are
      -- additional policy names beyond the maximum you specify, the
      -- IsTruncated response element is true. This parameter is optional.
      -- If you do not include it, it defaults to 100.
    } deriving (Generic)

makeLenses ''ListGroupPolicies

instance ToQuery ListGroupPolicies where
    toQuery = genericToQuery def

data ListGroupPoliciesResponse = ListGroupPoliciesResponse
    { _lgpsIsTruncated :: Bool
      -- ^ A flag that indicates whether there are more policy names to
      -- list. If your results were truncated, you can make a subsequent
      -- pagination request using the Marker request parameter to retrieve
      -- more policy names in the list.
    , _lgpsPolicyNames :: [Text]
      -- ^ A list of policy names.
    , _lgpsMarker :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the
      -- value to use for the Marker parameter in a subsequent pagination
      -- request.
    } deriving (Generic)

makeLenses ''ListGroupPoliciesResponse

instance FromXML ListGroupPoliciesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListGroupPolicies where
    type Sv ListGroupPolicies = IAM
    type Rs ListGroupPolicies = ListGroupPoliciesResponse

    request = post "ListGroupPolicies"
    response _ = xmlResponse

instance AWSPager ListGroupPolicies where
    next rq rs
        | not (Keyed "_lgpsIsTruncated" rs) = Nothing
        | otherwise = Just $ rq
            { Keyed "_lgprMarker" = Keyed "_lgpsMarker" rs
            }
