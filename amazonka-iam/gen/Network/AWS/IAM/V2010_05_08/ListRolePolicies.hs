{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.ListRolePolicies
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the names of the policies associated with the specified role. If
-- there are none, the action returns an empty list. You can paginate the
-- results using the MaxItems and Marker parameters.
-- https://iam.amazonaws.com/ ?Action=ListRolePolicies &MaxItems=100
-- &RoleName=S3Access &Version=2010-05-08 &AUTHPARAMS
-- CloudwatchPutMetricPolicy S3AccessPolicy false
-- 8c7e1816-99f0-11e1-a4c3-27EXAMPLE804.
module Network.AWS.IAM.V2010_05_08.ListRolePolicies where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ListRolePolicies' request.
listRolePolicies :: Text -- ^ '_lrprRoleName'
                 -> ListRolePolicies
listRolePolicies p1 = ListRolePolicies
    { _lrprRoleName = p1
    , _lrprMarker = Nothing
    , _lrprMaxItems = Nothing
    }

data ListRolePolicies = ListRolePolicies
    { _lrprRoleName :: Text
      -- ^ The name of the role to list policies for.
    , _lrprMarker :: Maybe Text
      -- ^ Use this parameter only when paginating results, and only in a
      -- subsequent request after you've received a response where the
      -- results are truncated. Set it to the value of the Marker element
      -- in the response you just received.
    , _lrprMaxItems :: Maybe Integer
      -- ^ Use this parameter only when paginating results to indicate the
      -- maximum number of user names you want in the response. If there
      -- are additional user names beyond the maximum you specify, the
      -- IsTruncated response element is true. This parameter is optional.
      -- If you do not include it, it defaults to 100.
    } deriving (Show, Generic)

makeLenses ''ListRolePolicies

instance ToQuery ListRolePolicies where
    toQuery = genericQuery def

data ListRolePoliciesResponse = ListRolePoliciesResponse
    { _lrpsIsTruncated :: Bool
      -- ^ A flag that indicates whether there are more policy names to
      -- list. If your results were truncated, you can make a subsequent
      -- pagination request using the Marker request parameter to retrieve
      -- more policy names in the list.
    , _lrpsPolicyNames :: [Text]
      -- ^ A list of policy names.
    , _lrpsMarker :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the
      -- value to use for the Marker parameter in a subsequent pagination
      -- request.
    } deriving (Show, Generic)

makeLenses ''ListRolePoliciesResponse

instance FromXML ListRolePoliciesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListRolePolicies where
    type Sv ListRolePolicies = IAM
    type Rs ListRolePolicies = ListRolePoliciesResponse

    request = post "ListRolePolicies"
    response _ = xmlResponse

instance AWSPager ListRolePolicies where
    next rq rs
        | not (_lrpsIsTruncated rs) = Nothing
        | otherwise = Just $ rq
            { _lrprMarker = _lrpsMarker rs
            }
