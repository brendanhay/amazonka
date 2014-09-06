{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.IAM.V2010_05_08.ListGroupPolicies
    (
    -- * Request
      ListGroupPolicies
    -- ** Request constructor
    , mkListGroupPolicies
    -- ** Request lenses
    , lgpGroupName
    , lgpMarker
    , lgpMaxItems

    -- * Response
    , ListGroupPoliciesResponse
    -- ** Response lenses
    , lgprsPolicyNames
    , lgprsIsTruncated
    , lgprsMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | 
data ListGroupPolicies = ListGroupPolicies
    { _lgpGroupName :: Text
    , _lgpMarker :: Maybe Text
    , _lgpMaxItems :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListGroupPolicies' request.
mkListGroupPolicies :: Text -- ^ 'lgpGroupName'
                    -> ListGroupPolicies
mkListGroupPolicies p1 = ListGroupPolicies
    { _lgpGroupName = p1
    , _lgpMarker = Nothing
    , _lgpMaxItems = Nothing
    }
{-# INLINE mkListGroupPolicies #-}

-- | The name of the group to list policies for.
lgpGroupName :: Lens' ListGroupPolicies Text
lgpGroupName = lens _lgpGroupName (\s a -> s { _lgpGroupName = a })
{-# INLINE lgpGroupName #-}

-- | Use this only when paginating results, and only in a subsequent request
-- after you've received a response where the results are truncated. Set it to
-- the value of the Marker element in the response you just received.
lgpMarker :: Lens' ListGroupPolicies (Maybe Text)
lgpMarker = lens _lgpMarker (\s a -> s { _lgpMarker = a })
{-# INLINE lgpMarker #-}

-- | Use this only when paginating results to indicate the maximum number of
-- policy names you want in the response. If there are additional policy names
-- beyond the maximum you specify, the IsTruncated response element is true.
-- This parameter is optional. If you do not include it, it defaults to 100.
lgpMaxItems :: Lens' ListGroupPolicies (Maybe Integer)
lgpMaxItems = lens _lgpMaxItems (\s a -> s { _lgpMaxItems = a })
{-# INLINE lgpMaxItems #-}

instance ToQuery ListGroupPolicies where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the ListGroupPolicies
-- action.
data ListGroupPoliciesResponse = ListGroupPoliciesResponse
    { _lgprsPolicyNames :: [Text]
    , _lgprsIsTruncated :: Maybe Bool
    , _lgprsMarker :: Maybe Text
    } deriving (Show, Generic)

-- | A list of policy names.
lgprsPolicyNames :: Lens' ListGroupPoliciesResponse [Text]
lgprsPolicyNames =
    lens _lgprsPolicyNames (\s a -> s { _lgprsPolicyNames = a })
{-# INLINE lgprsPolicyNames #-}

-- | A flag that indicates whether there are more policy names to list. If your
-- results were truncated, you can make a subsequent pagination request using
-- the Marker request parameter to retrieve more policy names in the list.
lgprsIsTruncated :: Lens' ListGroupPoliciesResponse (Maybe Bool)
lgprsIsTruncated =
    lens _lgprsIsTruncated (\s a -> s { _lgprsIsTruncated = a })
{-# INLINE lgprsIsTruncated #-}

-- | If IsTruncated is true, this element is present and contains the value to
-- use for the Marker parameter in a subsequent pagination request.
lgprsMarker :: Lens' ListGroupPoliciesResponse (Maybe Text)
lgprsMarker = lens _lgprsMarker (\s a -> s { _lgprsMarker = a })
{-# INLINE lgprsMarker #-}

instance FromXML ListGroupPoliciesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListGroupPolicies where
    type Sv ListGroupPolicies = IAM
    type Rs ListGroupPolicies = ListGroupPoliciesResponse

    request = post "ListGroupPolicies"
    response _ = xmlResponse

instance AWSPager ListGroupPolicies where
    next rq rs
        | not (_lgprsIsTruncated rs) = Nothing
        | otherwise = Just $ rq
            { _lgpMarker = _lgprsMarker rs
            }
