{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.ListRolePolicies
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
module Network.AWS.IAM
    (
    -- * Request
      ListRolePolicies
    -- ** Request constructor
    , mkListRolePolicies
    -- ** Request lenses
    , lrpRoleName
    , lrpMarker
    , lrpMaxItems

    -- * Response
    , ListRolePoliciesResponse
    -- ** Response constructor
    , mkListRolePoliciesResponse
    -- ** Response lenses
    , lrprPolicyNames
    , lrprIsTruncated
    , lrprMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

data ListRolePolicies = ListRolePolicies
    { _lrpRoleName :: !Text
    , _lrpMarker :: !(Maybe Text)
    , _lrpMaxItems :: !(Maybe Integer)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListRolePolicies' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @RoleName ::@ @Text@
--
-- * @Marker ::@ @Maybe Text@
--
-- * @MaxItems ::@ @Maybe Integer@
--
mkListRolePolicies :: Text -- ^ 'lrpRoleName'
                   -> ListRolePolicies
mkListRolePolicies p1 = ListRolePolicies
    { _lrpRoleName = p1
    , _lrpMarker = Nothing
    , _lrpMaxItems = Nothing
    }

-- | The name of the role to list policies for.
lrpRoleName :: Lens' ListRolePolicies Text
lrpRoleName = lens _lrpRoleName (\s a -> s { _lrpRoleName = a })

-- | Use this parameter only when paginating results, and only in a subsequent
-- request after you've received a response where the results are truncated.
-- Set it to the value of the Marker element in the response you just
-- received.
lrpMarker :: Lens' ListRolePolicies (Maybe Text)
lrpMarker = lens _lrpMarker (\s a -> s { _lrpMarker = a })

-- | Use this parameter only when paginating results to indicate the maximum
-- number of user names you want in the response. If there are additional user
-- names beyond the maximum you specify, the IsTruncated response element is
-- true. This parameter is optional. If you do not include it, it defaults to
-- 100.
lrpMaxItems :: Lens' ListRolePolicies (Maybe Integer)
lrpMaxItems = lens _lrpMaxItems (\s a -> s { _lrpMaxItems = a })

instance ToQuery ListRolePolicies where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the ListRolePolicies
-- action.
data ListRolePoliciesResponse = ListRolePoliciesResponse
    { _lrprPolicyNames :: [Text]
    , _lrprIsTruncated :: !Bool
    , _lrprMarker :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListRolePoliciesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @PolicyNames ::@ @[Text]@
--
-- * @IsTruncated ::@ @Bool@
--
-- * @Marker ::@ @Maybe Text@
--
mkListRolePoliciesResponse :: [Text] -- ^ 'lrprPolicyNames'
                           -> Bool -- ^ 'lrprIsTruncated'
                           -> ListRolePoliciesResponse
mkListRolePoliciesResponse p1 p2 = ListRolePoliciesResponse
    { _lrprPolicyNames = p1
    , _lrprIsTruncated = p2
    , _lrprMarker = Nothing
    }

-- | A list of policy names.
lrprPolicyNames :: Lens' ListRolePoliciesResponse [Text]
lrprPolicyNames = lens _lrprPolicyNames (\s a -> s { _lrprPolicyNames = a })

-- | A flag that indicates whether there are more policy names to list. If your
-- results were truncated, you can make a subsequent pagination request using
-- the Marker request parameter to retrieve more policy names in the list.
lrprIsTruncated :: Lens' ListRolePoliciesResponse Bool
lrprIsTruncated = lens _lrprIsTruncated (\s a -> s { _lrprIsTruncated = a })

-- | If IsTruncated is true, this element is present and contains the value to
-- use for the Marker parameter in a subsequent pagination request.
lrprMarker :: Lens' ListRolePoliciesResponse (Maybe Text)
lrprMarker = lens _lrprMarker (\s a -> s { _lrprMarker = a })

instance FromXML ListRolePoliciesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListRolePolicies where
    type Sv ListRolePolicies = IAM
    type Rs ListRolePolicies = ListRolePoliciesResponse

    request = post "ListRolePolicies"
    response _ = xmlResponse

instance AWSPager ListRolePolicies where
    next rq rs
        | not (rs ^. lrprIsTruncated) = Nothing
        | otherwise = Just $
            rq & lrpMarker .~ rs ^. lrprMarker
