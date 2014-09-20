{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.ListGroupPolicies
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
module Network.AWS.IAM.ListGroupPolicies
    (
    -- * Request
      ListGroupPolicies
    -- ** Request constructor
    , listGroupPolicies
    -- ** Request lenses
    , lgpGroupName
    , lgpMarker
    , lgpMaxItems

    -- * Response
    , ListGroupPoliciesResponse
    -- ** Response constructor
    , listGroupPoliciesResponse
    -- ** Response lenses
    , lgprPolicyNames
    , lgprIsTruncated
    , lgprMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

data ListGroupPolicies = ListGroupPolicies
    { _lgpGroupName :: Text
    , _lgpMarker :: Maybe Text
    , _lgpMaxItems :: Maybe Integer
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListGroupPolicies' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GroupName ::@ @Text@
--
-- * @Marker ::@ @Maybe Text@
--
-- * @MaxItems ::@ @Maybe Integer@
--
listGroupPolicies :: Text -- ^ 'lgpGroupName'
                  -> ListGroupPolicies
listGroupPolicies p1 = ListGroupPolicies
    { _lgpGroupName = p1
    , _lgpMarker = Nothing
    , _lgpMaxItems = Nothing
    }

-- | The name of the group to list policies for.
lgpGroupName :: Lens' ListGroupPolicies Text
lgpGroupName = lens _lgpGroupName (\s a -> s { _lgpGroupName = a })

-- | Use this only when paginating results, and only in a subsequent request
-- after you've received a response where the results are truncated. Set it to
-- the value of the Marker element in the response you just received.
lgpMarker :: Lens' ListGroupPolicies (Maybe Text)
lgpMarker = lens _lgpMarker (\s a -> s { _lgpMarker = a })

-- | Use this only when paginating results to indicate the maximum number of
-- policy names you want in the response. If there are additional policy names
-- beyond the maximum you specify, the IsTruncated response element is true.
-- This parameter is optional. If you do not include it, it defaults to 100.
lgpMaxItems :: Lens' ListGroupPolicies (Maybe Integer)
lgpMaxItems = lens _lgpMaxItems (\s a -> s { _lgpMaxItems = a })

instance ToQuery ListGroupPolicies where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the ListGroupPolicies
-- action.
data ListGroupPoliciesResponse = ListGroupPoliciesResponse
    { _lgprPolicyNames :: [Text]
    , _lgprIsTruncated :: !Bool
    , _lgprMarker :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListGroupPoliciesResponse' response.
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
listGroupPoliciesResponse :: [Text] -- ^ 'lgprPolicyNames'
                          -> Bool -- ^ 'lgprIsTruncated'
                          -> ListGroupPoliciesResponse
listGroupPoliciesResponse p1 p2 = ListGroupPoliciesResponse
    { _lgprPolicyNames = p1
    , _lgprIsTruncated = p2
    , _lgprMarker = Nothing
    }

-- | A list of policy names.
lgprPolicyNames :: Lens' ListGroupPoliciesResponse [Text]
lgprPolicyNames = lens _lgprPolicyNames (\s a -> s { _lgprPolicyNames = a })

-- | A flag that indicates whether there are more policy names to list. If your
-- results were truncated, you can make a subsequent pagination request using
-- the Marker request parameter to retrieve more policy names in the list.
lgprIsTruncated :: Lens' ListGroupPoliciesResponse Bool
lgprIsTruncated = lens _lgprIsTruncated (\s a -> s { _lgprIsTruncated = a })

-- | If IsTruncated is true, this element is present and contains the value to
-- use for the Marker parameter in a subsequent pagination request.
lgprMarker :: Lens' ListGroupPoliciesResponse (Maybe Text)
lgprMarker = lens _lgprMarker (\s a -> s { _lgprMarker = a })

instance FromXML ListGroupPoliciesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListGroupPolicies where
    type Sv ListGroupPolicies = IAM
    type Rs ListGroupPolicies = ListGroupPoliciesResponse

    request = post "ListGroupPolicies"
    response _ = xmlResponse

instance AWSPager ListGroupPolicies where
    next rq rs
        | not (rs ^. lgprIsTruncated) = Nothing
        | otherwise = Just $
            rq & lgpMarker .~ rs ^. lgprMarker
