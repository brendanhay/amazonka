{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.ListUserPolicies
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
module Network.AWS.IAM.ListUserPolicies
    (
    -- * Request
      ListUserPolicies
    -- ** Request constructor
    , mkListUserPolicies
    -- ** Request lenses
    , lupUserName
    , lupMarker
    , lupMaxItems

    -- * Response
    , ListUserPoliciesResponse
    -- ** Response constructor
    , mkListUserPoliciesResponse
    -- ** Response lenses
    , luprPolicyNames
    , luprIsTruncated
    , luprMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

data ListUserPolicies = ListUserPolicies
    { _lupUserName :: Text
    , _lupMarker :: Maybe Text
    , _lupMaxItems :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListUserPolicies' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @UserName ::@ @Text@
--
-- * @Marker ::@ @Maybe Text@
--
-- * @MaxItems ::@ @Maybe Integer@
--
mkListUserPolicies :: Text -- ^ 'lupUserName'
                   -> ListUserPolicies
mkListUserPolicies p1 = ListUserPolicies
    { _lupUserName = p1
    , _lupMarker = Nothing
    , _lupMaxItems = Nothing
    }

-- | The name of the user to list policies for.
lupUserName :: Lens' ListUserPolicies Text
lupUserName = lens _lupUserName (\s a -> s { _lupUserName = a })

-- | Use this only when paginating results, and only in a subsequent request
-- after you've received a response where the results are truncated. Set it to
-- the value of the Marker element in the response you just received.
lupMarker :: Lens' ListUserPolicies (Maybe Text)
lupMarker = lens _lupMarker (\s a -> s { _lupMarker = a })

-- | Use this only when paginating results to indicate the maximum number of
-- policy names you want in the response. If there are additional policy names
-- beyond the maximum you specify, the IsTruncated response element is true.
-- This parameter is optional. If you do not include it, it defaults to 100.
lupMaxItems :: Lens' ListUserPolicies (Maybe Integer)
lupMaxItems = lens _lupMaxItems (\s a -> s { _lupMaxItems = a })

instance ToQuery ListUserPolicies where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the ListUserPolicies
-- action.
data ListUserPoliciesResponse = ListUserPoliciesResponse
    { _luprPolicyNames :: [Text]
    , _luprIsTruncated :: !Bool
    , _luprMarker :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListUserPoliciesResponse' response.
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
mkListUserPoliciesResponse :: [Text] -- ^ 'luprPolicyNames'
                           -> Bool -- ^ 'luprIsTruncated'
                           -> ListUserPoliciesResponse
mkListUserPoliciesResponse p1 p2 = ListUserPoliciesResponse
    { _luprPolicyNames = p1
    , _luprIsTruncated = p2
    , _luprMarker = Nothing
    }

-- | A list of policy names.
luprPolicyNames :: Lens' ListUserPoliciesResponse [Text]
luprPolicyNames = lens _luprPolicyNames (\s a -> s { _luprPolicyNames = a })

-- | A flag that indicates whether there are more policy names to list. If your
-- results were truncated, you can make a subsequent pagination request using
-- the Marker request parameter to retrieve more policy names in the list.
luprIsTruncated :: Lens' ListUserPoliciesResponse Bool
luprIsTruncated = lens _luprIsTruncated (\s a -> s { _luprIsTruncated = a })

-- | If IsTruncated is true, this element is present and contains the value to
-- use for the Marker parameter in a subsequent pagination request.
luprMarker :: Lens' ListUserPoliciesResponse (Maybe Text)
luprMarker = lens _luprMarker (\s a -> s { _luprMarker = a })

instance FromXML ListUserPoliciesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListUserPolicies where
    type Sv ListUserPolicies = IAM
    type Rs ListUserPolicies = ListUserPoliciesResponse

    request = post "ListUserPolicies"
    response _ = xmlResponse

instance AWSPager ListUserPolicies where
    next rq rs
        | not (rs ^. luprIsTruncated) = Nothing
        | otherwise = Just $
            rq & lupMarker .~ rs ^. luprMarker
