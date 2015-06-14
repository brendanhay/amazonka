{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.IAM.GetAccountAuthorizationDetails
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Retrieves information about all IAM users, groups, roles, and policies
-- in your account, including their relationships to one another. Use this
-- API to obtain a snapshot of the configuration of IAM permissions (users,
-- groups, roles, and policies) in your account.
--
-- You can optionally filter the results using the @Filter@ parameter. You
-- can paginate the results using the @MaxItems@ and @Marker@ parameters.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetAccountAuthorizationDetails.html>
module Network.AWS.IAM.GetAccountAuthorizationDetails
    (
    -- * Request
      GetAccountAuthorizationDetails
    -- ** Request constructor
    , getAccountAuthorizationDetails
    -- ** Request lenses
    , gaadFilter
    , gaadMaxItems
    , gaadMarker

    -- * Response
    , GetAccountAuthorizationDetailsResponse
    -- ** Response constructor
    , getAccountAuthorizationDetailsResponse
    -- ** Response lenses
    , gaadrRoleDetailList
    , gaadrGroupDetailList
    , gaadrUserDetailList
    , gaadrIsTruncated
    , gaadrPolicies
    , gaadrMarker
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.IAM.Types

-- | /See:/ 'getAccountAuthorizationDetails' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gaadFilter'
--
-- * 'gaadMaxItems'
--
-- * 'gaadMarker'
data GetAccountAuthorizationDetails = GetAccountAuthorizationDetails'{_gaadFilter :: [EntityType], _gaadMaxItems :: Nat, _gaadMarker :: Text} deriving (Eq, Read, Show)

-- | 'GetAccountAuthorizationDetails' smart constructor.
getAccountAuthorizationDetails :: Natural -> Text -> GetAccountAuthorizationDetails
getAccountAuthorizationDetails pMaxItems pMarker = GetAccountAuthorizationDetails'{_gaadFilter = mempty, _gaadMaxItems = _Nat # pMaxItems, _gaadMarker = pMarker};

-- | A list of entity types (user, group, role, local managed policy, or AWS
-- managed policy) for filtering the results.
gaadFilter :: Lens' GetAccountAuthorizationDetails [EntityType]
gaadFilter = lens _gaadFilter (\ s a -> s{_gaadFilter = a});

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If there are additional items beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@. This
-- parameter is optional. If you do not include it, it defaults to 100.
gaadMaxItems :: Lens' GetAccountAuthorizationDetails Natural
gaadMaxItems = lens _gaadMaxItems (\ s a -> s{_gaadMaxItems = a}) . _Nat;

-- | Use this only when paginating results, and only in a subsequent request
-- after you\'ve received a response where the results are truncated. Set
-- it to the value of the @Marker@ element in the response you just
-- received.
gaadMarker :: Lens' GetAccountAuthorizationDetails Text
gaadMarker = lens _gaadMarker (\ s a -> s{_gaadMarker = a});

instance AWSRequest GetAccountAuthorizationDetails
         where
        type Sv GetAccountAuthorizationDetails = IAM
        type Rs GetAccountAuthorizationDetails =
             GetAccountAuthorizationDetailsResponse
        request = post
        response
          = receiveXMLWrapper
              "GetAccountAuthorizationDetailsResult"
              (\ s h x ->
                 GetAccountAuthorizationDetailsResponse' <$>
                   (x .@? "RoleDetailList" .!@ mempty >>=
                      parseXMLList "member")
                     <*>
                     (x .@? "GroupDetailList" .!@ mempty >>=
                        parseXMLList "member")
                     <*>
                     (x .@? "UserDetailList" .!@ mempty >>=
                        parseXMLList "member")
                     <*> x .@? "IsTruncated"
                     <*>
                     (x .@? "Policies" .!@ mempty >>=
                        parseXMLList "member")
                     <*> x .@ "Marker")

instance ToHeaders GetAccountAuthorizationDetails
         where
        toHeaders = const mempty

instance ToPath GetAccountAuthorizationDetails where
        toPath = const "/"

instance ToQuery GetAccountAuthorizationDetails where
        toQuery GetAccountAuthorizationDetails'{..}
          = mconcat
              ["Action" =:
                 ("GetAccountAuthorizationDetails" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "Filter" =: "member" =: _gaadFilter,
               "MaxItems" =: _gaadMaxItems, "Marker" =: _gaadMarker]

-- | /See:/ 'getAccountAuthorizationDetailsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gaadrRoleDetailList'
--
-- * 'gaadrGroupDetailList'
--
-- * 'gaadrUserDetailList'
--
-- * 'gaadrIsTruncated'
--
-- * 'gaadrPolicies'
--
-- * 'gaadrMarker'
data GetAccountAuthorizationDetailsResponse = GetAccountAuthorizationDetailsResponse'{_gaadrRoleDetailList :: [RoleDetail], _gaadrGroupDetailList :: [GroupDetail], _gaadrUserDetailList :: [UserDetail], _gaadrIsTruncated :: Maybe Bool, _gaadrPolicies :: [ManagedPolicyDetail], _gaadrMarker :: Text} deriving (Eq, Read, Show)

-- | 'GetAccountAuthorizationDetailsResponse' smart constructor.
getAccountAuthorizationDetailsResponse :: Text -> GetAccountAuthorizationDetailsResponse
getAccountAuthorizationDetailsResponse pMarker = GetAccountAuthorizationDetailsResponse'{_gaadrRoleDetailList = mempty, _gaadrGroupDetailList = mempty, _gaadrUserDetailList = mempty, _gaadrIsTruncated = Nothing, _gaadrPolicies = mempty, _gaadrMarker = pMarker};

-- | A list containing information about IAM roles.
gaadrRoleDetailList :: Lens' GetAccountAuthorizationDetailsResponse [RoleDetail]
gaadrRoleDetailList = lens _gaadrRoleDetailList (\ s a -> s{_gaadrRoleDetailList = a});

-- | A list containing information about IAM groups.
gaadrGroupDetailList :: Lens' GetAccountAuthorizationDetailsResponse [GroupDetail]
gaadrGroupDetailList = lens _gaadrGroupDetailList (\ s a -> s{_gaadrGroupDetailList = a});

-- | A list containing information about IAM users.
gaadrUserDetailList :: Lens' GetAccountAuthorizationDetailsResponse [UserDetail]
gaadrUserDetailList = lens _gaadrUserDetailList (\ s a -> s{_gaadrUserDetailList = a});

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items.
gaadrIsTruncated :: Lens' GetAccountAuthorizationDetailsResponse (Maybe Bool)
gaadrIsTruncated = lens _gaadrIsTruncated (\ s a -> s{_gaadrIsTruncated = a});

-- | A list containing information about managed policies.
gaadrPolicies :: Lens' GetAccountAuthorizationDetailsResponse [ManagedPolicyDetail]
gaadrPolicies = lens _gaadrPolicies (\ s a -> s{_gaadrPolicies = a});

-- | If @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
gaadrMarker :: Lens' GetAccountAuthorizationDetailsResponse Text
gaadrMarker = lens _gaadrMarker (\ s a -> s{_gaadrMarker = a});
