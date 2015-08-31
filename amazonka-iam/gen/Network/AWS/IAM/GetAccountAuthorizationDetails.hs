{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetAccountAuthorizationDetails
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about all IAM users, groups, roles, and policies
-- in your account, including their relationships to one another. Use this
-- API to obtain a snapshot of the configuration of IAM permissions (users,
-- groups, roles, and policies) in your account.
--
-- You can optionally filter the results using the 'Filter' parameter. You
-- can paginate the results using the 'MaxItems' and 'Marker' parameters.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetAccountAuthorizationDetails.html AWS API Reference> for GetAccountAuthorizationDetails.
module Network.AWS.IAM.GetAccountAuthorizationDetails
    (
    -- * Creating a Request
      getAccountAuthorizationDetails
    , GetAccountAuthorizationDetails
    -- * Request Lenses
    , gaadMarker
    , gaadMaxItems
    , gaadFilter

    -- * Destructuring the Response
    , getAccountAuthorizationDetailsResponse
    , GetAccountAuthorizationDetailsResponse
    -- * Response Lenses
    , gaadrsRoleDetailList
    , gaadrsGroupDetailList
    , gaadrsUserDetailList
    , gaadrsMarker
    , gaadrsIsTruncated
    , gaadrsPolicies
    , gaadrsStatus
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getAccountAuthorizationDetails' smart constructor.
data GetAccountAuthorizationDetails = GetAccountAuthorizationDetails'
    { _gaadMarker   :: !(Maybe Text)
    , _gaadMaxItems :: !(Maybe Nat)
    , _gaadFilter   :: !(Maybe [EntityType])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetAccountAuthorizationDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaadMarker'
--
-- * 'gaadMaxItems'
--
-- * 'gaadFilter'
getAccountAuthorizationDetails
    :: GetAccountAuthorizationDetails
getAccountAuthorizationDetails =
    GetAccountAuthorizationDetails'
    { _gaadMarker = Nothing
    , _gaadMaxItems = Nothing
    , _gaadFilter = Nothing
    }

-- | Use this parameter only when paginating results and only after you have
-- received a response where the results are truncated. Set it to the value
-- of the 'Marker' element in the response you just received.
gaadMarker :: Lens' GetAccountAuthorizationDetails (Maybe Text)
gaadMarker = lens _gaadMarker (\ s a -> s{_gaadMarker = a});

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If there are additional items beyond the
-- maximum you specify, the 'IsTruncated' response element is 'true'.
--
-- This parameter is optional. If you do not include it, it defaults to
-- 100.
gaadMaxItems :: Lens' GetAccountAuthorizationDetails (Maybe Natural)
gaadMaxItems = lens _gaadMaxItems (\ s a -> s{_gaadMaxItems = a}) . mapping _Nat;

-- | A list of entity types (user, group, role, local managed policy, or AWS
-- managed policy) for filtering the results.
gaadFilter :: Lens' GetAccountAuthorizationDetails [EntityType]
gaadFilter = lens _gaadFilter (\ s a -> s{_gaadFilter = a}) . _Default . _Coerce;

instance AWSRequest GetAccountAuthorizationDetails
         where
        type Rs GetAccountAuthorizationDetails =
             GetAccountAuthorizationDetailsResponse
        request = postQuery iAM
        response
          = receiveXMLWrapper
              "GetAccountAuthorizationDetailsResult"
              (\ s h x ->
                 GetAccountAuthorizationDetailsResponse' <$>
                   (x .@? "RoleDetailList" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*>
                     (x .@? "GroupDetailList" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*>
                     (x .@? "UserDetailList" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (x .@? "Marker")
                     <*> (x .@? "IsTruncated")
                     <*>
                     (x .@? "Policies" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

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
               "Marker" =: _gaadMarker, "MaxItems" =: _gaadMaxItems,
               "Filter" =:
                 toQuery (toQueryList "member" <$> _gaadFilter)]

-- | Contains the response to a successful GetAccountAuthorizationDetails
-- request.
--
-- /See:/ 'getAccountAuthorizationDetailsResponse' smart constructor.
data GetAccountAuthorizationDetailsResponse = GetAccountAuthorizationDetailsResponse'
    { _gaadrsRoleDetailList  :: !(Maybe [RoleDetail])
    , _gaadrsGroupDetailList :: !(Maybe [GroupDetail])
    , _gaadrsUserDetailList  :: !(Maybe [UserDetail])
    , _gaadrsMarker          :: !(Maybe Text)
    , _gaadrsIsTruncated     :: !(Maybe Bool)
    , _gaadrsPolicies        :: !(Maybe [ManagedPolicyDetail])
    , _gaadrsStatus          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetAccountAuthorizationDetailsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaadrsRoleDetailList'
--
-- * 'gaadrsGroupDetailList'
--
-- * 'gaadrsUserDetailList'
--
-- * 'gaadrsMarker'
--
-- * 'gaadrsIsTruncated'
--
-- * 'gaadrsPolicies'
--
-- * 'gaadrsStatus'
getAccountAuthorizationDetailsResponse
    :: Int -- ^ 'gaadrsStatus'
    -> GetAccountAuthorizationDetailsResponse
getAccountAuthorizationDetailsResponse pStatus_ =
    GetAccountAuthorizationDetailsResponse'
    { _gaadrsRoleDetailList = Nothing
    , _gaadrsGroupDetailList = Nothing
    , _gaadrsUserDetailList = Nothing
    , _gaadrsMarker = Nothing
    , _gaadrsIsTruncated = Nothing
    , _gaadrsPolicies = Nothing
    , _gaadrsStatus = pStatus_
    }

-- | A list containing information about IAM roles.
gaadrsRoleDetailList :: Lens' GetAccountAuthorizationDetailsResponse [RoleDetail]
gaadrsRoleDetailList = lens _gaadrsRoleDetailList (\ s a -> s{_gaadrsRoleDetailList = a}) . _Default . _Coerce;

-- | A list containing information about IAM groups.
gaadrsGroupDetailList :: Lens' GetAccountAuthorizationDetailsResponse [GroupDetail]
gaadrsGroupDetailList = lens _gaadrsGroupDetailList (\ s a -> s{_gaadrsGroupDetailList = a}) . _Default . _Coerce;

-- | A list containing information about IAM users.
gaadrsUserDetailList :: Lens' GetAccountAuthorizationDetailsResponse [UserDetail]
gaadrsUserDetailList = lens _gaadrsUserDetailList (\ s a -> s{_gaadrsUserDetailList = a}) . _Default . _Coerce;

-- | When 'IsTruncated' is 'true', this element is present and contains the
-- value to use for the 'Marker' parameter in a subsequent pagination
-- request.
gaadrsMarker :: Lens' GetAccountAuthorizationDetailsResponse (Maybe Text)
gaadrsMarker = lens _gaadrsMarker (\ s a -> s{_gaadrsMarker = a});

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the 'Marker' request parameter to retrieve more items.
gaadrsIsTruncated :: Lens' GetAccountAuthorizationDetailsResponse (Maybe Bool)
gaadrsIsTruncated = lens _gaadrsIsTruncated (\ s a -> s{_gaadrsIsTruncated = a});

-- | A list containing information about managed policies.
gaadrsPolicies :: Lens' GetAccountAuthorizationDetailsResponse [ManagedPolicyDetail]
gaadrsPolicies = lens _gaadrsPolicies (\ s a -> s{_gaadrsPolicies = a}) . _Default . _Coerce;

-- | The response status code.
gaadrsStatus :: Lens' GetAccountAuthorizationDetailsResponse Int
gaadrsStatus = lens _gaadrsStatus (\ s a -> s{_gaadrsStatus = a});
