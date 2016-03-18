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
-- Module      : Network.AWS.IAM.ListEntitiesForPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all users, groups, and roles that the specified managed policy is
-- attached to.
--
-- You can use the optional 'EntityFilter' parameter to limit the results
-- to a particular type of entity (users, groups, or roles). For example,
-- to list only the roles that are attached to the specified policy, set
-- 'EntityFilter' to 'Role'.
--
-- You can paginate the results using the 'MaxItems' and 'Marker'
-- parameters.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListEntitiesForPolicy
    (
    -- * Creating a Request
      listEntitiesForPolicy
    , ListEntitiesForPolicy
    -- * Request Lenses
    , lefpPathPrefix
    , lefpEntityFilter
    , lefpMarker
    , lefpMaxItems
    , lefpPolicyARN

    -- * Destructuring the Response
    , listEntitiesForPolicyResponse
    , ListEntitiesForPolicyResponse
    -- * Response Lenses
    , lefprsPolicyGroups
    , lefprsPolicyRoles
    , lefprsMarker
    , lefprsPolicyUsers
    , lefprsIsTruncated
    , lefprsResponseStatus
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listEntitiesForPolicy' smart constructor.
data ListEntitiesForPolicy = ListEntitiesForPolicy'
    { _lefpPathPrefix   :: !(Maybe Text)
    , _lefpEntityFilter :: !(Maybe EntityType)
    , _lefpMarker       :: !(Maybe Text)
    , _lefpMaxItems     :: !(Maybe Nat)
    , _lefpPolicyARN    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListEntitiesForPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lefpPathPrefix'
--
-- * 'lefpEntityFilter'
--
-- * 'lefpMarker'
--
-- * 'lefpMaxItems'
--
-- * 'lefpPolicyARN'
listEntitiesForPolicy
    :: Text -- ^ 'lefpPolicyARN'
    -> ListEntitiesForPolicy
listEntitiesForPolicy pPolicyARN_ =
    ListEntitiesForPolicy'
    { _lefpPathPrefix = Nothing
    , _lefpEntityFilter = Nothing
    , _lefpMarker = Nothing
    , _lefpMaxItems = Nothing
    , _lefpPolicyARN = pPolicyARN_
    }

-- | The path prefix for filtering the results. This parameter is optional.
-- If it is not included, it defaults to a slash (\/), listing all
-- entities.
lefpPathPrefix :: Lens' ListEntitiesForPolicy (Maybe Text)
lefpPathPrefix = lens _lefpPathPrefix (\ s a -> s{_lefpPathPrefix = a});

-- | The entity type to use for filtering the results.
--
-- For example, when 'EntityFilter' is 'Role', only the roles that are
-- attached to the specified policy are returned. This parameter is
-- optional. If it is not included, all attached entities (users, groups,
-- and roles) are returned.
lefpEntityFilter :: Lens' ListEntitiesForPolicy (Maybe EntityType)
lefpEntityFilter = lens _lefpEntityFilter (\ s a -> s{_lefpEntityFilter = a});

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the 'Marker' element in the response that you received to
-- indicate where the next call should start.
lefpMarker :: Lens' ListEntitiesForPolicy (Maybe Text)
lefpMarker = lens _lefpMarker (\ s a -> s{_lefpMarker = a});

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the 'IsTruncated' response element is 'true'.
--
-- This parameter is optional. If you do not include it, it defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the 'IsTruncated' response element
-- returns 'true' and 'Marker' contains a value to include in the
-- subsequent call that tells the service where to continue from.
lefpMaxItems :: Lens' ListEntitiesForPolicy (Maybe Natural)
lefpMaxItems = lens _lefpMaxItems (\ s a -> s{_lefpMaxItems = a}) . mapping _Nat;

-- | Undocumented member.
lefpPolicyARN :: Lens' ListEntitiesForPolicy Text
lefpPolicyARN = lens _lefpPolicyARN (\ s a -> s{_lefpPolicyARN = a});

instance AWSPager ListEntitiesForPolicy where
        page rq rs
          | stop (rs ^. lefprsIsTruncated) = Nothing
          | isNothing (rs ^. lefprsMarker) = Nothing
          | otherwise =
            Just $ rq & lefpMarker .~ rs ^. lefprsMarker

instance AWSRequest ListEntitiesForPolicy where
        type Rs ListEntitiesForPolicy =
             ListEntitiesForPolicyResponse
        request = postQuery iAM
        response
          = receiveXMLWrapper "ListEntitiesForPolicyResult"
              (\ s h x ->
                 ListEntitiesForPolicyResponse' <$>
                   (x .@? "PolicyGroups" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*>
                     (x .@? "PolicyRoles" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (x .@? "Marker")
                     <*>
                     (x .@? "PolicyUsers" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (x .@? "IsTruncated")
                     <*> (pure (fromEnum s)))

instance ToHeaders ListEntitiesForPolicy where
        toHeaders = const mempty

instance ToPath ListEntitiesForPolicy where
        toPath = const "/"

instance ToQuery ListEntitiesForPolicy where
        toQuery ListEntitiesForPolicy'{..}
          = mconcat
              ["Action" =: ("ListEntitiesForPolicy" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "PathPrefix" =: _lefpPathPrefix,
               "EntityFilter" =: _lefpEntityFilter,
               "Marker" =: _lefpMarker, "MaxItems" =: _lefpMaxItems,
               "PolicyArn" =: _lefpPolicyARN]

-- | Contains the response to a successful < ListEntitiesForPolicy> request.
--
-- /See:/ 'listEntitiesForPolicyResponse' smart constructor.
data ListEntitiesForPolicyResponse = ListEntitiesForPolicyResponse'
    { _lefprsPolicyGroups   :: !(Maybe [PolicyGroup])
    , _lefprsPolicyRoles    :: !(Maybe [PolicyRole])
    , _lefprsMarker         :: !(Maybe Text)
    , _lefprsPolicyUsers    :: !(Maybe [PolicyUser])
    , _lefprsIsTruncated    :: !(Maybe Bool)
    , _lefprsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListEntitiesForPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lefprsPolicyGroups'
--
-- * 'lefprsPolicyRoles'
--
-- * 'lefprsMarker'
--
-- * 'lefprsPolicyUsers'
--
-- * 'lefprsIsTruncated'
--
-- * 'lefprsResponseStatus'
listEntitiesForPolicyResponse
    :: Int -- ^ 'lefprsResponseStatus'
    -> ListEntitiesForPolicyResponse
listEntitiesForPolicyResponse pResponseStatus_ =
    ListEntitiesForPolicyResponse'
    { _lefprsPolicyGroups = Nothing
    , _lefprsPolicyRoles = Nothing
    , _lefprsMarker = Nothing
    , _lefprsPolicyUsers = Nothing
    , _lefprsIsTruncated = Nothing
    , _lefprsResponseStatus = pResponseStatus_
    }

-- | A list of groups that the policy is attached to.
lefprsPolicyGroups :: Lens' ListEntitiesForPolicyResponse [PolicyGroup]
lefprsPolicyGroups = lens _lefprsPolicyGroups (\ s a -> s{_lefprsPolicyGroups = a}) . _Default . _Coerce;

-- | A list of roles that the policy is attached to.
lefprsPolicyRoles :: Lens' ListEntitiesForPolicyResponse [PolicyRole]
lefprsPolicyRoles = lens _lefprsPolicyRoles (\ s a -> s{_lefprsPolicyRoles = a}) . _Default . _Coerce;

-- | When 'IsTruncated' is 'true', this element is present and contains the
-- value to use for the 'Marker' parameter in a subsequent pagination
-- request.
lefprsMarker :: Lens' ListEntitiesForPolicyResponse (Maybe Text)
lefprsMarker = lens _lefprsMarker (\ s a -> s{_lefprsMarker = a});

-- | A list of users that the policy is attached to.
lefprsPolicyUsers :: Lens' ListEntitiesForPolicyResponse [PolicyUser]
lefprsPolicyUsers = lens _lefprsPolicyUsers (\ s a -> s{_lefprsPolicyUsers = a}) . _Default . _Coerce;

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the 'Marker' request parameter to retrieve more items. Note that
-- IAM might return fewer than the 'MaxItems' number of results even when
-- there are more results available. We recommend that you check
-- 'IsTruncated' after every call to ensure that you receive all of your
-- results.
lefprsIsTruncated :: Lens' ListEntitiesForPolicyResponse (Maybe Bool)
lefprsIsTruncated = lens _lefprsIsTruncated (\ s a -> s{_lefprsIsTruncated = a});

-- | The response status code.
lefprsResponseStatus :: Lens' ListEntitiesForPolicyResponse Int
lefprsResponseStatus = lens _lefprsResponseStatus (\ s a -> s{_lefprsResponseStatus = a});
