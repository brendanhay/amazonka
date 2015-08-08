{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

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
-- You can use the optional @EntityFilter@ parameter to limit the results
-- to a particular type of entity (users, groups, or roles). For example,
-- to list only the roles that are attached to the specified policy, set
-- @EntityFilter@ to @Role@.
--
-- You can paginate the results using the @MaxItems@ and @Marker@
-- parameters.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListEntitiesForPolicy.html AWS API Reference> for ListEntitiesForPolicy.
module Network.AWS.IAM.ListEntitiesForPolicy
    (
    -- * Creating a Request
      ListEntitiesForPolicy
    , listEntitiesForPolicy
    -- * Request Lenses
    , lefpPathPrefix
    , lefpEntityFilter
    , lefpMaxItems
    , lefpMarker
    , lefpPolicyARN

    -- * Destructuring the Response
    , ListEntitiesForPolicyResponse
    , listEntitiesForPolicyResponse
    -- * Response Lenses
    , lefprsPolicyGroups
    , lefprsPolicyRoles
    , lefprsPolicyUsers
    , lefprsMarker
    , lefprsIsTruncated
    , lefprsStatus
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listEntitiesForPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lefpPathPrefix'
--
-- * 'lefpEntityFilter'
--
-- * 'lefpMaxItems'
--
-- * 'lefpMarker'
--
-- * 'lefpPolicyARN'
data ListEntitiesForPolicy = ListEntitiesForPolicy'
    { _lefpPathPrefix   :: !(Maybe Text)
    , _lefpEntityFilter :: !(Maybe EntityType)
    , _lefpMaxItems     :: !(Maybe Nat)
    , _lefpMarker       :: !(Maybe Text)
    , _lefpPolicyARN    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListEntitiesForPolicy' smart constructor.
listEntitiesForPolicy :: Text -> ListEntitiesForPolicy
listEntitiesForPolicy pPolicyARN_ =
    ListEntitiesForPolicy'
    { _lefpPathPrefix = Nothing
    , _lefpEntityFilter = Nothing
    , _lefpMaxItems = Nothing
    , _lefpMarker = Nothing
    , _lefpPolicyARN = pPolicyARN_
    }

-- | The path prefix for filtering the results. This parameter is optional.
-- If it is not included, it defaults to a slash (\/), listing all
-- entities.
lefpPathPrefix :: Lens' ListEntitiesForPolicy (Maybe Text)
lefpPathPrefix = lens _lefpPathPrefix (\ s a -> s{_lefpPathPrefix = a});

-- | The entity type to use for filtering the results.
--
-- For example, when @EntityFilter@ is @Role@, only the roles that are
-- attached to the specified policy are returned. This parameter is
-- optional. If it is not included, all attached entities (users, groups,
-- and roles) are returned.
lefpEntityFilter :: Lens' ListEntitiesForPolicy (Maybe EntityType)
lefpEntityFilter = lens _lefpEntityFilter (\ s a -> s{_lefpEntityFilter = a});

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If there are additional items beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- This parameter is optional. If you do not include it, it defaults to
-- 100.
lefpMaxItems :: Lens' ListEntitiesForPolicy (Maybe Natural)
lefpMaxItems = lens _lefpMaxItems (\ s a -> s{_lefpMaxItems = a}) . mapping _Nat;

-- | Use this parameter only when paginating results and only after you have
-- received a response where the results are truncated. Set it to the value
-- of the @Marker@ element in the response you just received.
lefpMarker :: Lens' ListEntitiesForPolicy (Maybe Text)
lefpMarker = lens _lefpMarker (\ s a -> s{_lefpMarker = a});

-- | Undocumented member.
lefpPolicyARN :: Lens' ListEntitiesForPolicy Text
lefpPolicyARN = lens _lefpPolicyARN (\ s a -> s{_lefpPolicyARN = a});

instance AWSRequest ListEntitiesForPolicy where
        type Sv ListEntitiesForPolicy = IAM
        type Rs ListEntitiesForPolicy =
             ListEntitiesForPolicyResponse
        request = postQuery
        response
          = receiveXMLWrapper "ListEntitiesForPolicyResult"
              (\ s h x ->
                 ListEntitiesForPolicyResponse' <$>
                   (x .@? "PolicyGroups" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*>
                     (x .@? "PolicyRoles" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*>
                     (x .@? "PolicyUsers" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (x .@? "Marker")
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
               "MaxItems" =: _lefpMaxItems, "Marker" =: _lefpMarker,
               "PolicyArn" =: _lefpPolicyARN]

-- | Contains the response to a successful ListEntitiesForPolicy request.
--
-- /See:/ 'listEntitiesForPolicyResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lefprsPolicyGroups'
--
-- * 'lefprsPolicyRoles'
--
-- * 'lefprsPolicyUsers'
--
-- * 'lefprsMarker'
--
-- * 'lefprsIsTruncated'
--
-- * 'lefprsStatus'
data ListEntitiesForPolicyResponse = ListEntitiesForPolicyResponse'
    { _lefprsPolicyGroups :: !(Maybe [PolicyGroup])
    , _lefprsPolicyRoles  :: !(Maybe [PolicyRole])
    , _lefprsPolicyUsers  :: !(Maybe [PolicyUser])
    , _lefprsMarker       :: !(Maybe Text)
    , _lefprsIsTruncated  :: !(Maybe Bool)
    , _lefprsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListEntitiesForPolicyResponse' smart constructor.
listEntitiesForPolicyResponse :: Int -> ListEntitiesForPolicyResponse
listEntitiesForPolicyResponse pStatus_ =
    ListEntitiesForPolicyResponse'
    { _lefprsPolicyGroups = Nothing
    , _lefprsPolicyRoles = Nothing
    , _lefprsPolicyUsers = Nothing
    , _lefprsMarker = Nothing
    , _lefprsIsTruncated = Nothing
    , _lefprsStatus = pStatus_
    }

-- | A list of groups that the policy is attached to.
lefprsPolicyGroups :: Lens' ListEntitiesForPolicyResponse [PolicyGroup]
lefprsPolicyGroups = lens _lefprsPolicyGroups (\ s a -> s{_lefprsPolicyGroups = a}) . _Default . _Coerce;

-- | A list of roles that the policy is attached to.
lefprsPolicyRoles :: Lens' ListEntitiesForPolicyResponse [PolicyRole]
lefprsPolicyRoles = lens _lefprsPolicyRoles (\ s a -> s{_lefprsPolicyRoles = a}) . _Default . _Coerce;

-- | A list of users that the policy is attached to.
lefprsPolicyUsers :: Lens' ListEntitiesForPolicyResponse [PolicyUser]
lefprsPolicyUsers = lens _lefprsPolicyUsers (\ s a -> s{_lefprsPolicyUsers = a}) . _Default . _Coerce;

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
lefprsMarker :: Lens' ListEntitiesForPolicyResponse (Maybe Text)
lefprsMarker = lens _lefprsMarker (\ s a -> s{_lefprsMarker = a});

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items.
lefprsIsTruncated :: Lens' ListEntitiesForPolicyResponse (Maybe Bool)
lefprsIsTruncated = lens _lefprsIsTruncated (\ s a -> s{_lefprsIsTruncated = a});

-- | Undocumented member.
lefprsStatus :: Lens' ListEntitiesForPolicyResponse Int
lefprsStatus = lens _lefprsStatus (\ s a -> s{_lefprsStatus = a});
