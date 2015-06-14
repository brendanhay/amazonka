{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.IAM.ListEntitiesForPolicy
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

-- | Lists all users, groups, and roles that the specified managed policy is
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
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListEntitiesForPolicy.html>
module Network.AWS.IAM.ListEntitiesForPolicy
    (
    -- * Request
      ListEntitiesForPolicy
    -- ** Request constructor
    , listEntitiesForPolicy
    -- ** Request lenses
    , lefpEntityFilter
    , lefpPolicyARN
    , lefpPathPrefix
    , lefpMaxItems
    , lefpMarker

    -- * Response
    , ListEntitiesForPolicyResponse
    -- ** Response constructor
    , listEntitiesForPolicyResponse
    -- ** Response lenses
    , lefprPolicyGroups
    , lefprPolicyRoles
    , lefprPolicyUsers
    , lefprIsTruncated
    , lefprMarker
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.IAM.Types

-- | /See:/ 'listEntitiesForPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lefpEntityFilter'
--
-- * 'lefpPolicyARN'
--
-- * 'lefpPathPrefix'
--
-- * 'lefpMaxItems'
--
-- * 'lefpMarker'
data ListEntitiesForPolicy = ListEntitiesForPolicy'{_lefpEntityFilter :: Maybe EntityType, _lefpPolicyARN :: Text, _lefpPathPrefix :: Text, _lefpMaxItems :: Nat, _lefpMarker :: Text} deriving (Eq, Read, Show)

-- | 'ListEntitiesForPolicy' smart constructor.
listEntitiesForPolicy :: Text -> Text -> Natural -> Text -> ListEntitiesForPolicy
listEntitiesForPolicy pPolicyARN pPathPrefix pMaxItems pMarker = ListEntitiesForPolicy'{_lefpEntityFilter = Nothing, _lefpPolicyARN = pPolicyARN, _lefpPathPrefix = pPathPrefix, _lefpMaxItems = _Nat # pMaxItems, _lefpMarker = pMarker};

-- | The entity type to use for filtering the results.
--
-- For example, when @EntityFilter@ is @Role@, only the roles that are
-- attached to the specified policy are returned. This parameter is
-- optional. If it is not included, all attached entities (users, groups,
-- and roles) are returned.
lefpEntityFilter :: Lens' ListEntitiesForPolicy (Maybe EntityType)
lefpEntityFilter = lens _lefpEntityFilter (\ s a -> s{_lefpEntityFilter = a});

-- | FIXME: Undocumented member.
lefpPolicyARN :: Lens' ListEntitiesForPolicy Text
lefpPolicyARN = lens _lefpPolicyARN (\ s a -> s{_lefpPolicyARN = a});

-- | The path prefix for filtering the results. This parameter is optional.
-- If it is not included, it defaults to a slash (\/), listing all
-- entities.
lefpPathPrefix :: Lens' ListEntitiesForPolicy Text
lefpPathPrefix = lens _lefpPathPrefix (\ s a -> s{_lefpPathPrefix = a});

-- | Use this only when paginating results to indicate the maximum number of
-- entities you want in the response. If there are additional entities
-- beyond the maximum you specify, the @IsTruncated@ response element is
-- @true@. This parameter is optional. If you do not include it, it
-- defaults to 100.
lefpMaxItems :: Lens' ListEntitiesForPolicy Natural
lefpMaxItems = lens _lefpMaxItems (\ s a -> s{_lefpMaxItems = a}) . _Nat;

-- | Use this only when paginating results, and only in a subsequent request
-- after you\'ve received a response where the results are truncated. Set
-- it to the value of the @Marker@ element in the response you just
-- received.
lefpMarker :: Lens' ListEntitiesForPolicy Text
lefpMarker = lens _lefpMarker (\ s a -> s{_lefpMarker = a});

instance AWSRequest ListEntitiesForPolicy where
        type Sv ListEntitiesForPolicy = IAM
        type Rs ListEntitiesForPolicy =
             ListEntitiesForPolicyResponse
        request = post
        response
          = receiveXMLWrapper "ListEntitiesForPolicyResult"
              (\ s h x ->
                 ListEntitiesForPolicyResponse' <$>
                   (x .@? "PolicyGroups" .!@ mempty >>=
                      parseXMLList "member")
                     <*>
                     (x .@? "PolicyRoles" .!@ mempty >>=
                        parseXMLList "member")
                     <*>
                     (x .@? "PolicyUsers" .!@ mempty >>=
                        parseXMLList "member")
                     <*> x .@? "IsTruncated"
                     <*> x .@ "Marker")

instance ToHeaders ListEntitiesForPolicy where
        toHeaders = const mempty

instance ToPath ListEntitiesForPolicy where
        toPath = const "/"

instance ToQuery ListEntitiesForPolicy where
        toQuery ListEntitiesForPolicy'{..}
          = mconcat
              ["Action" =: ("ListEntitiesForPolicy" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "EntityFilter" =: _lefpEntityFilter,
               "PolicyArn" =: _lefpPolicyARN,
               "PathPrefix" =: _lefpPathPrefix,
               "MaxItems" =: _lefpMaxItems, "Marker" =: _lefpMarker]

-- | /See:/ 'listEntitiesForPolicyResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lefprPolicyGroups'
--
-- * 'lefprPolicyRoles'
--
-- * 'lefprPolicyUsers'
--
-- * 'lefprIsTruncated'
--
-- * 'lefprMarker'
data ListEntitiesForPolicyResponse = ListEntitiesForPolicyResponse'{_lefprPolicyGroups :: [PolicyGroup], _lefprPolicyRoles :: [PolicyRole], _lefprPolicyUsers :: [PolicyUser], _lefprIsTruncated :: Maybe Bool, _lefprMarker :: Text} deriving (Eq, Read, Show)

-- | 'ListEntitiesForPolicyResponse' smart constructor.
listEntitiesForPolicyResponse :: Text -> ListEntitiesForPolicyResponse
listEntitiesForPolicyResponse pMarker = ListEntitiesForPolicyResponse'{_lefprPolicyGroups = mempty, _lefprPolicyRoles = mempty, _lefprPolicyUsers = mempty, _lefprIsTruncated = Nothing, _lefprMarker = pMarker};

-- | A list of groups that the policy is attached to.
lefprPolicyGroups :: Lens' ListEntitiesForPolicyResponse [PolicyGroup]
lefprPolicyGroups = lens _lefprPolicyGroups (\ s a -> s{_lefprPolicyGroups = a});

-- | A list of roles that the policy is attached to.
lefprPolicyRoles :: Lens' ListEntitiesForPolicyResponse [PolicyRole]
lefprPolicyRoles = lens _lefprPolicyRoles (\ s a -> s{_lefprPolicyRoles = a});

-- | A list of users that the policy is attached to.
lefprPolicyUsers :: Lens' ListEntitiesForPolicyResponse [PolicyUser]
lefprPolicyUsers = lens _lefprPolicyUsers (\ s a -> s{_lefprPolicyUsers = a});

-- | A flag that indicates whether there are more entities to list. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more entities in the
-- list.
lefprIsTruncated :: Lens' ListEntitiesForPolicyResponse (Maybe Bool)
lefprIsTruncated = lens _lefprIsTruncated (\ s a -> s{_lefprIsTruncated = a});

-- | If @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
lefprMarker :: Lens' ListEntitiesForPolicyResponse Text
lefprMarker = lens _lefprMarker (\ s a -> s{_lefprMarker = a});
