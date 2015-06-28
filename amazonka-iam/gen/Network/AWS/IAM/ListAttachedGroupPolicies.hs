{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.IAM.ListAttachedGroupPolicies
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

-- | Lists all managed policies that are attached to the specified group.
--
-- A group can also have inline policies embedded with it. To list the
-- inline policies for a group, use the ListGroupPolicies API. For
-- information about policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- You can paginate the results using the @MaxItems@ and @Marker@
-- parameters. You can use the @PathPrefix@ parameter to limit the list of
-- policies to only those matching the specified path prefix. If there are
-- no policies attached to the specified group (or none that match the
-- specified path prefix), the action returns an empty list.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListAttachedGroupPolicies.html>
module Network.AWS.IAM.ListAttachedGroupPolicies
    (
    -- * Request
      ListAttachedGroupPolicies
    -- ** Request constructor
    , listAttachedGroupPolicies
    -- ** Request lenses
    , lagpPathPrefix
    , lagpMaxItems
    , lagpMarker
    , lagpGroupName

    -- * Response
    , ListAttachedGroupPoliciesResponse
    -- ** Response constructor
    , listAttachedGroupPoliciesResponse
    -- ** Response lenses
    , lagprAttachedPolicies
    , lagprMarker
    , lagprIsTruncated
    , lagprStatus
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listAttachedGroupPolicies' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lagpPathPrefix'
--
-- * 'lagpMaxItems'
--
-- * 'lagpMarker'
--
-- * 'lagpGroupName'
data ListAttachedGroupPolicies = ListAttachedGroupPolicies'
    { _lagpPathPrefix :: !(Maybe Text)
    , _lagpMaxItems   :: !(Maybe Nat)
    , _lagpMarker     :: !(Maybe Text)
    , _lagpGroupName  :: !Text
    } deriving (Eq,Read,Show)

-- | 'ListAttachedGroupPolicies' smart constructor.
listAttachedGroupPolicies :: Text -> ListAttachedGroupPolicies
listAttachedGroupPolicies pGroupName =
    ListAttachedGroupPolicies'
    { _lagpPathPrefix = Nothing
    , _lagpMaxItems = Nothing
    , _lagpMarker = Nothing
    , _lagpGroupName = pGroupName
    }

-- | The path prefix for filtering the results. This parameter is optional.
-- If it is not included, it defaults to a slash (\/), listing all
-- policies.
lagpPathPrefix :: Lens' ListAttachedGroupPolicies (Maybe Text)
lagpPathPrefix = lens _lagpPathPrefix (\ s a -> s{_lagpPathPrefix = a});

-- | Use this only when paginating results to indicate the maximum number of
-- policies you want in the response. If there are additional policies
-- beyond the maximum you specify, the @IsTruncated@ response element is
-- @true@. This parameter is optional. If you do not include it, it
-- defaults to 100.
lagpMaxItems :: Lens' ListAttachedGroupPolicies (Maybe Natural)
lagpMaxItems = lens _lagpMaxItems (\ s a -> s{_lagpMaxItems = a}) . mapping _Nat;

-- | Use this only when paginating results, and only in a subsequent request
-- after you\'ve received a response where the results are truncated. Set
-- it to the value of the @Marker@ element in the response you just
-- received.
lagpMarker :: Lens' ListAttachedGroupPolicies (Maybe Text)
lagpMarker = lens _lagpMarker (\ s a -> s{_lagpMarker = a});

-- | The name (friendly name, not ARN) of the group to list attached policies
-- for.
lagpGroupName :: Lens' ListAttachedGroupPolicies Text
lagpGroupName = lens _lagpGroupName (\ s a -> s{_lagpGroupName = a});

instance AWSRequest ListAttachedGroupPolicies where
        type Sv ListAttachedGroupPolicies = IAM
        type Rs ListAttachedGroupPolicies =
             ListAttachedGroupPoliciesResponse
        request = post
        response
          = receiveXMLWrapper "ListAttachedGroupPoliciesResult"
              (\ s h x ->
                 ListAttachedGroupPoliciesResponse' <$>
                   (x .@? "AttachedPolicies" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "Marker")
                     <*> (x .@? "IsTruncated")
                     <*> (pure s))

instance ToHeaders ListAttachedGroupPolicies where
        toHeaders = const mempty

instance ToPath ListAttachedGroupPolicies where
        toPath = const "/"

instance ToQuery ListAttachedGroupPolicies where
        toQuery ListAttachedGroupPolicies'{..}
          = mconcat
              ["Action" =:
                 ("ListAttachedGroupPolicies" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "PathPrefix" =: _lagpPathPrefix,
               "MaxItems" =: _lagpMaxItems, "Marker" =: _lagpMarker,
               "GroupName" =: _lagpGroupName]

-- | Contains the response to a successful ListAttachedGroupPolicies request.
--
-- /See:/ 'listAttachedGroupPoliciesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lagprAttachedPolicies'
--
-- * 'lagprMarker'
--
-- * 'lagprIsTruncated'
--
-- * 'lagprStatus'
data ListAttachedGroupPoliciesResponse = ListAttachedGroupPoliciesResponse'
    { _lagprAttachedPolicies :: !(Maybe [AttachedPolicy])
    , _lagprMarker           :: !(Maybe Text)
    , _lagprIsTruncated      :: !(Maybe Bool)
    , _lagprStatus           :: !Status
    } deriving (Eq,Show)

-- | 'ListAttachedGroupPoliciesResponse' smart constructor.
listAttachedGroupPoliciesResponse :: Status -> ListAttachedGroupPoliciesResponse
listAttachedGroupPoliciesResponse pStatus =
    ListAttachedGroupPoliciesResponse'
    { _lagprAttachedPolicies = Nothing
    , _lagprMarker = Nothing
    , _lagprIsTruncated = Nothing
    , _lagprStatus = pStatus
    }

-- | A list of the attached policies.
lagprAttachedPolicies :: Lens' ListAttachedGroupPoliciesResponse [AttachedPolicy]
lagprAttachedPolicies = lens _lagprAttachedPolicies (\ s a -> s{_lagprAttachedPolicies = a}) . _Default;

-- | If @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
lagprMarker :: Lens' ListAttachedGroupPoliciesResponse (Maybe Text)
lagprMarker = lens _lagprMarker (\ s a -> s{_lagprMarker = a});

-- | A flag that indicates whether there are more policies to list. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more policies in the
-- list.
lagprIsTruncated :: Lens' ListAttachedGroupPoliciesResponse (Maybe Bool)
lagprIsTruncated = lens _lagprIsTruncated (\ s a -> s{_lagprIsTruncated = a});

-- | FIXME: Undocumented member.
lagprStatus :: Lens' ListAttachedGroupPoliciesResponse Status
lagprStatus = lens _lagprStatus (\ s a -> s{_lagprStatus = a});
