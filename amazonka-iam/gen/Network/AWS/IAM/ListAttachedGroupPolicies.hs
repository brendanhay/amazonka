{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListAttachedGroupPolicies
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Lists all managed policies that are attached to the specified group.
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
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListAttachedGroupPolicies.html AWS API Reference> for ListAttachedGroupPolicies.
module Network.AWS.IAM.ListAttachedGroupPolicies
    (
    -- * Creating a Request
      ListAttachedGroupPolicies
    , listAttachedGroupPolicies
    -- * Request Lenses
    , lagpPathPrefix
    , lagpMaxItems
    , lagpMarker
    , lagpGroupName

    -- * Destructuring the Response
    , ListAttachedGroupPoliciesResponse
    , listAttachedGroupPoliciesResponse
    -- * Response Lenses
    , lagprsAttachedPolicies
    , lagprsMarker
    , lagprsIsTruncated
    , lagprsStatus
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListAttachedGroupPolicies' smart constructor.
listAttachedGroupPolicies :: Text -> ListAttachedGroupPolicies
listAttachedGroupPolicies pGroupName_ =
    ListAttachedGroupPolicies'
    { _lagpPathPrefix = Nothing
    , _lagpMaxItems = Nothing
    , _lagpMarker = Nothing
    , _lagpGroupName = pGroupName_
    }

-- | The path prefix for filtering the results. This parameter is optional.
-- If it is not included, it defaults to a slash (\/), listing all
-- policies.
lagpPathPrefix :: Lens' ListAttachedGroupPolicies (Maybe Text)
lagpPathPrefix = lens _lagpPathPrefix (\ s a -> s{_lagpPathPrefix = a});

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If there are additional items beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- This parameter is optional. If you do not include it, it defaults to
-- 100.
lagpMaxItems :: Lens' ListAttachedGroupPolicies (Maybe Natural)
lagpMaxItems = lens _lagpMaxItems (\ s a -> s{_lagpMaxItems = a}) . mapping _Nat;

-- | Use this parameter only when paginating results and only after you have
-- received a response where the results are truncated. Set it to the value
-- of the @Marker@ element in the response you just received.
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
        request = postQuery
        response
          = receiveXMLWrapper "ListAttachedGroupPoliciesResult"
              (\ s h x ->
                 ListAttachedGroupPoliciesResponse' <$>
                   (x .@? "AttachedPolicies" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "Marker")
                     <*> (x .@? "IsTruncated")
                     <*> (pure (fromEnum s)))

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
-- * 'lagprsAttachedPolicies'
--
-- * 'lagprsMarker'
--
-- * 'lagprsIsTruncated'
--
-- * 'lagprsStatus'
data ListAttachedGroupPoliciesResponse = ListAttachedGroupPoliciesResponse'
    { _lagprsAttachedPolicies :: !(Maybe [AttachedPolicy])
    , _lagprsMarker           :: !(Maybe Text)
    , _lagprsIsTruncated      :: !(Maybe Bool)
    , _lagprsStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListAttachedGroupPoliciesResponse' smart constructor.
listAttachedGroupPoliciesResponse :: Int -> ListAttachedGroupPoliciesResponse
listAttachedGroupPoliciesResponse pStatus_ =
    ListAttachedGroupPoliciesResponse'
    { _lagprsAttachedPolicies = Nothing
    , _lagprsMarker = Nothing
    , _lagprsIsTruncated = Nothing
    , _lagprsStatus = pStatus_
    }

-- | A list of the attached policies.
lagprsAttachedPolicies :: Lens' ListAttachedGroupPoliciesResponse [AttachedPolicy]
lagprsAttachedPolicies = lens _lagprsAttachedPolicies (\ s a -> s{_lagprsAttachedPolicies = a}) . _Default . _Coerce;

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
lagprsMarker :: Lens' ListAttachedGroupPoliciesResponse (Maybe Text)
lagprsMarker = lens _lagprsMarker (\ s a -> s{_lagprsMarker = a});

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items.
lagprsIsTruncated :: Lens' ListAttachedGroupPoliciesResponse (Maybe Bool)
lagprsIsTruncated = lens _lagprsIsTruncated (\ s a -> s{_lagprsIsTruncated = a});

-- | Undocumented member.
lagprsStatus :: Lens' ListAttachedGroupPoliciesResponse Int
lagprsStatus = lens _lagprsStatus (\ s a -> s{_lagprsStatus = a});
