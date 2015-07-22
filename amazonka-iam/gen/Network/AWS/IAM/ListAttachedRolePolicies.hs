{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListAttachedRolePolicies
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Lists all managed policies that are attached to the specified role.
--
-- A role can also have inline policies embedded with it. To list the
-- inline policies for a role, use the ListRolePolicies API. For
-- information about policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- You can paginate the results using the @MaxItems@ and @Marker@
-- parameters. You can use the @PathPrefix@ parameter to limit the list of
-- policies to only those matching the specified path prefix. If there are
-- no policies attached to the specified role (or none that match the
-- specified path prefix), the action returns an empty list.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListAttachedRolePolicies.html>
module Network.AWS.IAM.ListAttachedRolePolicies
    (
    -- * Request
      ListAttachedRolePolicies
    -- ** Request constructor
    , listAttachedRolePolicies
    -- ** Request lenses
    , larprqPathPrefix
    , larprqMaxItems
    , larprqMarker
    , larprqRoleName

    -- * Response
    , ListAttachedRolePoliciesResponse
    -- ** Response constructor
    , listAttachedRolePoliciesResponse
    -- ** Response lenses
    , larprsAttachedPolicies
    , larprsMarker
    , larprsIsTruncated
    , larprsStatus
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listAttachedRolePolicies' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'larprqPathPrefix'
--
-- * 'larprqMaxItems'
--
-- * 'larprqMarker'
--
-- * 'larprqRoleName'
data ListAttachedRolePolicies = ListAttachedRolePolicies'
    { _larprqPathPrefix :: !(Maybe Text)
    , _larprqMaxItems   :: !(Maybe Nat)
    , _larprqMarker     :: !(Maybe Text)
    , _larprqRoleName   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListAttachedRolePolicies' smart constructor.
listAttachedRolePolicies :: Text -> ListAttachedRolePolicies
listAttachedRolePolicies pRoleName =
    ListAttachedRolePolicies'
    { _larprqPathPrefix = Nothing
    , _larprqMaxItems = Nothing
    , _larprqMarker = Nothing
    , _larprqRoleName = pRoleName
    }

-- | The path prefix for filtering the results. This parameter is optional.
-- If it is not included, it defaults to a slash (\/), listing all
-- policies.
larprqPathPrefix :: Lens' ListAttachedRolePolicies (Maybe Text)
larprqPathPrefix = lens _larprqPathPrefix (\ s a -> s{_larprqPathPrefix = a});

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If there are additional items beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- This parameter is optional. If you do not include it, it defaults to
-- 100.
larprqMaxItems :: Lens' ListAttachedRolePolicies (Maybe Natural)
larprqMaxItems = lens _larprqMaxItems (\ s a -> s{_larprqMaxItems = a}) . mapping _Nat;

-- | Use this parameter only when paginating results and only after you have
-- received a response where the results are truncated. Set it to the value
-- of the @Marker@ element in the response you just received.
larprqMarker :: Lens' ListAttachedRolePolicies (Maybe Text)
larprqMarker = lens _larprqMarker (\ s a -> s{_larprqMarker = a});

-- | The name (friendly name, not ARN) of the role to list attached policies
-- for.
larprqRoleName :: Lens' ListAttachedRolePolicies Text
larprqRoleName = lens _larprqRoleName (\ s a -> s{_larprqRoleName = a});

instance AWSRequest ListAttachedRolePolicies where
        type Sv ListAttachedRolePolicies = IAM
        type Rs ListAttachedRolePolicies =
             ListAttachedRolePoliciesResponse
        request = post
        response
          = receiveXMLWrapper "ListAttachedRolePoliciesResult"
              (\ s h x ->
                 ListAttachedRolePoliciesResponse' <$>
                   (x .@? "AttachedPolicies" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "Marker")
                     <*> (x .@? "IsTruncated")
                     <*> (pure (fromEnum s)))

instance ToHeaders ListAttachedRolePolicies where
        toHeaders = const mempty

instance ToPath ListAttachedRolePolicies where
        toPath = const "/"

instance ToQuery ListAttachedRolePolicies where
        toQuery ListAttachedRolePolicies'{..}
          = mconcat
              ["Action" =:
                 ("ListAttachedRolePolicies" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "PathPrefix" =: _larprqPathPrefix,
               "MaxItems" =: _larprqMaxItems,
               "Marker" =: _larprqMarker,
               "RoleName" =: _larprqRoleName]

-- | Contains the response to a successful ListAttachedRolePolicies request.
--
-- /See:/ 'listAttachedRolePoliciesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'larprsAttachedPolicies'
--
-- * 'larprsMarker'
--
-- * 'larprsIsTruncated'
--
-- * 'larprsStatus'
data ListAttachedRolePoliciesResponse = ListAttachedRolePoliciesResponse'
    { _larprsAttachedPolicies :: !(Maybe [AttachedPolicy])
    , _larprsMarker           :: !(Maybe Text)
    , _larprsIsTruncated      :: !(Maybe Bool)
    , _larprsStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListAttachedRolePoliciesResponse' smart constructor.
listAttachedRolePoliciesResponse :: Int -> ListAttachedRolePoliciesResponse
listAttachedRolePoliciesResponse pStatus =
    ListAttachedRolePoliciesResponse'
    { _larprsAttachedPolicies = Nothing
    , _larprsMarker = Nothing
    , _larprsIsTruncated = Nothing
    , _larprsStatus = pStatus
    }

-- | A list of the attached policies.
larprsAttachedPolicies :: Lens' ListAttachedRolePoliciesResponse [AttachedPolicy]
larprsAttachedPolicies = lens _larprsAttachedPolicies (\ s a -> s{_larprsAttachedPolicies = a}) . _Default;

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
larprsMarker :: Lens' ListAttachedRolePoliciesResponse (Maybe Text)
larprsMarker = lens _larprsMarker (\ s a -> s{_larprsMarker = a});

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items.
larprsIsTruncated :: Lens' ListAttachedRolePoliciesResponse (Maybe Bool)
larprsIsTruncated = lens _larprsIsTruncated (\ s a -> s{_larprsIsTruncated = a});

-- | FIXME: Undocumented member.
larprsStatus :: Lens' ListAttachedRolePoliciesResponse Int
larprsStatus = lens _larprsStatus (\ s a -> s{_larprsStatus = a});
