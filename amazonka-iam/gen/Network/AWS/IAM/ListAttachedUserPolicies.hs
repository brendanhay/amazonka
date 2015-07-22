{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListAttachedUserPolicies
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Lists all managed policies that are attached to the specified user.
--
-- A user can also have inline policies embedded with it. To list the
-- inline policies for a user, use the ListUserPolicies API. For
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
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListAttachedUserPolicies.html>
module Network.AWS.IAM.ListAttachedUserPolicies
    (
    -- * Request
      ListAttachedUserPolicies
    -- ** Request constructor
    , listAttachedUserPolicies
    -- ** Request lenses
    , lauprqPathPrefix
    , lauprqMaxItems
    , lauprqMarker
    , lauprqUserName

    -- * Response
    , ListAttachedUserPoliciesResponse
    -- ** Response constructor
    , listAttachedUserPoliciesResponse
    -- ** Response lenses
    , lauprsAttachedPolicies
    , lauprsMarker
    , lauprsIsTruncated
    , lauprsStatus
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listAttachedUserPolicies' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lauprqPathPrefix'
--
-- * 'lauprqMaxItems'
--
-- * 'lauprqMarker'
--
-- * 'lauprqUserName'
data ListAttachedUserPolicies = ListAttachedUserPolicies'
    { _lauprqPathPrefix :: !(Maybe Text)
    , _lauprqMaxItems   :: !(Maybe Nat)
    , _lauprqMarker     :: !(Maybe Text)
    , _lauprqUserName   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListAttachedUserPolicies' smart constructor.
listAttachedUserPolicies :: Text -> ListAttachedUserPolicies
listAttachedUserPolicies pUserName_ =
    ListAttachedUserPolicies'
    { _lauprqPathPrefix = Nothing
    , _lauprqMaxItems = Nothing
    , _lauprqMarker = Nothing
    , _lauprqUserName = pUserName_
    }

-- | The path prefix for filtering the results. This parameter is optional.
-- If it is not included, it defaults to a slash (\/), listing all
-- policies.
lauprqPathPrefix :: Lens' ListAttachedUserPolicies (Maybe Text)
lauprqPathPrefix = lens _lauprqPathPrefix (\ s a -> s{_lauprqPathPrefix = a});

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If there are additional items beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- This parameter is optional. If you do not include it, it defaults to
-- 100.
lauprqMaxItems :: Lens' ListAttachedUserPolicies (Maybe Natural)
lauprqMaxItems = lens _lauprqMaxItems (\ s a -> s{_lauprqMaxItems = a}) . mapping _Nat;

-- | Use this parameter only when paginating results and only after you have
-- received a response where the results are truncated. Set it to the value
-- of the @Marker@ element in the response you just received.
lauprqMarker :: Lens' ListAttachedUserPolicies (Maybe Text)
lauprqMarker = lens _lauprqMarker (\ s a -> s{_lauprqMarker = a});

-- | The name (friendly name, not ARN) of the user to list attached policies
-- for.
lauprqUserName :: Lens' ListAttachedUserPolicies Text
lauprqUserName = lens _lauprqUserName (\ s a -> s{_lauprqUserName = a});

instance AWSRequest ListAttachedUserPolicies where
        type Sv ListAttachedUserPolicies = IAM
        type Rs ListAttachedUserPolicies =
             ListAttachedUserPoliciesResponse
        request = post
        response
          = receiveXMLWrapper "ListAttachedUserPoliciesResult"
              (\ s h x ->
                 ListAttachedUserPoliciesResponse' <$>
                   (x .@? "AttachedPolicies" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "Marker")
                     <*> (x .@? "IsTruncated")
                     <*> (pure (fromEnum s)))

instance ToHeaders ListAttachedUserPolicies where
        toHeaders = const mempty

instance ToPath ListAttachedUserPolicies where
        toPath = const "/"

instance ToQuery ListAttachedUserPolicies where
        toQuery ListAttachedUserPolicies'{..}
          = mconcat
              ["Action" =:
                 ("ListAttachedUserPolicies" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "PathPrefix" =: _lauprqPathPrefix,
               "MaxItems" =: _lauprqMaxItems,
               "Marker" =: _lauprqMarker,
               "UserName" =: _lauprqUserName]

-- | Contains the response to a successful ListAttachedUserPolicies request.
--
-- /See:/ 'listAttachedUserPoliciesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lauprsAttachedPolicies'
--
-- * 'lauprsMarker'
--
-- * 'lauprsIsTruncated'
--
-- * 'lauprsStatus'
data ListAttachedUserPoliciesResponse = ListAttachedUserPoliciesResponse'
    { _lauprsAttachedPolicies :: !(Maybe [AttachedPolicy])
    , _lauprsMarker           :: !(Maybe Text)
    , _lauprsIsTruncated      :: !(Maybe Bool)
    , _lauprsStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListAttachedUserPoliciesResponse' smart constructor.
listAttachedUserPoliciesResponse :: Int -> ListAttachedUserPoliciesResponse
listAttachedUserPoliciesResponse pStatus_ =
    ListAttachedUserPoliciesResponse'
    { _lauprsAttachedPolicies = Nothing
    , _lauprsMarker = Nothing
    , _lauprsIsTruncated = Nothing
    , _lauprsStatus = pStatus_
    }

-- | A list of the attached policies.
lauprsAttachedPolicies :: Lens' ListAttachedUserPoliciesResponse [AttachedPolicy]
lauprsAttachedPolicies = lens _lauprsAttachedPolicies (\ s a -> s{_lauprsAttachedPolicies = a}) . _Default;

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
lauprsMarker :: Lens' ListAttachedUserPoliciesResponse (Maybe Text)
lauprsMarker = lens _lauprsMarker (\ s a -> s{_lauprsMarker = a});

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items.
lauprsIsTruncated :: Lens' ListAttachedUserPoliciesResponse (Maybe Bool)
lauprsIsTruncated = lens _lauprsIsTruncated (\ s a -> s{_lauprsIsTruncated = a});

-- | FIXME: Undocumented member.
lauprsStatus :: Lens' ListAttachedUserPoliciesResponse Int
lauprsStatus = lens _lauprsStatus (\ s a -> s{_lauprsStatus = a});
