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
    , laupPathPrefix
    , laupMaxItems
    , laupMarker
    , laupUserName

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
-- * 'laupPathPrefix'
--
-- * 'laupMaxItems'
--
-- * 'laupMarker'
--
-- * 'laupUserName'
data ListAttachedUserPolicies = ListAttachedUserPolicies'
    { _laupPathPrefix :: !(Maybe Text)
    , _laupMaxItems   :: !(Maybe Nat)
    , _laupMarker     :: !(Maybe Text)
    , _laupUserName   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListAttachedUserPolicies' smart constructor.
listAttachedUserPolicies :: Text -> ListAttachedUserPolicies
listAttachedUserPolicies pUserName_ =
    ListAttachedUserPolicies'
    { _laupPathPrefix = Nothing
    , _laupMaxItems = Nothing
    , _laupMarker = Nothing
    , _laupUserName = pUserName_
    }

-- | The path prefix for filtering the results. This parameter is optional.
-- If it is not included, it defaults to a slash (\/), listing all
-- policies.
laupPathPrefix :: Lens' ListAttachedUserPolicies (Maybe Text)
laupPathPrefix = lens _laupPathPrefix (\ s a -> s{_laupPathPrefix = a});

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If there are additional items beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- This parameter is optional. If you do not include it, it defaults to
-- 100.
laupMaxItems :: Lens' ListAttachedUserPolicies (Maybe Natural)
laupMaxItems = lens _laupMaxItems (\ s a -> s{_laupMaxItems = a}) . mapping _Nat;

-- | Use this parameter only when paginating results and only after you have
-- received a response where the results are truncated. Set it to the value
-- of the @Marker@ element in the response you just received.
laupMarker :: Lens' ListAttachedUserPolicies (Maybe Text)
laupMarker = lens _laupMarker (\ s a -> s{_laupMarker = a});

-- | The name (friendly name, not ARN) of the user to list attached policies
-- for.
laupUserName :: Lens' ListAttachedUserPolicies Text
laupUserName = lens _laupUserName (\ s a -> s{_laupUserName = a});

instance AWSRequest ListAttachedUserPolicies where
        type Sv ListAttachedUserPolicies = IAM
        type Rs ListAttachedUserPolicies =
             ListAttachedUserPoliciesResponse
        request = post "ListAttachedUserPolicies"
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
               "PathPrefix" =: _laupPathPrefix,
               "MaxItems" =: _laupMaxItems, "Marker" =: _laupMarker,
               "UserName" =: _laupUserName]

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
