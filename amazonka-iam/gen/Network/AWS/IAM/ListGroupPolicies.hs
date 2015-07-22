{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListGroupPolicies
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Lists the names of the inline policies that are embedded in the
-- specified group.
--
-- A group can also have managed policies attached to it. To list the
-- managed policies that are attached to a group, use
-- ListAttachedGroupPolicies. For more information about policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- You can paginate the results using the @MaxItems@ and @Marker@
-- parameters. If there are no inline policies embedded with the specified
-- group, the action returns an empty list.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListGroupPolicies.html>
module Network.AWS.IAM.ListGroupPolicies
    (
    -- * Request
      ListGroupPolicies
    -- ** Request constructor
    , listGroupPolicies
    -- ** Request lenses
    , lgprqMaxItems
    , lgprqMarker
    , lgprqGroupName

    -- * Response
    , ListGroupPoliciesResponse
    -- ** Response constructor
    , listGroupPoliciesResponse
    -- ** Response lenses
    , lgprsMarker
    , lgprsIsTruncated
    , lgprsStatus
    , lgprsPolicyNames
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listGroupPolicies' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lgprqMaxItems'
--
-- * 'lgprqMarker'
--
-- * 'lgprqGroupName'
data ListGroupPolicies = ListGroupPolicies'
    { _lgprqMaxItems  :: !(Maybe Nat)
    , _lgprqMarker    :: !(Maybe Text)
    , _lgprqGroupName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListGroupPolicies' smart constructor.
listGroupPolicies :: Text -> ListGroupPolicies
listGroupPolicies pGroupName =
    ListGroupPolicies'
    { _lgprqMaxItems = Nothing
    , _lgprqMarker = Nothing
    , _lgprqGroupName = pGroupName
    }

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If there are additional items beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- This parameter is optional. If you do not include it, it defaults to
-- 100.
lgprqMaxItems :: Lens' ListGroupPolicies (Maybe Natural)
lgprqMaxItems = lens _lgprqMaxItems (\ s a -> s{_lgprqMaxItems = a}) . mapping _Nat;

-- | Use this parameter only when paginating results and only after you have
-- received a response where the results are truncated. Set it to the value
-- of the @Marker@ element in the response you just received.
lgprqMarker :: Lens' ListGroupPolicies (Maybe Text)
lgprqMarker = lens _lgprqMarker (\ s a -> s{_lgprqMarker = a});

-- | The name of the group to list policies for.
lgprqGroupName :: Lens' ListGroupPolicies Text
lgprqGroupName = lens _lgprqGroupName (\ s a -> s{_lgprqGroupName = a});

instance AWSPager ListGroupPolicies where
        page rq rs
          | stop (rs ^. lgprsIsTruncated) = Nothing
          | isNothing (rs ^. lgprsMarker) = Nothing
          | otherwise =
            Just $ rq & lgprqMarker .~ rs ^. lgprsMarker

instance AWSRequest ListGroupPolicies where
        type Sv ListGroupPolicies = IAM
        type Rs ListGroupPolicies = ListGroupPoliciesResponse
        request = post
        response
          = receiveXMLWrapper "ListGroupPoliciesResult"
              (\ s h x ->
                 ListGroupPoliciesResponse' <$>
                   (x .@? "Marker") <*> (x .@? "IsTruncated") <*>
                     (pure (fromEnum s))
                     <*>
                     (x .@? "PolicyNames" .!@ mempty >>=
                        parseXMLList "member"))

instance ToHeaders ListGroupPolicies where
        toHeaders = const mempty

instance ToPath ListGroupPolicies where
        toPath = const "/"

instance ToQuery ListGroupPolicies where
        toQuery ListGroupPolicies'{..}
          = mconcat
              ["Action" =: ("ListGroupPolicies" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "MaxItems" =: _lgprqMaxItems,
               "Marker" =: _lgprqMarker,
               "GroupName" =: _lgprqGroupName]

-- | Contains the response to a successful ListGroupPolicies request.
--
-- /See:/ 'listGroupPoliciesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lgprsMarker'
--
-- * 'lgprsIsTruncated'
--
-- * 'lgprsStatus'
--
-- * 'lgprsPolicyNames'
data ListGroupPoliciesResponse = ListGroupPoliciesResponse'
    { _lgprsMarker      :: !(Maybe Text)
    , _lgprsIsTruncated :: !(Maybe Bool)
    , _lgprsStatus      :: !Int
    , _lgprsPolicyNames :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListGroupPoliciesResponse' smart constructor.
listGroupPoliciesResponse :: Int -> ListGroupPoliciesResponse
listGroupPoliciesResponse pStatus =
    ListGroupPoliciesResponse'
    { _lgprsMarker = Nothing
    , _lgprsIsTruncated = Nothing
    , _lgprsStatus = pStatus
    , _lgprsPolicyNames = mempty
    }

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
lgprsMarker :: Lens' ListGroupPoliciesResponse (Maybe Text)
lgprsMarker = lens _lgprsMarker (\ s a -> s{_lgprsMarker = a});

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items.
lgprsIsTruncated :: Lens' ListGroupPoliciesResponse (Maybe Bool)
lgprsIsTruncated = lens _lgprsIsTruncated (\ s a -> s{_lgprsIsTruncated = a});

-- | FIXME: Undocumented member.
lgprsStatus :: Lens' ListGroupPoliciesResponse Int
lgprsStatus = lens _lgprsStatus (\ s a -> s{_lgprsStatus = a});

-- | A list of policy names.
lgprsPolicyNames :: Lens' ListGroupPoliciesResponse [Text]
lgprsPolicyNames = lens _lgprsPolicyNames (\ s a -> s{_lgprsPolicyNames = a});
