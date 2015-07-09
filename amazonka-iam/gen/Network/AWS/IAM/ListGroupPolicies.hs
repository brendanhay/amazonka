{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListGroupPolicies
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Lists the names of the inline policies that are embedded in the
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
    , lgpMaxItems
    , lgpMarker
    , lgpGroupName

    -- * Response
    , ListGroupPoliciesResponse
    -- ** Response constructor
    , listGroupPoliciesResponse
    -- ** Response lenses
    , lgprMarker
    , lgprIsTruncated
    , lgprStatus
    , lgprPolicyNames
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
-- * 'lgpMaxItems'
--
-- * 'lgpMarker'
--
-- * 'lgpGroupName'
data ListGroupPolicies = ListGroupPolicies'
    { _lgpMaxItems  :: !(Maybe Nat)
    , _lgpMarker    :: !(Maybe Text)
    , _lgpGroupName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListGroupPolicies' smart constructor.
listGroupPolicies :: Text -> ListGroupPolicies
listGroupPolicies pGroupName =
    ListGroupPolicies'
    { _lgpMaxItems = Nothing
    , _lgpMarker = Nothing
    , _lgpGroupName = pGroupName
    }

-- | Use this only when paginating results to indicate the maximum number of
-- policy names you want in the response. If there are additional policy
-- names beyond the maximum you specify, the @IsTruncated@ response element
-- is @true@. This parameter is optional. If you do not include it, it
-- defaults to 100.
lgpMaxItems :: Lens' ListGroupPolicies (Maybe Natural)
lgpMaxItems = lens _lgpMaxItems (\ s a -> s{_lgpMaxItems = a}) . mapping _Nat;

-- | Use this only when paginating results, and only in a subsequent request
-- after you\'ve received a response where the results are truncated. Set
-- it to the value of the @Marker@ element in the response you just
-- received.
lgpMarker :: Lens' ListGroupPolicies (Maybe Text)
lgpMarker = lens _lgpMarker (\ s a -> s{_lgpMarker = a});

-- | The name of the group to list policies for.
lgpGroupName :: Lens' ListGroupPolicies Text
lgpGroupName = lens _lgpGroupName (\ s a -> s{_lgpGroupName = a});

instance AWSPager ListGroupPolicies where
        page rq rs
          | stop (rs ^. lgprIsTruncated) = Nothing
          | isNothing (rs ^. lgprMarker) = Nothing
          | otherwise =
            Just $ rq & lgpMarker .~ rs ^. lgprMarker

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
               "MaxItems" =: _lgpMaxItems, "Marker" =: _lgpMarker,
               "GroupName" =: _lgpGroupName]

-- | Contains the response to a successful ListGroupPolicies request.
--
-- /See:/ 'listGroupPoliciesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lgprMarker'
--
-- * 'lgprIsTruncated'
--
-- * 'lgprStatus'
--
-- * 'lgprPolicyNames'
data ListGroupPoliciesResponse = ListGroupPoliciesResponse'
    { _lgprMarker      :: !(Maybe Text)
    , _lgprIsTruncated :: !(Maybe Bool)
    , _lgprStatus      :: !Int
    , _lgprPolicyNames :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListGroupPoliciesResponse' smart constructor.
listGroupPoliciesResponse :: Int -> ListGroupPoliciesResponse
listGroupPoliciesResponse pStatus =
    ListGroupPoliciesResponse'
    { _lgprMarker = Nothing
    , _lgprIsTruncated = Nothing
    , _lgprStatus = pStatus
    , _lgprPolicyNames = mempty
    }

-- | If @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
lgprMarker :: Lens' ListGroupPoliciesResponse (Maybe Text)
lgprMarker = lens _lgprMarker (\ s a -> s{_lgprMarker = a});

-- | A flag that indicates whether there are more policy names to list. If
-- your results were truncated, you can make a subsequent pagination
-- request using the @Marker@ request parameter to retrieve more policy
-- names in the list.
lgprIsTruncated :: Lens' ListGroupPoliciesResponse (Maybe Bool)
lgprIsTruncated = lens _lgprIsTruncated (\ s a -> s{_lgprIsTruncated = a});

-- | FIXME: Undocumented member.
lgprStatus :: Lens' ListGroupPoliciesResponse Int
lgprStatus = lens _lgprStatus (\ s a -> s{_lgprStatus = a});

-- | A list of policy names.
lgprPolicyNames :: Lens' ListGroupPoliciesResponse [Text]
lgprPolicyNames = lens _lgprPolicyNames (\ s a -> s{_lgprPolicyNames = a});
