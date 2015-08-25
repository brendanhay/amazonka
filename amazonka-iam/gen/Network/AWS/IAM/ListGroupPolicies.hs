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
-- Module      : Network.AWS.IAM.ListGroupPolicies
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
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
-- You can paginate the results using the 'MaxItems' and 'Marker'
-- parameters. If there are no inline policies embedded with the specified
-- group, the action returns an empty list.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListGroupPolicies.html AWS API Reference> for ListGroupPolicies.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListGroupPolicies
    (
    -- * Creating a Request
      listGroupPolicies
    , ListGroupPolicies
    -- * Request Lenses
    , lgpMaxItems
    , lgpMarker
    , lgpGroupName

    -- * Destructuring the Response
    , listGroupPoliciesResponse
    , ListGroupPoliciesResponse
    -- * Response Lenses
    , lgprsMarker
    , lgprsIsTruncated
    , lgprsStatus
    , lgprsPolicyNames
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listGroupPolicies' smart constructor.
data ListGroupPolicies = ListGroupPolicies'
    { _lgpMaxItems  :: !(Maybe Nat)
    , _lgpMarker    :: !(Maybe Text)
    , _lgpGroupName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListGroupPolicies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgpMaxItems'
--
-- * 'lgpMarker'
--
-- * 'lgpGroupName'
listGroupPolicies
    :: Text -- ^ 'lgpGroupName'
    -> ListGroupPolicies
listGroupPolicies pGroupName_ =
    ListGroupPolicies'
    { _lgpMaxItems = Nothing
    , _lgpMarker = Nothing
    , _lgpGroupName = pGroupName_
    }

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If there are additional items beyond the
-- maximum you specify, the 'IsTruncated' response element is 'true'.
--
-- This parameter is optional. If you do not include it, it defaults to
-- 100.
lgpMaxItems :: Lens' ListGroupPolicies (Maybe Natural)
lgpMaxItems = lens _lgpMaxItems (\ s a -> s{_lgpMaxItems = a}) . mapping _Nat;

-- | Use this parameter only when paginating results and only after you have
-- received a response where the results are truncated. Set it to the value
-- of the 'Marker' element in the response you just received.
lgpMarker :: Lens' ListGroupPolicies (Maybe Text)
lgpMarker = lens _lgpMarker (\ s a -> s{_lgpMarker = a});

-- | The name of the group to list policies for.
lgpGroupName :: Lens' ListGroupPolicies Text
lgpGroupName = lens _lgpGroupName (\ s a -> s{_lgpGroupName = a});

instance AWSPager ListGroupPolicies where
        page rq rs
          | stop (rs ^. lgprsIsTruncated) = Nothing
          | isNothing (rs ^. lgprsMarker) = Nothing
          | otherwise =
            Just $ rq & lgpMarker .~ rs ^. lgprsMarker

instance AWSRequest ListGroupPolicies where
        type Rs ListGroupPolicies = ListGroupPoliciesResponse
        request = postQuery iAM
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
data ListGroupPoliciesResponse = ListGroupPoliciesResponse'
    { _lgprsMarker      :: !(Maybe Text)
    , _lgprsIsTruncated :: !(Maybe Bool)
    , _lgprsStatus      :: !Int
    , _lgprsPolicyNames :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListGroupPoliciesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgprsMarker'
--
-- * 'lgprsIsTruncated'
--
-- * 'lgprsStatus'
--
-- * 'lgprsPolicyNames'
listGroupPoliciesResponse
    :: Int -- ^ 'lgprsStatus'
    -> ListGroupPoliciesResponse
listGroupPoliciesResponse pStatus_ =
    ListGroupPoliciesResponse'
    { _lgprsMarker = Nothing
    , _lgprsIsTruncated = Nothing
    , _lgprsStatus = pStatus_
    , _lgprsPolicyNames = mempty
    }

-- | When 'IsTruncated' is 'true', this element is present and contains the
-- value to use for the 'Marker' parameter in a subsequent pagination
-- request.
lgprsMarker :: Lens' ListGroupPoliciesResponse (Maybe Text)
lgprsMarker = lens _lgprsMarker (\ s a -> s{_lgprsMarker = a});

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the 'Marker' request parameter to retrieve more items.
lgprsIsTruncated :: Lens' ListGroupPoliciesResponse (Maybe Bool)
lgprsIsTruncated = lens _lgprsIsTruncated (\ s a -> s{_lgprsIsTruncated = a});

-- | The response status code.
lgprsStatus :: Lens' ListGroupPoliciesResponse Int
lgprsStatus = lens _lgprsStatus (\ s a -> s{_lgprsStatus = a});

-- | A list of policy names.
lgprsPolicyNames :: Lens' ListGroupPoliciesResponse [Text]
lgprsPolicyNames = lens _lgprsPolicyNames (\ s a -> s{_lgprsPolicyNames = a}) . _Coerce;
