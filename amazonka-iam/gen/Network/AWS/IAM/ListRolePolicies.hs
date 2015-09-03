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
-- Module      : Network.AWS.IAM.ListRolePolicies
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the names of the inline policies that are embedded in the
-- specified role.
--
-- A role can also have managed policies attached to it. To list the
-- managed policies that are attached to a role, use
-- ListAttachedRolePolicies. For more information about policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- You can paginate the results using the 'MaxItems' and 'Marker'
-- parameters. If there are no inline policies embedded with the specified
-- role, the action returns an empty list.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListRolePolicies.html AWS API Reference> for ListRolePolicies.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListRolePolicies
    (
    -- * Creating a Request
      listRolePolicies
    , ListRolePolicies
    -- * Request Lenses
    , lrpMarker
    , lrpMaxItems
    , lrpRoleName

    -- * Destructuring the Response
    , listRolePoliciesResponse
    , ListRolePoliciesResponse
    -- * Response Lenses
    , lrprsMarker
    , lrprsIsTruncated
    , lrprsResponseStatus
    , lrprsPolicyNames
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listRolePolicies' smart constructor.
data ListRolePolicies = ListRolePolicies'
    { _lrpMarker   :: !(Maybe Text)
    , _lrpMaxItems :: !(Maybe Nat)
    , _lrpRoleName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListRolePolicies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrpMarker'
--
-- * 'lrpMaxItems'
--
-- * 'lrpRoleName'
listRolePolicies
    :: Text -- ^ 'lrpRoleName'
    -> ListRolePolicies
listRolePolicies pRoleName_ =
    ListRolePolicies'
    { _lrpMarker = Nothing
    , _lrpMaxItems = Nothing
    , _lrpRoleName = pRoleName_
    }

-- | Use this parameter only when paginating results and only after you have
-- received a response where the results are truncated. Set it to the value
-- of the 'Marker' element in the response you just received.
lrpMarker :: Lens' ListRolePolicies (Maybe Text)
lrpMarker = lens _lrpMarker (\ s a -> s{_lrpMarker = a});

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If there are additional items beyond the
-- maximum you specify, the 'IsTruncated' response element is 'true'.
--
-- This parameter is optional. If you do not include it, it defaults to
-- 100.
lrpMaxItems :: Lens' ListRolePolicies (Maybe Natural)
lrpMaxItems = lens _lrpMaxItems (\ s a -> s{_lrpMaxItems = a}) . mapping _Nat;

-- | The name of the role to list policies for.
lrpRoleName :: Lens' ListRolePolicies Text
lrpRoleName = lens _lrpRoleName (\ s a -> s{_lrpRoleName = a});

instance AWSPager ListRolePolicies where
        page rq rs
          | stop (rs ^. lrprsMarker) = Nothing
          | stop (rs ^. lrprsPolicyNames) = Nothing
          | otherwise =
            Just $ rq & lrpMarker .~ rs ^. lrprsMarker

instance AWSRequest ListRolePolicies where
        type Rs ListRolePolicies = ListRolePoliciesResponse
        request = postQuery iAM
        response
          = receiveXMLWrapper "ListRolePoliciesResult"
              (\ s h x ->
                 ListRolePoliciesResponse' <$>
                   (x .@? "Marker") <*> (x .@? "IsTruncated") <*>
                     (pure (fromEnum s))
                     <*>
                     (x .@? "PolicyNames" .!@ mempty >>=
                        parseXMLList "member"))

instance ToHeaders ListRolePolicies where
        toHeaders = const mempty

instance ToPath ListRolePolicies where
        toPath = const "/"

instance ToQuery ListRolePolicies where
        toQuery ListRolePolicies'{..}
          = mconcat
              ["Action" =: ("ListRolePolicies" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "Marker" =: _lrpMarker, "MaxItems" =: _lrpMaxItems,
               "RoleName" =: _lrpRoleName]

-- | Contains the response to a successful ListRolePolicies request.
--
-- /See:/ 'listRolePoliciesResponse' smart constructor.
data ListRolePoliciesResponse = ListRolePoliciesResponse'
    { _lrprsMarker         :: !(Maybe Text)
    , _lrprsIsTruncated    :: !(Maybe Bool)
    , _lrprsResponseStatus :: !Int
    , _lrprsPolicyNames    :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListRolePoliciesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrprsMarker'
--
-- * 'lrprsIsTruncated'
--
-- * 'lrprsResponseStatus'
--
-- * 'lrprsPolicyNames'
listRolePoliciesResponse
    :: Int -- ^ 'lrprsResponseStatus'
    -> ListRolePoliciesResponse
listRolePoliciesResponse pResponseStatus_ =
    ListRolePoliciesResponse'
    { _lrprsMarker = Nothing
    , _lrprsIsTruncated = Nothing
    , _lrprsResponseStatus = pResponseStatus_
    , _lrprsPolicyNames = mempty
    }

-- | When 'IsTruncated' is 'true', this element is present and contains the
-- value to use for the 'Marker' parameter in a subsequent pagination
-- request.
lrprsMarker :: Lens' ListRolePoliciesResponse (Maybe Text)
lrprsMarker = lens _lrprsMarker (\ s a -> s{_lrprsMarker = a});

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the 'Marker' request parameter to retrieve more items.
lrprsIsTruncated :: Lens' ListRolePoliciesResponse (Maybe Bool)
lrprsIsTruncated = lens _lrprsIsTruncated (\ s a -> s{_lrprsIsTruncated = a});

-- | The response status code.
lrprsResponseStatus :: Lens' ListRolePoliciesResponse Int
lrprsResponseStatus = lens _lrprsResponseStatus (\ s a -> s{_lrprsResponseStatus = a});

-- | A list of policy names.
lrprsPolicyNames :: Lens' ListRolePoliciesResponse [Text]
lrprsPolicyNames = lens _lrprsPolicyNames (\ s a -> s{_lrprsPolicyNames = a}) . _Coerce;
