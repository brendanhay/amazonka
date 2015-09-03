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
-- Module      : Network.AWS.IAM.ListUserPolicies
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the names of the inline policies embedded in the specified user.
--
-- A user can also have managed policies attached to it. To list the
-- managed policies that are attached to a user, use
-- ListAttachedUserPolicies. For more information about policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- You can paginate the results using the 'MaxItems' and 'Marker'
-- parameters. If there are no inline policies embedded with the specified
-- user, the action returns an empty list.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListUserPolicies.html AWS API Reference> for ListUserPolicies.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListUserPolicies
    (
    -- * Creating a Request
      listUserPolicies
    , ListUserPolicies
    -- * Request Lenses
    , lupMarker
    , lupMaxItems
    , lupUserName

    -- * Destructuring the Response
    , listUserPoliciesResponse
    , ListUserPoliciesResponse
    -- * Response Lenses
    , luprsMarker
    , luprsIsTruncated
    , luprsResponseStatus
    , luprsPolicyNames
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listUserPolicies' smart constructor.
data ListUserPolicies = ListUserPolicies'
    { _lupMarker   :: !(Maybe Text)
    , _lupMaxItems :: !(Maybe Nat)
    , _lupUserName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListUserPolicies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lupMarker'
--
-- * 'lupMaxItems'
--
-- * 'lupUserName'
listUserPolicies
    :: Text -- ^ 'lupUserName'
    -> ListUserPolicies
listUserPolicies pUserName_ =
    ListUserPolicies'
    { _lupMarker = Nothing
    , _lupMaxItems = Nothing
    , _lupUserName = pUserName_
    }

-- | Use this parameter only when paginating results and only after you have
-- received a response where the results are truncated. Set it to the value
-- of the 'Marker' element in the response you just received.
lupMarker :: Lens' ListUserPolicies (Maybe Text)
lupMarker = lens _lupMarker (\ s a -> s{_lupMarker = a});

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If there are additional items beyond the
-- maximum you specify, the 'IsTruncated' response element is 'true'.
--
-- This parameter is optional. If you do not include it, it defaults to
-- 100.
lupMaxItems :: Lens' ListUserPolicies (Maybe Natural)
lupMaxItems = lens _lupMaxItems (\ s a -> s{_lupMaxItems = a}) . mapping _Nat;

-- | The name of the user to list policies for.
lupUserName :: Lens' ListUserPolicies Text
lupUserName = lens _lupUserName (\ s a -> s{_lupUserName = a});

instance AWSPager ListUserPolicies where
        page rq rs
          | stop (rs ^. luprsMarker) = Nothing
          | stop (rs ^. luprsPolicyNames) = Nothing
          | otherwise =
            Just $ rq & lupMarker .~ rs ^. luprsMarker

instance AWSRequest ListUserPolicies where
        type Rs ListUserPolicies = ListUserPoliciesResponse
        request = postQuery iAM
        response
          = receiveXMLWrapper "ListUserPoliciesResult"
              (\ s h x ->
                 ListUserPoliciesResponse' <$>
                   (x .@? "Marker") <*> (x .@? "IsTruncated") <*>
                     (pure (fromEnum s))
                     <*>
                     (x .@? "PolicyNames" .!@ mempty >>=
                        parseXMLList "member"))

instance ToHeaders ListUserPolicies where
        toHeaders = const mempty

instance ToPath ListUserPolicies where
        toPath = const "/"

instance ToQuery ListUserPolicies where
        toQuery ListUserPolicies'{..}
          = mconcat
              ["Action" =: ("ListUserPolicies" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "Marker" =: _lupMarker, "MaxItems" =: _lupMaxItems,
               "UserName" =: _lupUserName]

-- | Contains the response to a successful ListUserPolicies request.
--
-- /See:/ 'listUserPoliciesResponse' smart constructor.
data ListUserPoliciesResponse = ListUserPoliciesResponse'
    { _luprsMarker         :: !(Maybe Text)
    , _luprsIsTruncated    :: !(Maybe Bool)
    , _luprsResponseStatus :: !Int
    , _luprsPolicyNames    :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListUserPoliciesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'luprsMarker'
--
-- * 'luprsIsTruncated'
--
-- * 'luprsResponseStatus'
--
-- * 'luprsPolicyNames'
listUserPoliciesResponse
    :: Int -- ^ 'luprsResponseStatus'
    -> ListUserPoliciesResponse
listUserPoliciesResponse pResponseStatus_ =
    ListUserPoliciesResponse'
    { _luprsMarker = Nothing
    , _luprsIsTruncated = Nothing
    , _luprsResponseStatus = pResponseStatus_
    , _luprsPolicyNames = mempty
    }

-- | When 'IsTruncated' is 'true', this element is present and contains the
-- value to use for the 'Marker' parameter in a subsequent pagination
-- request.
luprsMarker :: Lens' ListUserPoliciesResponse (Maybe Text)
luprsMarker = lens _luprsMarker (\ s a -> s{_luprsMarker = a});

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the 'Marker' request parameter to retrieve more items.
luprsIsTruncated :: Lens' ListUserPoliciesResponse (Maybe Bool)
luprsIsTruncated = lens _luprsIsTruncated (\ s a -> s{_luprsIsTruncated = a});

-- | The response status code.
luprsResponseStatus :: Lens' ListUserPoliciesResponse Int
luprsResponseStatus = lens _luprsResponseStatus (\ s a -> s{_luprsResponseStatus = a});

-- | A list of policy names.
luprsPolicyNames :: Lens' ListUserPoliciesResponse [Text]
luprsPolicyNames = lens _luprsPolicyNames (\ s a -> s{_luprsPolicyNames = a}) . _Coerce;
