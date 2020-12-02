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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the names of the inline policies embedded in the specified IAM user.
--
--
-- An IAM user can also have managed policies attached to it. To list the managed policies that are attached to a user, use 'ListAttachedUserPolicies' . For more information about policies, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
-- You can paginate the results using the @MaxItems@ and @Marker@ parameters. If there are no inline policies embedded with the specified user, the operation returns an empty list.
--
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

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listUserPolicies' smart constructor.
data ListUserPolicies = ListUserPolicies'
  { _lupMarker   :: !(Maybe Text)
  , _lupMaxItems :: !(Maybe Nat)
  , _lupUserName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListUserPolicies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lupMarker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- * 'lupMaxItems' - (Optional) Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ . If you do not include this parameter, it defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- * 'lupUserName' - The name of the user to list policies for. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
listUserPolicies
    :: Text -- ^ 'lupUserName'
    -> ListUserPolicies
listUserPolicies pUserName_ =
  ListUserPolicies'
    {_lupMarker = Nothing, _lupMaxItems = Nothing, _lupUserName = pUserName_}


-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
lupMarker :: Lens' ListUserPolicies (Maybe Text)
lupMarker = lens _lupMarker (\ s a -> s{_lupMarker = a})

-- | (Optional) Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ . If you do not include this parameter, it defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
lupMaxItems :: Lens' ListUserPolicies (Maybe Natural)
lupMaxItems = lens _lupMaxItems (\ s a -> s{_lupMaxItems = a}) . mapping _Nat

-- | The name of the user to list policies for. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
lupUserName :: Lens' ListUserPolicies Text
lupUserName = lens _lupUserName (\ s a -> s{_lupUserName = a})

instance AWSPager ListUserPolicies where
        page rq rs
          | stop (rs ^. luprsIsTruncated) = Nothing
          | isNothing (rs ^. luprsMarker) = Nothing
          | otherwise =
            Just $ rq & lupMarker .~ rs ^. luprsMarker

instance AWSRequest ListUserPolicies where
        type Rs ListUserPolicies = ListUserPoliciesResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "ListUserPoliciesResult"
              (\ s h x ->
                 ListUserPoliciesResponse' <$>
                   (x .@? "Marker") <*> (x .@? "IsTruncated") <*>
                     (pure (fromEnum s))
                     <*>
                     (x .@? "PolicyNames" .!@ mempty >>=
                        parseXMLList "member"))

instance Hashable ListUserPolicies where

instance NFData ListUserPolicies where

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

-- | Contains the response to a successful 'ListUserPolicies' request.
--
--
--
-- /See:/ 'listUserPoliciesResponse' smart constructor.
data ListUserPoliciesResponse = ListUserPoliciesResponse'
  { _luprsMarker         :: !(Maybe Text)
  , _luprsIsTruncated    :: !(Maybe Bool)
  , _luprsResponseStatus :: !Int
  , _luprsPolicyNames    :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListUserPoliciesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'luprsMarker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- * 'luprsIsTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all of your results.
--
-- * 'luprsResponseStatus' - -- | The response status code.
--
-- * 'luprsPolicyNames' - A list of policy names.
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


-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
luprsMarker :: Lens' ListUserPoliciesResponse (Maybe Text)
luprsMarker = lens _luprsMarker (\ s a -> s{_luprsMarker = a})

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all of your results.
luprsIsTruncated :: Lens' ListUserPoliciesResponse (Maybe Bool)
luprsIsTruncated = lens _luprsIsTruncated (\ s a -> s{_luprsIsTruncated = a})

-- | -- | The response status code.
luprsResponseStatus :: Lens' ListUserPoliciesResponse Int
luprsResponseStatus = lens _luprsResponseStatus (\ s a -> s{_luprsResponseStatus = a})

-- | A list of policy names.
luprsPolicyNames :: Lens' ListUserPoliciesResponse [Text]
luprsPolicyNames = lens _luprsPolicyNames (\ s a -> s{_luprsPolicyNames = a}) . _Coerce

instance NFData ListUserPoliciesResponse where
