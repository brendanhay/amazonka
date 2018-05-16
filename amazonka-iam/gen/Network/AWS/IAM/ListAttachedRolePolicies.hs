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
-- Module      : Network.AWS.IAM.ListAttachedRolePolicies
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all managed policies that are attached to the specified IAM role.
--
--
-- An IAM role can also have inline policies embedded with it. To list the inline policies for a role, use the 'ListRolePolicies' API. For information about policies, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
-- You can paginate the results using the @MaxItems@ and @Marker@ parameters. You can use the @PathPrefix@ parameter to limit the list of policies to only those matching the specified path prefix. If there are no policies attached to the specified role (or none that match the specified path prefix), the operation returns an empty list.
--
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListAttachedRolePolicies
    (
    -- * Creating a Request
      listAttachedRolePolicies
    , ListAttachedRolePolicies
    -- * Request Lenses
    , larpPathPrefix
    , larpMarker
    , larpMaxItems
    , larpRoleName

    -- * Destructuring the Response
    , listAttachedRolePoliciesResponse
    , ListAttachedRolePoliciesResponse
    -- * Response Lenses
    , larprsAttachedPolicies
    , larprsMarker
    , larprsIsTruncated
    , larprsResponseStatus
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listAttachedRolePolicies' smart constructor.
data ListAttachedRolePolicies = ListAttachedRolePolicies'
  { _larpPathPrefix :: !(Maybe Text)
  , _larpMarker     :: !(Maybe Text)
  , _larpMaxItems   :: !(Maybe Nat)
  , _larpRoleName   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAttachedRolePolicies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'larpPathPrefix' - The path prefix for filtering the results. This parameter is optional. If it is not included, it defaults to a slash (/), listing all policies. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (\u0021) through the DEL character (\u007F), including most punctuation characters, digits, and upper and lowercased letters.
--
-- * 'larpMarker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- * 'larpMaxItems' - (Optional) Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ . If you do not include this parameter, it defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- * 'larpRoleName' - The name (friendly name, not ARN) of the role to list attached policies for. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
listAttachedRolePolicies
    :: Text -- ^ 'larpRoleName'
    -> ListAttachedRolePolicies
listAttachedRolePolicies pRoleName_ =
  ListAttachedRolePolicies'
    { _larpPathPrefix = Nothing
    , _larpMarker = Nothing
    , _larpMaxItems = Nothing
    , _larpRoleName = pRoleName_
    }


-- | The path prefix for filtering the results. This parameter is optional. If it is not included, it defaults to a slash (/), listing all policies. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (\u0021) through the DEL character (\u007F), including most punctuation characters, digits, and upper and lowercased letters.
larpPathPrefix :: Lens' ListAttachedRolePolicies (Maybe Text)
larpPathPrefix = lens _larpPathPrefix (\ s a -> s{_larpPathPrefix = a})

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
larpMarker :: Lens' ListAttachedRolePolicies (Maybe Text)
larpMarker = lens _larpMarker (\ s a -> s{_larpMarker = a})

-- | (Optional) Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ . If you do not include this parameter, it defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
larpMaxItems :: Lens' ListAttachedRolePolicies (Maybe Natural)
larpMaxItems = lens _larpMaxItems (\ s a -> s{_larpMaxItems = a}) . mapping _Nat

-- | The name (friendly name, not ARN) of the role to list attached policies for. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
larpRoleName :: Lens' ListAttachedRolePolicies Text
larpRoleName = lens _larpRoleName (\ s a -> s{_larpRoleName = a})

instance AWSPager ListAttachedRolePolicies where
        page rq rs
          | stop (rs ^. larprsIsTruncated) = Nothing
          | isNothing (rs ^. larprsMarker) = Nothing
          | otherwise =
            Just $ rq & larpMarker .~ rs ^. larprsMarker

instance AWSRequest ListAttachedRolePolicies where
        type Rs ListAttachedRolePolicies =
             ListAttachedRolePoliciesResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "ListAttachedRolePoliciesResult"
              (\ s h x ->
                 ListAttachedRolePoliciesResponse' <$>
                   (x .@? "AttachedPolicies" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "Marker")
                     <*> (x .@? "IsTruncated")
                     <*> (pure (fromEnum s)))

instance Hashable ListAttachedRolePolicies where

instance NFData ListAttachedRolePolicies where

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
               "PathPrefix" =: _larpPathPrefix,
               "Marker" =: _larpMarker, "MaxItems" =: _larpMaxItems,
               "RoleName" =: _larpRoleName]

-- | Contains the response to a successful 'ListAttachedRolePolicies' request.
--
--
--
-- /See:/ 'listAttachedRolePoliciesResponse' smart constructor.
data ListAttachedRolePoliciesResponse = ListAttachedRolePoliciesResponse'
  { _larprsAttachedPolicies :: !(Maybe [AttachedPolicy])
  , _larprsMarker           :: !(Maybe Text)
  , _larprsIsTruncated      :: !(Maybe Bool)
  , _larprsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAttachedRolePoliciesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'larprsAttachedPolicies' - A list of the attached policies.
--
-- * 'larprsMarker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- * 'larprsIsTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all of your results.
--
-- * 'larprsResponseStatus' - -- | The response status code.
listAttachedRolePoliciesResponse
    :: Int -- ^ 'larprsResponseStatus'
    -> ListAttachedRolePoliciesResponse
listAttachedRolePoliciesResponse pResponseStatus_ =
  ListAttachedRolePoliciesResponse'
    { _larprsAttachedPolicies = Nothing
    , _larprsMarker = Nothing
    , _larprsIsTruncated = Nothing
    , _larprsResponseStatus = pResponseStatus_
    }


-- | A list of the attached policies.
larprsAttachedPolicies :: Lens' ListAttachedRolePoliciesResponse [AttachedPolicy]
larprsAttachedPolicies = lens _larprsAttachedPolicies (\ s a -> s{_larprsAttachedPolicies = a}) . _Default . _Coerce

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
larprsMarker :: Lens' ListAttachedRolePoliciesResponse (Maybe Text)
larprsMarker = lens _larprsMarker (\ s a -> s{_larprsMarker = a})

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all of your results.
larprsIsTruncated :: Lens' ListAttachedRolePoliciesResponse (Maybe Bool)
larprsIsTruncated = lens _larprsIsTruncated (\ s a -> s{_larprsIsTruncated = a})

-- | -- | The response status code.
larprsResponseStatus :: Lens' ListAttachedRolePoliciesResponse Int
larprsResponseStatus = lens _larprsResponseStatus (\ s a -> s{_larprsResponseStatus = a})

instance NFData ListAttachedRolePoliciesResponse
         where
