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
-- Module      : Network.AWS.IAM.ListPolicies
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the managed policies that are available in your AWS account, including your own customer-defined managed policies and all AWS managed policies.
--
--
-- You can filter the list of policies that is returned using the optional @OnlyAttached@ , @Scope@ , and @PathPrefix@ parameters. For example, to list only the customer managed policies in your AWS account, set @Scope@ to @Local@ . To list only AWS managed policies, set @Scope@ to @AWS@ .
--
-- You can paginate the results using the @MaxItems@ and @Marker@ parameters.
--
-- For more information about managed policies, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListPolicies
    (
    -- * Creating a Request
      listPolicies
    , ListPolicies
    -- * Request Lenses
    , lpPathPrefix
    , lpOnlyAttached
    , lpMarker
    , lpScope
    , lpMaxItems

    -- * Destructuring the Response
    , listPoliciesResponse
    , ListPoliciesResponse
    -- * Response Lenses
    , lprsMarker
    , lprsIsTruncated
    , lprsPolicies
    , lprsResponseStatus
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listPolicies' smart constructor.
data ListPolicies = ListPolicies'
  { _lpPathPrefix   :: !(Maybe Text)
  , _lpOnlyAttached :: !(Maybe Bool)
  , _lpMarker       :: !(Maybe Text)
  , _lpScope        :: !(Maybe PolicyScopeType)
  , _lpMaxItems     :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPolicies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpPathPrefix' - The path prefix for filtering the results. This parameter is optional. If it is not included, it defaults to a slash (/), listing all policies. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (\u0021) through the DEL character (\u007F), including most punctuation characters, digits, and upper and lowercased letters.
--
-- * 'lpOnlyAttached' - A flag to filter the results to only the attached policies. When @OnlyAttached@ is @true@ , the returned list contains only the policies that are attached to an IAM user, group, or role. When @OnlyAttached@ is @false@ , or when the parameter is not included, all policies are returned.
--
-- * 'lpMarker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- * 'lpScope' - The scope to use for filtering the results. To list only AWS managed policies, set @Scope@ to @AWS@ . To list only the customer managed policies in your AWS account, set @Scope@ to @Local@ . This parameter is optional. If it is not included, or if it is set to @All@ , all policies are returned.
--
-- * 'lpMaxItems' - (Optional) Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ . If you do not include this parameter, it defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
listPolicies
    :: ListPolicies
listPolicies =
  ListPolicies'
    { _lpPathPrefix = Nothing
    , _lpOnlyAttached = Nothing
    , _lpMarker = Nothing
    , _lpScope = Nothing
    , _lpMaxItems = Nothing
    }


-- | The path prefix for filtering the results. This parameter is optional. If it is not included, it defaults to a slash (/), listing all policies. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (\u0021) through the DEL character (\u007F), including most punctuation characters, digits, and upper and lowercased letters.
lpPathPrefix :: Lens' ListPolicies (Maybe Text)
lpPathPrefix = lens _lpPathPrefix (\ s a -> s{_lpPathPrefix = a})

-- | A flag to filter the results to only the attached policies. When @OnlyAttached@ is @true@ , the returned list contains only the policies that are attached to an IAM user, group, or role. When @OnlyAttached@ is @false@ , or when the parameter is not included, all policies are returned.
lpOnlyAttached :: Lens' ListPolicies (Maybe Bool)
lpOnlyAttached = lens _lpOnlyAttached (\ s a -> s{_lpOnlyAttached = a})

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
lpMarker :: Lens' ListPolicies (Maybe Text)
lpMarker = lens _lpMarker (\ s a -> s{_lpMarker = a})

-- | The scope to use for filtering the results. To list only AWS managed policies, set @Scope@ to @AWS@ . To list only the customer managed policies in your AWS account, set @Scope@ to @Local@ . This parameter is optional. If it is not included, or if it is set to @All@ , all policies are returned.
lpScope :: Lens' ListPolicies (Maybe PolicyScopeType)
lpScope = lens _lpScope (\ s a -> s{_lpScope = a})

-- | (Optional) Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ . If you do not include this parameter, it defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
lpMaxItems :: Lens' ListPolicies (Maybe Natural)
lpMaxItems = lens _lpMaxItems (\ s a -> s{_lpMaxItems = a}) . mapping _Nat

instance AWSPager ListPolicies where
        page rq rs
          | stop (rs ^. lprsIsTruncated) = Nothing
          | isNothing (rs ^. lprsMarker) = Nothing
          | otherwise =
            Just $ rq & lpMarker .~ rs ^. lprsMarker

instance AWSRequest ListPolicies where
        type Rs ListPolicies = ListPoliciesResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "ListPoliciesResult"
              (\ s h x ->
                 ListPoliciesResponse' <$>
                   (x .@? "Marker") <*> (x .@? "IsTruncated") <*>
                     (x .@? "Policies" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable ListPolicies where

instance NFData ListPolicies where

instance ToHeaders ListPolicies where
        toHeaders = const mempty

instance ToPath ListPolicies where
        toPath = const "/"

instance ToQuery ListPolicies where
        toQuery ListPolicies'{..}
          = mconcat
              ["Action" =: ("ListPolicies" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "PathPrefix" =: _lpPathPrefix,
               "OnlyAttached" =: _lpOnlyAttached,
               "Marker" =: _lpMarker, "Scope" =: _lpScope,
               "MaxItems" =: _lpMaxItems]

-- | Contains the response to a successful 'ListPolicies' request.
--
--
--
-- /See:/ 'listPoliciesResponse' smart constructor.
data ListPoliciesResponse = ListPoliciesResponse'
  { _lprsMarker         :: !(Maybe Text)
  , _lprsIsTruncated    :: !(Maybe Bool)
  , _lprsPolicies       :: !(Maybe [Policy])
  , _lprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPoliciesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lprsMarker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- * 'lprsIsTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all of your results.
--
-- * 'lprsPolicies' - A list of policies.
--
-- * 'lprsResponseStatus' - -- | The response status code.
listPoliciesResponse
    :: Int -- ^ 'lprsResponseStatus'
    -> ListPoliciesResponse
listPoliciesResponse pResponseStatus_ =
  ListPoliciesResponse'
    { _lprsMarker = Nothing
    , _lprsIsTruncated = Nothing
    , _lprsPolicies = Nothing
    , _lprsResponseStatus = pResponseStatus_
    }


-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
lprsMarker :: Lens' ListPoliciesResponse (Maybe Text)
lprsMarker = lens _lprsMarker (\ s a -> s{_lprsMarker = a})

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all of your results.
lprsIsTruncated :: Lens' ListPoliciesResponse (Maybe Bool)
lprsIsTruncated = lens _lprsIsTruncated (\ s a -> s{_lprsIsTruncated = a})

-- | A list of policies.
lprsPolicies :: Lens' ListPoliciesResponse [Policy]
lprsPolicies = lens _lprsPolicies (\ s a -> s{_lprsPolicies = a}) . _Default . _Coerce

-- | -- | The response status code.
lprsResponseStatus :: Lens' ListPoliciesResponse Int
lprsResponseStatus = lens _lprsResponseStatus (\ s a -> s{_lprsResponseStatus = a})

instance NFData ListPoliciesResponse where
