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
-- Module      : Network.AWS.IAM.ListPolicyVersions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about the versions of the specified managed policy, including the version that is currently set as the policy's default version.
--
--
-- For more information about managed policies, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListPolicyVersions
    (
    -- * Creating a Request
      listPolicyVersions
    , ListPolicyVersions
    -- * Request Lenses
    , lpvMarker
    , lpvMaxItems
    , lpvPolicyARN

    -- * Destructuring the Response
    , listPolicyVersionsResponse
    , ListPolicyVersionsResponse
    -- * Response Lenses
    , lpvrsVersions
    , lpvrsMarker
    , lpvrsIsTruncated
    , lpvrsResponseStatus
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listPolicyVersions' smart constructor.
data ListPolicyVersions = ListPolicyVersions'
  { _lpvMarker    :: !(Maybe Text)
  , _lpvMaxItems  :: !(Maybe Nat)
  , _lpvPolicyARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPolicyVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpvMarker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- * 'lpvMaxItems' - (Optional) Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ . If you do not include this parameter, it defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- * 'lpvPolicyARN' - The Amazon Resource Name (ARN) of the IAM policy for which you want the versions. For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
listPolicyVersions
    :: Text -- ^ 'lpvPolicyARN'
    -> ListPolicyVersions
listPolicyVersions pPolicyARN_ =
  ListPolicyVersions'
    {_lpvMarker = Nothing, _lpvMaxItems = Nothing, _lpvPolicyARN = pPolicyARN_}


-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
lpvMarker :: Lens' ListPolicyVersions (Maybe Text)
lpvMarker = lens _lpvMarker (\ s a -> s{_lpvMarker = a})

-- | (Optional) Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ . If you do not include this parameter, it defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
lpvMaxItems :: Lens' ListPolicyVersions (Maybe Natural)
lpvMaxItems = lens _lpvMaxItems (\ s a -> s{_lpvMaxItems = a}) . mapping _Nat

-- | The Amazon Resource Name (ARN) of the IAM policy for which you want the versions. For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
lpvPolicyARN :: Lens' ListPolicyVersions Text
lpvPolicyARN = lens _lpvPolicyARN (\ s a -> s{_lpvPolicyARN = a})

instance AWSPager ListPolicyVersions where
        page rq rs
          | stop (rs ^. lpvrsIsTruncated) = Nothing
          | isNothing (rs ^. lpvrsMarker) = Nothing
          | otherwise =
            Just $ rq & lpvMarker .~ rs ^. lpvrsMarker

instance AWSRequest ListPolicyVersions where
        type Rs ListPolicyVersions =
             ListPolicyVersionsResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "ListPolicyVersionsResult"
              (\ s h x ->
                 ListPolicyVersionsResponse' <$>
                   (x .@? "Versions" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "Marker")
                     <*> (x .@? "IsTruncated")
                     <*> (pure (fromEnum s)))

instance Hashable ListPolicyVersions where

instance NFData ListPolicyVersions where

instance ToHeaders ListPolicyVersions where
        toHeaders = const mempty

instance ToPath ListPolicyVersions where
        toPath = const "/"

instance ToQuery ListPolicyVersions where
        toQuery ListPolicyVersions'{..}
          = mconcat
              ["Action" =: ("ListPolicyVersions" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "Marker" =: _lpvMarker, "MaxItems" =: _lpvMaxItems,
               "PolicyArn" =: _lpvPolicyARN]

-- | Contains the response to a successful 'ListPolicyVersions' request.
--
--
--
-- /See:/ 'listPolicyVersionsResponse' smart constructor.
data ListPolicyVersionsResponse = ListPolicyVersionsResponse'
  { _lpvrsVersions       :: !(Maybe [PolicyVersion])
  , _lpvrsMarker         :: !(Maybe Text)
  , _lpvrsIsTruncated    :: !(Maybe Bool)
  , _lpvrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPolicyVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpvrsVersions' - A list of policy versions. For more information about managed policy versions, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/ .
--
-- * 'lpvrsMarker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- * 'lpvrsIsTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all of your results.
--
-- * 'lpvrsResponseStatus' - -- | The response status code.
listPolicyVersionsResponse
    :: Int -- ^ 'lpvrsResponseStatus'
    -> ListPolicyVersionsResponse
listPolicyVersionsResponse pResponseStatus_ =
  ListPolicyVersionsResponse'
    { _lpvrsVersions = Nothing
    , _lpvrsMarker = Nothing
    , _lpvrsIsTruncated = Nothing
    , _lpvrsResponseStatus = pResponseStatus_
    }


-- | A list of policy versions. For more information about managed policy versions, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/ .
lpvrsVersions :: Lens' ListPolicyVersionsResponse [PolicyVersion]
lpvrsVersions = lens _lpvrsVersions (\ s a -> s{_lpvrsVersions = a}) . _Default . _Coerce

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
lpvrsMarker :: Lens' ListPolicyVersionsResponse (Maybe Text)
lpvrsMarker = lens _lpvrsMarker (\ s a -> s{_lpvrsMarker = a})

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all of your results.
lpvrsIsTruncated :: Lens' ListPolicyVersionsResponse (Maybe Bool)
lpvrsIsTruncated = lens _lpvrsIsTruncated (\ s a -> s{_lpvrsIsTruncated = a})

-- | -- | The response status code.
lpvrsResponseStatus :: Lens' ListPolicyVersionsResponse Int
lpvrsResponseStatus = lens _lpvrsResponseStatus (\ s a -> s{_lpvrsResponseStatus = a})

instance NFData ListPolicyVersionsResponse where
