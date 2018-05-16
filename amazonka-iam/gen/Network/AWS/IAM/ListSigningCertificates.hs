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
-- Module      : Network.AWS.IAM.ListSigningCertificates
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the signing certificates associated with the specified IAM user. If there are none, the operation returns an empty list.
--
--
-- Although each user is limited to a small number of signing certificates, you can still paginate the results using the @MaxItems@ and @Marker@ parameters.
--
-- If the @UserName@ field is not specified, the user name is determined implicitly based on the AWS access key ID used to sign the request for this API. Because this operation works for access keys under the AWS account, you can use this operation to manage AWS account root user credentials even if the AWS account has no associated users.
--
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListSigningCertificates
    (
    -- * Creating a Request
      listSigningCertificates
    , ListSigningCertificates
    -- * Request Lenses
    , lUserName
    , lMarker
    , lMaxItems

    -- * Destructuring the Response
    , listSigningCertificatesResponse
    , ListSigningCertificatesResponse
    -- * Response Lenses
    , lrsMarker
    , lrsIsTruncated
    , lrsResponseStatus
    , lrsCertificates
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listSigningCertificates' smart constructor.
data ListSigningCertificates = ListSigningCertificates'
  { _lUserName :: !(Maybe Text)
  , _lMarker   :: !(Maybe Text)
  , _lMaxItems :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListSigningCertificates' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lUserName' - The name of the IAM user whose signing certificates you want to examine. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- * 'lMarker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- * 'lMaxItems' - (Optional) Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ . If you do not include this parameter, it defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
listSigningCertificates
    :: ListSigningCertificates
listSigningCertificates =
  ListSigningCertificates'
    {_lUserName = Nothing, _lMarker = Nothing, _lMaxItems = Nothing}


-- | The name of the IAM user whose signing certificates you want to examine. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
lUserName :: Lens' ListSigningCertificates (Maybe Text)
lUserName = lens _lUserName (\ s a -> s{_lUserName = a})

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
lMarker :: Lens' ListSigningCertificates (Maybe Text)
lMarker = lens _lMarker (\ s a -> s{_lMarker = a})

-- | (Optional) Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ . If you do not include this parameter, it defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
lMaxItems :: Lens' ListSigningCertificates (Maybe Natural)
lMaxItems = lens _lMaxItems (\ s a -> s{_lMaxItems = a}) . mapping _Nat

instance AWSPager ListSigningCertificates where
        page rq rs
          | stop (rs ^. lrsIsTruncated) = Nothing
          | isNothing (rs ^. lrsMarker) = Nothing
          | otherwise = Just $ rq & lMarker .~ rs ^. lrsMarker

instance AWSRequest ListSigningCertificates where
        type Rs ListSigningCertificates =
             ListSigningCertificatesResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "ListSigningCertificatesResult"
              (\ s h x ->
                 ListSigningCertificatesResponse' <$>
                   (x .@? "Marker") <*> (x .@? "IsTruncated") <*>
                     (pure (fromEnum s))
                     <*>
                     (x .@? "Certificates" .!@ mempty >>=
                        parseXMLList "member"))

instance Hashable ListSigningCertificates where

instance NFData ListSigningCertificates where

instance ToHeaders ListSigningCertificates where
        toHeaders = const mempty

instance ToPath ListSigningCertificates where
        toPath = const "/"

instance ToQuery ListSigningCertificates where
        toQuery ListSigningCertificates'{..}
          = mconcat
              ["Action" =:
                 ("ListSigningCertificates" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "UserName" =: _lUserName, "Marker" =: _lMarker,
               "MaxItems" =: _lMaxItems]

-- | Contains the response to a successful 'ListSigningCertificates' request.
--
--
--
-- /See:/ 'listSigningCertificatesResponse' smart constructor.
data ListSigningCertificatesResponse = ListSigningCertificatesResponse'
  { _lrsMarker         :: !(Maybe Text)
  , _lrsIsTruncated    :: !(Maybe Bool)
  , _lrsResponseStatus :: !Int
  , _lrsCertificates   :: ![SigningCertificate]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListSigningCertificatesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrsMarker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- * 'lrsIsTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all of your results.
--
-- * 'lrsResponseStatus' - -- | The response status code.
--
-- * 'lrsCertificates' - A list of the user's signing certificate information.
listSigningCertificatesResponse
    :: Int -- ^ 'lrsResponseStatus'
    -> ListSigningCertificatesResponse
listSigningCertificatesResponse pResponseStatus_ =
  ListSigningCertificatesResponse'
    { _lrsMarker = Nothing
    , _lrsIsTruncated = Nothing
    , _lrsResponseStatus = pResponseStatus_
    , _lrsCertificates = mempty
    }


-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
lrsMarker :: Lens' ListSigningCertificatesResponse (Maybe Text)
lrsMarker = lens _lrsMarker (\ s a -> s{_lrsMarker = a})

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all of your results.
lrsIsTruncated :: Lens' ListSigningCertificatesResponse (Maybe Bool)
lrsIsTruncated = lens _lrsIsTruncated (\ s a -> s{_lrsIsTruncated = a})

-- | -- | The response status code.
lrsResponseStatus :: Lens' ListSigningCertificatesResponse Int
lrsResponseStatus = lens _lrsResponseStatus (\ s a -> s{_lrsResponseStatus = a})

-- | A list of the user's signing certificate information.
lrsCertificates :: Lens' ListSigningCertificatesResponse [SigningCertificate]
lrsCertificates = lens _lrsCertificates (\ s a -> s{_lrsCertificates = a}) . _Coerce

instance NFData ListSigningCertificatesResponse where
