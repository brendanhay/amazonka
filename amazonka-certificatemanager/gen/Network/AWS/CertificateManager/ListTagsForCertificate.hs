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
-- Module      : Network.AWS.CertificateManager.ListTagsForCertificate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags that have been applied to the ACM certificate. Use the certificate's Amazon Resource Name (ARN) to specify the certificate. To add a tag to an ACM certificate, use the 'AddTagsToCertificate' action. To delete a tag, use the 'RemoveTagsFromCertificate' action.
--
--
module Network.AWS.CertificateManager.ListTagsForCertificate
    (
    -- * Creating a Request
      listTagsForCertificate
    , ListTagsForCertificate
    -- * Request Lenses
    , ltfcCertificateARN

    -- * Destructuring the Response
    , listTagsForCertificateResponse
    , ListTagsForCertificateResponse
    -- * Response Lenses
    , ltfcrsTags
    , ltfcrsResponseStatus
    ) where

import Network.AWS.CertificateManager.Types
import Network.AWS.CertificateManager.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listTagsForCertificate' smart constructor.
newtype ListTagsForCertificate = ListTagsForCertificate'
  { _ltfcCertificateARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTagsForCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltfcCertificateARN' - String that contains the ARN of the ACM certificate for which you want to list the tags. This must have the following form: @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@  For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
listTagsForCertificate
    :: Text -- ^ 'ltfcCertificateARN'
    -> ListTagsForCertificate
listTagsForCertificate pCertificateARN_ =
  ListTagsForCertificate' {_ltfcCertificateARN = pCertificateARN_}


-- | String that contains the ARN of the ACM certificate for which you want to list the tags. This must have the following form: @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@  For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
ltfcCertificateARN :: Lens' ListTagsForCertificate Text
ltfcCertificateARN = lens _ltfcCertificateARN (\ s a -> s{_ltfcCertificateARN = a})

instance AWSRequest ListTagsForCertificate where
        type Rs ListTagsForCertificate =
             ListTagsForCertificateResponse
        request = postJSON certificateManager
        response
          = receiveJSON
              (\ s h x ->
                 ListTagsForCertificateResponse' <$>
                   (x .?> "Tags") <*> (pure (fromEnum s)))

instance Hashable ListTagsForCertificate where

instance NFData ListTagsForCertificate where

instance ToHeaders ListTagsForCertificate where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CertificateManager.ListTagsForCertificate" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListTagsForCertificate where
        toJSON ListTagsForCertificate'{..}
          = object
              (catMaybes
                 [Just ("CertificateArn" .= _ltfcCertificateARN)])

instance ToPath ListTagsForCertificate where
        toPath = const "/"

instance ToQuery ListTagsForCertificate where
        toQuery = const mempty

-- | /See:/ 'listTagsForCertificateResponse' smart constructor.
data ListTagsForCertificateResponse = ListTagsForCertificateResponse'
  { _ltfcrsTags           :: !(Maybe (List1 Tag))
  , _ltfcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTagsForCertificateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltfcrsTags' - The key-value pairs that define the applied tags.
--
-- * 'ltfcrsResponseStatus' - -- | The response status code.
listTagsForCertificateResponse
    :: Int -- ^ 'ltfcrsResponseStatus'
    -> ListTagsForCertificateResponse
listTagsForCertificateResponse pResponseStatus_ =
  ListTagsForCertificateResponse'
    {_ltfcrsTags = Nothing, _ltfcrsResponseStatus = pResponseStatus_}


-- | The key-value pairs that define the applied tags.
ltfcrsTags :: Lens' ListTagsForCertificateResponse (Maybe (NonEmpty Tag))
ltfcrsTags = lens _ltfcrsTags (\ s a -> s{_ltfcrsTags = a}) . mapping _List1

-- | -- | The response status code.
ltfcrsResponseStatus :: Lens' ListTagsForCertificateResponse Int
ltfcrsResponseStatus = lens _ltfcrsResponseStatus (\ s a -> s{_ltfcrsResponseStatus = a})

instance NFData ListTagsForCertificateResponse where
