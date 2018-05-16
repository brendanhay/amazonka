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
-- Module      : Network.AWS.CertificateManager.RemoveTagsFromCertificate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove one or more tags from an ACM certificate. A tag consists of a key-value pair. If you do not specify the value portion of the tag when calling this function, the tag will be removed regardless of value. If you specify a value, the tag is removed only if it is associated with the specified value.
--
--
-- To add tags to a certificate, use the 'AddTagsToCertificate' action. To view all of the tags that have been applied to a specific ACM certificate, use the 'ListTagsForCertificate' action.
--
module Network.AWS.CertificateManager.RemoveTagsFromCertificate
    (
    -- * Creating a Request
      removeTagsFromCertificate
    , RemoveTagsFromCertificate
    -- * Request Lenses
    , rtfcCertificateARN
    , rtfcTags

    -- * Destructuring the Response
    , removeTagsFromCertificateResponse
    , RemoveTagsFromCertificateResponse
    ) where

import Network.AWS.CertificateManager.Types
import Network.AWS.CertificateManager.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'removeTagsFromCertificate' smart constructor.
data RemoveTagsFromCertificate = RemoveTagsFromCertificate'
  { _rtfcCertificateARN :: !Text
  , _rtfcTags           :: !(List1 Tag)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveTagsFromCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtfcCertificateARN' - String that contains the ARN of the ACM Certificate with one or more tags that you want to remove. This must be of the form: @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@  For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- * 'rtfcTags' - The key-value pair that defines the tag to remove.
removeTagsFromCertificate
    :: Text -- ^ 'rtfcCertificateARN'
    -> NonEmpty Tag -- ^ 'rtfcTags'
    -> RemoveTagsFromCertificate
removeTagsFromCertificate pCertificateARN_ pTags_ =
  RemoveTagsFromCertificate'
    {_rtfcCertificateARN = pCertificateARN_, _rtfcTags = _List1 # pTags_}


-- | String that contains the ARN of the ACM Certificate with one or more tags that you want to remove. This must be of the form: @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@  For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
rtfcCertificateARN :: Lens' RemoveTagsFromCertificate Text
rtfcCertificateARN = lens _rtfcCertificateARN (\ s a -> s{_rtfcCertificateARN = a})

-- | The key-value pair that defines the tag to remove.
rtfcTags :: Lens' RemoveTagsFromCertificate (NonEmpty Tag)
rtfcTags = lens _rtfcTags (\ s a -> s{_rtfcTags = a}) . _List1

instance AWSRequest RemoveTagsFromCertificate where
        type Rs RemoveTagsFromCertificate =
             RemoveTagsFromCertificateResponse
        request = postJSON certificateManager
        response
          = receiveNull RemoveTagsFromCertificateResponse'

instance Hashable RemoveTagsFromCertificate where

instance NFData RemoveTagsFromCertificate where

instance ToHeaders RemoveTagsFromCertificate where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CertificateManager.RemoveTagsFromCertificate" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RemoveTagsFromCertificate where
        toJSON RemoveTagsFromCertificate'{..}
          = object
              (catMaybes
                 [Just ("CertificateArn" .= _rtfcCertificateARN),
                  Just ("Tags" .= _rtfcTags)])

instance ToPath RemoveTagsFromCertificate where
        toPath = const "/"

instance ToQuery RemoveTagsFromCertificate where
        toQuery = const mempty

-- | /See:/ 'removeTagsFromCertificateResponse' smart constructor.
data RemoveTagsFromCertificateResponse =
  RemoveTagsFromCertificateResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveTagsFromCertificateResponse' with the minimum fields required to make a request.
--
removeTagsFromCertificateResponse
    :: RemoveTagsFromCertificateResponse
removeTagsFromCertificateResponse = RemoveTagsFromCertificateResponse'


instance NFData RemoveTagsFromCertificateResponse
         where
