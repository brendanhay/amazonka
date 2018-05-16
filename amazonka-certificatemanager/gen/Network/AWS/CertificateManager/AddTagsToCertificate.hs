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
-- Module      : Network.AWS.CertificateManager.AddTagsToCertificate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more tags to an ACM certificate. Tags are labels that you can use to identify and organize your AWS resources. Each tag consists of a @key@ and an optional @value@ . You specify the certificate on input by its Amazon Resource Name (ARN). You specify the tag by using a key-value pair.
--
--
-- You can apply a tag to just one certificate if you want to identify a specific characteristic of that certificate, or you can apply the same tag to multiple certificates if you want to filter for a common relationship among those certificates. Similarly, you can apply the same tag to multiple resources if you want to specify a relationship among those resources. For example, you can add the same tag to an ACM certificate and an Elastic Load Balancing load balancer to indicate that they are both used by the same website. For more information, see <http://docs.aws.amazon.com/acm/latest/userguide/tags.html Tagging ACM certificates> .
--
-- To remove one or more tags, use the 'RemoveTagsFromCertificate' action. To view all of the tags that have been applied to the certificate, use the 'ListTagsForCertificate' action.
--
module Network.AWS.CertificateManager.AddTagsToCertificate
    (
    -- * Creating a Request
      addTagsToCertificate
    , AddTagsToCertificate
    -- * Request Lenses
    , attcCertificateARN
    , attcTags

    -- * Destructuring the Response
    , addTagsToCertificateResponse
    , AddTagsToCertificateResponse
    ) where

import Network.AWS.CertificateManager.Types
import Network.AWS.CertificateManager.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'addTagsToCertificate' smart constructor.
data AddTagsToCertificate = AddTagsToCertificate'
  { _attcCertificateARN :: !Text
  , _attcTags           :: !(List1 Tag)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddTagsToCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'attcCertificateARN' - String that contains the ARN of the ACM certificate to which the tag is to be applied. This must be of the form: @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@  For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- * 'attcTags' - The key-value pair that defines the tag. The tag value is optional.
addTagsToCertificate
    :: Text -- ^ 'attcCertificateARN'
    -> NonEmpty Tag -- ^ 'attcTags'
    -> AddTagsToCertificate
addTagsToCertificate pCertificateARN_ pTags_ =
  AddTagsToCertificate'
    {_attcCertificateARN = pCertificateARN_, _attcTags = _List1 # pTags_}


-- | String that contains the ARN of the ACM certificate to which the tag is to be applied. This must be of the form: @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@  For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
attcCertificateARN :: Lens' AddTagsToCertificate Text
attcCertificateARN = lens _attcCertificateARN (\ s a -> s{_attcCertificateARN = a})

-- | The key-value pair that defines the tag. The tag value is optional.
attcTags :: Lens' AddTagsToCertificate (NonEmpty Tag)
attcTags = lens _attcTags (\ s a -> s{_attcTags = a}) . _List1

instance AWSRequest AddTagsToCertificate where
        type Rs AddTagsToCertificate =
             AddTagsToCertificateResponse
        request = postJSON certificateManager
        response = receiveNull AddTagsToCertificateResponse'

instance Hashable AddTagsToCertificate where

instance NFData AddTagsToCertificate where

instance ToHeaders AddTagsToCertificate where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CertificateManager.AddTagsToCertificate" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AddTagsToCertificate where
        toJSON AddTagsToCertificate'{..}
          = object
              (catMaybes
                 [Just ("CertificateArn" .= _attcCertificateARN),
                  Just ("Tags" .= _attcTags)])

instance ToPath AddTagsToCertificate where
        toPath = const "/"

instance ToQuery AddTagsToCertificate where
        toQuery = const mempty

-- | /See:/ 'addTagsToCertificateResponse' smart constructor.
data AddTagsToCertificateResponse =
  AddTagsToCertificateResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddTagsToCertificateResponse' with the minimum fields required to make a request.
--
addTagsToCertificateResponse
    :: AddTagsToCertificateResponse
addTagsToCertificateResponse = AddTagsToCertificateResponse'


instance NFData AddTagsToCertificateResponse where
