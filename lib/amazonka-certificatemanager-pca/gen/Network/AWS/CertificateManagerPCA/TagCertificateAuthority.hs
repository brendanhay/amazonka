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
-- Module      : Network.AWS.CertificateManagerPCA.TagCertificateAuthority
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more tags to your private CA. Tags are labels that you can use to identify and organize your AWS resources. Each tag consists of a key and an optional value. You specify the private CA on input by its Amazon Resource Name (ARN). You specify the tag by using a key-value pair. You can apply a tag to just one private CA if you want to identify a specific characteristic of that CA, or you can apply the same tag to multiple private CAs if you want to filter for a common relationship among those CAs. To remove one or more tags, use the 'UntagCertificateAuthority' function. Call the 'ListTags' function to see what tags are associated with your CA.
--
--
module Network.AWS.CertificateManagerPCA.TagCertificateAuthority
    (
    -- * Creating a Request
      tagCertificateAuthority
    , TagCertificateAuthority
    -- * Request Lenses
    , tcaCertificateAuthorityARN
    , tcaTags

    -- * Destructuring the Response
    , tagCertificateAuthorityResponse
    , TagCertificateAuthorityResponse
    ) where

import Network.AWS.CertificateManagerPCA.Types
import Network.AWS.CertificateManagerPCA.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'tagCertificateAuthority' smart constructor.
data TagCertificateAuthority = TagCertificateAuthority'
  { _tcaCertificateAuthorityARN :: !Text
  , _tcaTags                    :: !(List1 Tag)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TagCertificateAuthority' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tcaCertificateAuthorityARN' - The Amazon Resource Name (ARN) that was returned when you called 'CreateCertificateAuthority' . This must be of the form:  @arn:aws:acm:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
--
-- * 'tcaTags' - List of tags to be associated with the CA.
tagCertificateAuthority
    :: Text -- ^ 'tcaCertificateAuthorityARN'
    -> NonEmpty Tag -- ^ 'tcaTags'
    -> TagCertificateAuthority
tagCertificateAuthority pCertificateAuthorityARN_ pTags_ =
  TagCertificateAuthority'
    { _tcaCertificateAuthorityARN = pCertificateAuthorityARN_
    , _tcaTags = _List1 # pTags_
    }


-- | The Amazon Resource Name (ARN) that was returned when you called 'CreateCertificateAuthority' . This must be of the form:  @arn:aws:acm:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
tcaCertificateAuthorityARN :: Lens' TagCertificateAuthority Text
tcaCertificateAuthorityARN = lens _tcaCertificateAuthorityARN (\ s a -> s{_tcaCertificateAuthorityARN = a})

-- | List of tags to be associated with the CA.
tcaTags :: Lens' TagCertificateAuthority (NonEmpty Tag)
tcaTags = lens _tcaTags (\ s a -> s{_tcaTags = a}) . _List1

instance AWSRequest TagCertificateAuthority where
        type Rs TagCertificateAuthority =
             TagCertificateAuthorityResponse
        request = postJSON certificateManagerPCA
        response
          = receiveNull TagCertificateAuthorityResponse'

instance Hashable TagCertificateAuthority where

instance NFData TagCertificateAuthority where

instance ToHeaders TagCertificateAuthority where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ACMPrivateCA.TagCertificateAuthority" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON TagCertificateAuthority where
        toJSON TagCertificateAuthority'{..}
          = object
              (catMaybes
                 [Just
                    ("CertificateAuthorityArn" .=
                       _tcaCertificateAuthorityARN),
                  Just ("Tags" .= _tcaTags)])

instance ToPath TagCertificateAuthority where
        toPath = const "/"

instance ToQuery TagCertificateAuthority where
        toQuery = const mempty

-- | /See:/ 'tagCertificateAuthorityResponse' smart constructor.
data TagCertificateAuthorityResponse =
  TagCertificateAuthorityResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TagCertificateAuthorityResponse' with the minimum fields required to make a request.
--
tagCertificateAuthorityResponse
    :: TagCertificateAuthorityResponse
tagCertificateAuthorityResponse = TagCertificateAuthorityResponse'


instance NFData TagCertificateAuthorityResponse where
