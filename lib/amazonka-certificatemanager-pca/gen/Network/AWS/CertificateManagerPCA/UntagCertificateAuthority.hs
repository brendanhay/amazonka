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
-- Module      : Network.AWS.CertificateManagerPCA.UntagCertificateAuthority
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove one or more tags from your private CA. A tag consists of a key-value pair. If you do not specify the value portion of the tag when calling this function, the tag will be removed regardless of value. If you specify a value, the tag is removed only if it is associated with the specified value. To add tags to a private CA, use the 'TagCertificateAuthority' . Call the 'ListTags' function to see what tags are associated with your CA.
--
--
module Network.AWS.CertificateManagerPCA.UntagCertificateAuthority
    (
    -- * Creating a Request
      untagCertificateAuthority
    , UntagCertificateAuthority
    -- * Request Lenses
    , uCertificateAuthorityARN
    , uTags

    -- * Destructuring the Response
    , untagCertificateAuthorityResponse
    , UntagCertificateAuthorityResponse
    ) where

import Network.AWS.CertificateManagerPCA.Types
import Network.AWS.CertificateManagerPCA.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'untagCertificateAuthority' smart constructor.
data UntagCertificateAuthority = UntagCertificateAuthority'
  { _uCertificateAuthorityARN :: !Text
  , _uTags                    :: !(List1 Tag)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UntagCertificateAuthority' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uCertificateAuthorityARN' - The Amazon Resource Name (ARN) that was returned when you called 'CreateCertificateAuthority' . This must be of the form:  @arn:aws:acm:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
--
-- * 'uTags' - List of tags to be removed from the CA.
untagCertificateAuthority
    :: Text -- ^ 'uCertificateAuthorityARN'
    -> NonEmpty Tag -- ^ 'uTags'
    -> UntagCertificateAuthority
untagCertificateAuthority pCertificateAuthorityARN_ pTags_ =
  UntagCertificateAuthority'
    { _uCertificateAuthorityARN = pCertificateAuthorityARN_
    , _uTags = _List1 # pTags_
    }


-- | The Amazon Resource Name (ARN) that was returned when you called 'CreateCertificateAuthority' . This must be of the form:  @arn:aws:acm:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
uCertificateAuthorityARN :: Lens' UntagCertificateAuthority Text
uCertificateAuthorityARN = lens _uCertificateAuthorityARN (\ s a -> s{_uCertificateAuthorityARN = a})

-- | List of tags to be removed from the CA.
uTags :: Lens' UntagCertificateAuthority (NonEmpty Tag)
uTags = lens _uTags (\ s a -> s{_uTags = a}) . _List1

instance AWSRequest UntagCertificateAuthority where
        type Rs UntagCertificateAuthority =
             UntagCertificateAuthorityResponse
        request = postJSON certificateManagerPCA
        response
          = receiveNull UntagCertificateAuthorityResponse'

instance Hashable UntagCertificateAuthority where

instance NFData UntagCertificateAuthority where

instance ToHeaders UntagCertificateAuthority where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ACMPrivateCA.UntagCertificateAuthority" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UntagCertificateAuthority where
        toJSON UntagCertificateAuthority'{..}
          = object
              (catMaybes
                 [Just
                    ("CertificateAuthorityArn" .=
                       _uCertificateAuthorityARN),
                  Just ("Tags" .= _uTags)])

instance ToPath UntagCertificateAuthority where
        toPath = const "/"

instance ToQuery UntagCertificateAuthority where
        toQuery = const mempty

-- | /See:/ 'untagCertificateAuthorityResponse' smart constructor.
data UntagCertificateAuthorityResponse =
  UntagCertificateAuthorityResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UntagCertificateAuthorityResponse' with the minimum fields required to make a request.
--
untagCertificateAuthorityResponse
    :: UntagCertificateAuthorityResponse
untagCertificateAuthorityResponse = UntagCertificateAuthorityResponse'


instance NFData UntagCertificateAuthorityResponse
         where
