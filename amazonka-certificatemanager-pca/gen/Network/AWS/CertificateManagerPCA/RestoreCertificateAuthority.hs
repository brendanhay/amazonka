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
-- Module      : Network.AWS.CertificateManagerPCA.RestoreCertificateAuthority
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores a certificate authority (CA) that is in the @DELETED@ state. You can restore a CA during the period that you defined in the __PermanentDeletionTimeInDays__ parameter of the 'DeleteCertificateAuthority' operation. Currently, you can specify 7 to 30 days. If you did not specify a __PermanentDeletionTimeInDays__ value, by default you can restore the CA at any time in a 30 day period. You can check the time remaining in the restoration period of a private CA in the @DELETED@ state by calling the 'DescribeCertificateAuthority' or 'ListCertificateAuthorities' operations. The status of a restored CA is set to its pre-deletion status when the __RestoreCertificateAuthority__ operation returns. To change its status to @ACTIVE@ , call the 'UpdateCertificateAuthority' operation. If the private CA was in the @PENDING_CERTIFICATE@ state at deletion, you must use the 'ImportCertificateAuthorityCertificate' operation to import a certificate authority into the private CA before it can be activated. You cannot restore a CA after the restoration period has ended.
--
--
module Network.AWS.CertificateManagerPCA.RestoreCertificateAuthority
    (
    -- * Creating a Request
      restoreCertificateAuthority
    , RestoreCertificateAuthority
    -- * Request Lenses
    , rcaCertificateAuthorityARN

    -- * Destructuring the Response
    , restoreCertificateAuthorityResponse
    , RestoreCertificateAuthorityResponse
    ) where

import Network.AWS.CertificateManagerPCA.Types
import Network.AWS.CertificateManagerPCA.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'restoreCertificateAuthority' smart constructor.
newtype RestoreCertificateAuthority = RestoreCertificateAuthority'
  { _rcaCertificateAuthorityARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RestoreCertificateAuthority' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcaCertificateAuthorityARN' - The Amazon Resource Name (ARN) that was returned when you called the 'CreateCertificateAuthority' operation. This must be of the form:  @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
restoreCertificateAuthority
    :: Text -- ^ 'rcaCertificateAuthorityARN'
    -> RestoreCertificateAuthority
restoreCertificateAuthority pCertificateAuthorityARN_ =
  RestoreCertificateAuthority'
    {_rcaCertificateAuthorityARN = pCertificateAuthorityARN_}


-- | The Amazon Resource Name (ARN) that was returned when you called the 'CreateCertificateAuthority' operation. This must be of the form:  @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
rcaCertificateAuthorityARN :: Lens' RestoreCertificateAuthority Text
rcaCertificateAuthorityARN = lens _rcaCertificateAuthorityARN (\ s a -> s{_rcaCertificateAuthorityARN = a})

instance AWSRequest RestoreCertificateAuthority where
        type Rs RestoreCertificateAuthority =
             RestoreCertificateAuthorityResponse
        request = postJSON certificateManagerPCA
        response
          = receiveNull RestoreCertificateAuthorityResponse'

instance Hashable RestoreCertificateAuthority where

instance NFData RestoreCertificateAuthority where

instance ToHeaders RestoreCertificateAuthority where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ACMPrivateCA.RestoreCertificateAuthority" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RestoreCertificateAuthority where
        toJSON RestoreCertificateAuthority'{..}
          = object
              (catMaybes
                 [Just
                    ("CertificateAuthorityArn" .=
                       _rcaCertificateAuthorityARN)])

instance ToPath RestoreCertificateAuthority where
        toPath = const "/"

instance ToQuery RestoreCertificateAuthority where
        toQuery = const mempty

-- | /See:/ 'restoreCertificateAuthorityResponse' smart constructor.
data RestoreCertificateAuthorityResponse =
  RestoreCertificateAuthorityResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RestoreCertificateAuthorityResponse' with the minimum fields required to make a request.
--
restoreCertificateAuthorityResponse
    :: RestoreCertificateAuthorityResponse
restoreCertificateAuthorityResponse = RestoreCertificateAuthorityResponse'


instance NFData RestoreCertificateAuthorityResponse
         where
