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
-- Module      : Network.AWS.CertificateManagerPCA.DeleteCertificateAuthority
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the private certificate authority (CA) that you created or started to create by calling the 'CreateCertificateAuthority' function. This action requires that you enter an ARN (Amazon Resource Name) for the private CA that you want to delete. You can find the ARN by calling the 'ListCertificateAuthorities' function. You can delete the CA if you are waiting for it to be created (the __Status__ field of the 'CertificateAuthority' is @CREATING@ ) or if the CA has been created but you haven't yet imported the signed certificate (the __Status__ is @PENDING_CERTIFICATE@ ) into ACM PCA. If you've already imported the certificate, you cannot delete the CA unless it has been disabled for more than 30 days. To disable a CA, call the 'UpdateCertificateAuthority' function and set the __CertificateAuthorityStatus__ argument to @DISABLED@ .
--
--
module Network.AWS.CertificateManagerPCA.DeleteCertificateAuthority
    (
    -- * Creating a Request
      deleteCertificateAuthority
    , DeleteCertificateAuthority
    -- * Request Lenses
    , dcaCertificateAuthorityARN

    -- * Destructuring the Response
    , deleteCertificateAuthorityResponse
    , DeleteCertificateAuthorityResponse
    ) where

import Network.AWS.CertificateManagerPCA.Types
import Network.AWS.CertificateManagerPCA.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteCertificateAuthority' smart constructor.
newtype DeleteCertificateAuthority = DeleteCertificateAuthority'
  { _dcaCertificateAuthorityARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteCertificateAuthority' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcaCertificateAuthorityARN' - The Amazon Resource Name (ARN) that was returned when you called 'CreateCertificateAuthority' . This must be of the form:  @arn:aws:acm:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
deleteCertificateAuthority
    :: Text -- ^ 'dcaCertificateAuthorityARN'
    -> DeleteCertificateAuthority
deleteCertificateAuthority pCertificateAuthorityARN_ =
  DeleteCertificateAuthority'
    {_dcaCertificateAuthorityARN = pCertificateAuthorityARN_}


-- | The Amazon Resource Name (ARN) that was returned when you called 'CreateCertificateAuthority' . This must be of the form:  @arn:aws:acm:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
dcaCertificateAuthorityARN :: Lens' DeleteCertificateAuthority Text
dcaCertificateAuthorityARN = lens _dcaCertificateAuthorityARN (\ s a -> s{_dcaCertificateAuthorityARN = a})

instance AWSRequest DeleteCertificateAuthority where
        type Rs DeleteCertificateAuthority =
             DeleteCertificateAuthorityResponse
        request = postJSON certificateManagerPCA
        response
          = receiveNull DeleteCertificateAuthorityResponse'

instance Hashable DeleteCertificateAuthority where

instance NFData DeleteCertificateAuthority where

instance ToHeaders DeleteCertificateAuthority where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ACMPrivateCA.DeleteCertificateAuthority" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteCertificateAuthority where
        toJSON DeleteCertificateAuthority'{..}
          = object
              (catMaybes
                 [Just
                    ("CertificateAuthorityArn" .=
                       _dcaCertificateAuthorityARN)])

instance ToPath DeleteCertificateAuthority where
        toPath = const "/"

instance ToQuery DeleteCertificateAuthority where
        toQuery = const mempty

-- | /See:/ 'deleteCertificateAuthorityResponse' smart constructor.
data DeleteCertificateAuthorityResponse =
  DeleteCertificateAuthorityResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteCertificateAuthorityResponse' with the minimum fields required to make a request.
--
deleteCertificateAuthorityResponse
    :: DeleteCertificateAuthorityResponse
deleteCertificateAuthorityResponse = DeleteCertificateAuthorityResponse'


instance NFData DeleteCertificateAuthorityResponse
         where
