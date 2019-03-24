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
-- Deletes a private certificate authority (CA). You must provide the ARN (Amazon Resource Name) of the private CA that you want to delete. You can find the ARN by calling the 'ListCertificateAuthorities' operation. Before you can delete a CA, you must disable it. Call the 'UpdateCertificateAuthority' operation and set the __CertificateAuthorityStatus__ parameter to @DISABLED@ .
--
--
-- Additionally, you can delete a CA if you are waiting for it to be created (the __Status__ field of the 'CertificateAuthority' is @CREATING@ ). You can also delete it if the CA has been created but you haven't yet imported the signed certificate (the __Status__ is @PENDING_CERTIFICATE@ ) into ACM PCA.
--
-- If the CA is in one of the previously mentioned states and you call 'DeleteCertificateAuthority' , the CA's status changes to @DELETED@ . However, the CA won't be permanently deleted until the restoration period has passed. By default, if you do not set the @PermanentDeletionTimeInDays@ parameter, the CA remains restorable for 30 days. You can set the parameter from 7 to 30 days. The 'DescribeCertificateAuthority' operation returns the time remaining in the restoration window of a Private CA in the @DELETED@ state. To restore an eligible CA, call the 'RestoreCertificateAuthority' operation.
--
module Network.AWS.CertificateManagerPCA.DeleteCertificateAuthority
    (
    -- * Creating a Request
      deleteCertificateAuthority
    , DeleteCertificateAuthority
    -- * Request Lenses
    , dcaPermanentDeletionTimeInDays
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
data DeleteCertificateAuthority = DeleteCertificateAuthority'
  { _dcaPermanentDeletionTimeInDays :: !(Maybe Nat)
  , _dcaCertificateAuthorityARN     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteCertificateAuthority' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcaPermanentDeletionTimeInDays' - The number of days to make a CA restorable after it has been deleted. This can be anywhere from 7 to 30 days, with 30 being the default.
--
-- * 'dcaCertificateAuthorityARN' - The Amazon Resource Name (ARN) that was returned when you called 'CreateCertificateAuthority' . This must have the following form:  @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
deleteCertificateAuthority
    :: Text -- ^ 'dcaCertificateAuthorityARN'
    -> DeleteCertificateAuthority
deleteCertificateAuthority pCertificateAuthorityARN_ =
  DeleteCertificateAuthority'
    { _dcaPermanentDeletionTimeInDays = Nothing
    , _dcaCertificateAuthorityARN = pCertificateAuthorityARN_
    }


-- | The number of days to make a CA restorable after it has been deleted. This can be anywhere from 7 to 30 days, with 30 being the default.
dcaPermanentDeletionTimeInDays :: Lens' DeleteCertificateAuthority (Maybe Natural)
dcaPermanentDeletionTimeInDays = lens _dcaPermanentDeletionTimeInDays (\ s a -> s{_dcaPermanentDeletionTimeInDays = a}) . mapping _Nat

-- | The Amazon Resource Name (ARN) that was returned when you called 'CreateCertificateAuthority' . This must have the following form:  @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
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
                 [("PermanentDeletionTimeInDays" .=) <$>
                    _dcaPermanentDeletionTimeInDays,
                  Just
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
