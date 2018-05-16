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
-- Module      : Network.AWS.CertificateManagerPCA.UpdateCertificateAuthority
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the status or configuration of a private certificate authority (CA). Your private CA must be in the __@ACTIVE@ __ or __@DISABLED@ __ state before you can update it. You can disable a private CA that is in the __@ACTIVE@ __ state or make a CA that is in the __@DISABLED@ __ state active again.
--
--
module Network.AWS.CertificateManagerPCA.UpdateCertificateAuthority
    (
    -- * Creating a Request
      updateCertificateAuthority
    , UpdateCertificateAuthority
    -- * Request Lenses
    , ucaStatus
    , ucaRevocationConfiguration
    , ucaCertificateAuthorityARN

    -- * Destructuring the Response
    , updateCertificateAuthorityResponse
    , UpdateCertificateAuthorityResponse
    ) where

import Network.AWS.CertificateManagerPCA.Types
import Network.AWS.CertificateManagerPCA.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateCertificateAuthority' smart constructor.
data UpdateCertificateAuthority = UpdateCertificateAuthority'
  { _ucaStatus                  :: !(Maybe CertificateAuthorityStatus)
  , _ucaRevocationConfiguration :: !(Maybe RevocationConfiguration)
  , _ucaCertificateAuthorityARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateCertificateAuthority' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucaStatus' - Status of your private CA.
--
-- * 'ucaRevocationConfiguration' - Revocation information for your private CA.
--
-- * 'ucaCertificateAuthorityARN' - Amazon Resource Name (ARN) of the private CA that issued the certificate to be revoked. This must be of the form: @arn:aws:acm:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
updateCertificateAuthority
    :: Text -- ^ 'ucaCertificateAuthorityARN'
    -> UpdateCertificateAuthority
updateCertificateAuthority pCertificateAuthorityARN_ =
  UpdateCertificateAuthority'
    { _ucaStatus = Nothing
    , _ucaRevocationConfiguration = Nothing
    , _ucaCertificateAuthorityARN = pCertificateAuthorityARN_
    }


-- | Status of your private CA.
ucaStatus :: Lens' UpdateCertificateAuthority (Maybe CertificateAuthorityStatus)
ucaStatus = lens _ucaStatus (\ s a -> s{_ucaStatus = a})

-- | Revocation information for your private CA.
ucaRevocationConfiguration :: Lens' UpdateCertificateAuthority (Maybe RevocationConfiguration)
ucaRevocationConfiguration = lens _ucaRevocationConfiguration (\ s a -> s{_ucaRevocationConfiguration = a})

-- | Amazon Resource Name (ARN) of the private CA that issued the certificate to be revoked. This must be of the form: @arn:aws:acm:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
ucaCertificateAuthorityARN :: Lens' UpdateCertificateAuthority Text
ucaCertificateAuthorityARN = lens _ucaCertificateAuthorityARN (\ s a -> s{_ucaCertificateAuthorityARN = a})

instance AWSRequest UpdateCertificateAuthority where
        type Rs UpdateCertificateAuthority =
             UpdateCertificateAuthorityResponse
        request = postJSON certificateManagerPCA
        response
          = receiveNull UpdateCertificateAuthorityResponse'

instance Hashable UpdateCertificateAuthority where

instance NFData UpdateCertificateAuthority where

instance ToHeaders UpdateCertificateAuthority where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ACMPrivateCA.UpdateCertificateAuthority" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateCertificateAuthority where
        toJSON UpdateCertificateAuthority'{..}
          = object
              (catMaybes
                 [("Status" .=) <$> _ucaStatus,
                  ("RevocationConfiguration" .=) <$>
                    _ucaRevocationConfiguration,
                  Just
                    ("CertificateAuthorityArn" .=
                       _ucaCertificateAuthorityARN)])

instance ToPath UpdateCertificateAuthority where
        toPath = const "/"

instance ToQuery UpdateCertificateAuthority where
        toQuery = const mempty

-- | /See:/ 'updateCertificateAuthorityResponse' smart constructor.
data UpdateCertificateAuthorityResponse =
  UpdateCertificateAuthorityResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateCertificateAuthorityResponse' with the minimum fields required to make a request.
--
updateCertificateAuthorityResponse
    :: UpdateCertificateAuthorityResponse
updateCertificateAuthorityResponse = UpdateCertificateAuthorityResponse'


instance NFData UpdateCertificateAuthorityResponse
         where
