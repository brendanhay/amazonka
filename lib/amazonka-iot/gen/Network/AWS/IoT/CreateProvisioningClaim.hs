{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateProvisioningClaim
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a provisioning claim.
module Network.AWS.IoT.CreateProvisioningClaim
  ( -- * Creating a Request
    createProvisioningClaim,
    CreateProvisioningClaim,

    -- * Request Lenses
    cpcTemplateName,

    -- * Destructuring the Response
    createProvisioningClaimResponse,
    CreateProvisioningClaimResponse,

    -- * Response Lenses
    cpcrsKeyPair,
    cpcrsCertificatePem,
    cpcrsCertificateId,
    cpcrsExpiration,
    cpcrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createProvisioningClaim' smart constructor.
newtype CreateProvisioningClaim = CreateProvisioningClaim'
  { _cpcTemplateName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateProvisioningClaim' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpcTemplateName' - The name of the provisioning template to use.
createProvisioningClaim ::
  -- | 'cpcTemplateName'
  Text ->
  CreateProvisioningClaim
createProvisioningClaim pTemplateName_ =
  CreateProvisioningClaim' {_cpcTemplateName = pTemplateName_}

-- | The name of the provisioning template to use.
cpcTemplateName :: Lens' CreateProvisioningClaim Text
cpcTemplateName = lens _cpcTemplateName (\s a -> s {_cpcTemplateName = a})

instance AWSRequest CreateProvisioningClaim where
  type Rs CreateProvisioningClaim = CreateProvisioningClaimResponse
  request = postJSON ioT
  response =
    receiveJSON
      ( \s h x ->
          CreateProvisioningClaimResponse'
            <$> (x .?> "keyPair")
            <*> (x .?> "certificatePem")
            <*> (x .?> "certificateId")
            <*> (x .?> "expiration")
            <*> (pure (fromEnum s))
      )

instance Hashable CreateProvisioningClaim

instance NFData CreateProvisioningClaim

instance ToHeaders CreateProvisioningClaim where
  toHeaders = const mempty

instance ToJSON CreateProvisioningClaim where
  toJSON = const (Object mempty)

instance ToPath CreateProvisioningClaim where
  toPath CreateProvisioningClaim' {..} =
    mconcat
      [ "/provisioning-templates/",
        toBS _cpcTemplateName,
        "/provisioning-claim"
      ]

instance ToQuery CreateProvisioningClaim where
  toQuery = const mempty

-- | /See:/ 'createProvisioningClaimResponse' smart constructor.
data CreateProvisioningClaimResponse = CreateProvisioningClaimResponse'
  { _cpcrsKeyPair ::
      !(Maybe KeyPair),
    _cpcrsCertificatePem ::
      !(Maybe Text),
    _cpcrsCertificateId ::
      !(Maybe Text),
    _cpcrsExpiration ::
      !(Maybe POSIX),
    _cpcrsResponseStatus ::
      !Int
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateProvisioningClaimResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpcrsKeyPair' - The provisioning claim key pair.
--
-- * 'cpcrsCertificatePem' - The provisioning claim certificate.
--
-- * 'cpcrsCertificateId' - The ID of the certificate.
--
-- * 'cpcrsExpiration' - The provisioning claim expiration time.
--
-- * 'cpcrsResponseStatus' - -- | The response status code.
createProvisioningClaimResponse ::
  -- | 'cpcrsResponseStatus'
  Int ->
  CreateProvisioningClaimResponse
createProvisioningClaimResponse pResponseStatus_ =
  CreateProvisioningClaimResponse'
    { _cpcrsKeyPair = Nothing,
      _cpcrsCertificatePem = Nothing,
      _cpcrsCertificateId = Nothing,
      _cpcrsExpiration = Nothing,
      _cpcrsResponseStatus = pResponseStatus_
    }

-- | The provisioning claim key pair.
cpcrsKeyPair :: Lens' CreateProvisioningClaimResponse (Maybe KeyPair)
cpcrsKeyPair = lens _cpcrsKeyPair (\s a -> s {_cpcrsKeyPair = a})

-- | The provisioning claim certificate.
cpcrsCertificatePem :: Lens' CreateProvisioningClaimResponse (Maybe Text)
cpcrsCertificatePem = lens _cpcrsCertificatePem (\s a -> s {_cpcrsCertificatePem = a})

-- | The ID of the certificate.
cpcrsCertificateId :: Lens' CreateProvisioningClaimResponse (Maybe Text)
cpcrsCertificateId = lens _cpcrsCertificateId (\s a -> s {_cpcrsCertificateId = a})

-- | The provisioning claim expiration time.
cpcrsExpiration :: Lens' CreateProvisioningClaimResponse (Maybe UTCTime)
cpcrsExpiration = lens _cpcrsExpiration (\s a -> s {_cpcrsExpiration = a}) . mapping _Time

-- | -- | The response status code.
cpcrsResponseStatus :: Lens' CreateProvisioningClaimResponse Int
cpcrsResponseStatus = lens _cpcrsResponseStatus (\s a -> s {_cpcrsResponseStatus = a})

instance NFData CreateProvisioningClaimResponse
