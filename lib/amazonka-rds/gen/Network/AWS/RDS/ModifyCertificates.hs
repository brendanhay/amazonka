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
-- Module      : Network.AWS.RDS.ModifyCertificates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Override the system-default Secure Sockets Layer/Transport Layer Security (SSL/TLS) certificate for Amazon RDS for new DB instances temporarily, or remove the override.
--
--
-- By using this operation, you can specify an RDS-approved SSL/TLS certificate for new DB instances that is different from the default certificate provided by RDS. You can also use this operation to remove the override, so that new DB instances use the default certificate provided by RDS.
--
-- You might need to override the default certificate in the following situations:
--
--     * You already migrated your applications to support the latest certificate authority (CA) certificate, but the new CA certificate is not yet the RDS default CA certificate for the specified AWS Region.
--
--     * RDS has already moved to a new default CA certificate for the specified AWS Region, but you are still in the process of supporting the new CA certificate. In this case, you temporarily need additional time to finish your application changes.
--
--
--
-- For more information about rotating your SSL/TLS certificate for RDS DB engines, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.SSL-certificate-rotation.html Rotating Your SSL/TLS Certificate> in the /Amazon RDS User Guide/ .
--
-- For more information about rotating your SSL/TLS certificate for Aurora DB engines, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.SSL-certificate-rotation.html Rotating Your SSL/TLS Certificate> in the /Amazon Aurora User Guide/ .
module Network.AWS.RDS.ModifyCertificates
  ( -- * Creating a Request
    modifyCertificates,
    ModifyCertificates,

    -- * Request Lenses
    mcCertificateIdentifier,
    mcRemoveCustomerOverride,

    -- * Destructuring the Response
    modifyCertificatesResponse,
    ModifyCertificatesResponse,

    -- * Response Lenses
    mcrsCertificate,
    mcrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifyCertificates' smart constructor.
data ModifyCertificates = ModifyCertificates'
  { _mcCertificateIdentifier ::
      !(Maybe Text),
    _mcRemoveCustomerOverride :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyCertificates' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mcCertificateIdentifier' - The new default certificate identifier to override the current one with. To determine the valid values, use the @describe-certificates@ AWS CLI command or the @DescribeCertificates@ API operation.
--
-- * 'mcRemoveCustomerOverride' - A value that indicates whether to remove the override for the default certificate. If the override is removed, the default certificate is the system default.
modifyCertificates ::
  ModifyCertificates
modifyCertificates =
  ModifyCertificates'
    { _mcCertificateIdentifier = Nothing,
      _mcRemoveCustomerOverride = Nothing
    }

-- | The new default certificate identifier to override the current one with. To determine the valid values, use the @describe-certificates@ AWS CLI command or the @DescribeCertificates@ API operation.
mcCertificateIdentifier :: Lens' ModifyCertificates (Maybe Text)
mcCertificateIdentifier = lens _mcCertificateIdentifier (\s a -> s {_mcCertificateIdentifier = a})

-- | A value that indicates whether to remove the override for the default certificate. If the override is removed, the default certificate is the system default.
mcRemoveCustomerOverride :: Lens' ModifyCertificates (Maybe Bool)
mcRemoveCustomerOverride = lens _mcRemoveCustomerOverride (\s a -> s {_mcRemoveCustomerOverride = a})

instance AWSRequest ModifyCertificates where
  type Rs ModifyCertificates = ModifyCertificatesResponse
  request = postQuery rds
  response =
    receiveXMLWrapper
      "ModifyCertificatesResult"
      ( \s h x ->
          ModifyCertificatesResponse'
            <$> (x .@? "Certificate") <*> (pure (fromEnum s))
      )

instance Hashable ModifyCertificates

instance NFData ModifyCertificates

instance ToHeaders ModifyCertificates where
  toHeaders = const mempty

instance ToPath ModifyCertificates where
  toPath = const "/"

instance ToQuery ModifyCertificates where
  toQuery ModifyCertificates' {..} =
    mconcat
      [ "Action" =: ("ModifyCertificates" :: ByteString),
        "Version" =: ("2014-10-31" :: ByteString),
        "CertificateIdentifier" =: _mcCertificateIdentifier,
        "RemoveCustomerOverride" =: _mcRemoveCustomerOverride
      ]

-- | /See:/ 'modifyCertificatesResponse' smart constructor.
data ModifyCertificatesResponse = ModifyCertificatesResponse'
  { _mcrsCertificate ::
      !(Maybe Certificate),
    _mcrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyCertificatesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mcrsCertificate' - Undocumented member.
--
-- * 'mcrsResponseStatus' - -- | The response status code.
modifyCertificatesResponse ::
  -- | 'mcrsResponseStatus'
  Int ->
  ModifyCertificatesResponse
modifyCertificatesResponse pResponseStatus_ =
  ModifyCertificatesResponse'
    { _mcrsCertificate = Nothing,
      _mcrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
mcrsCertificate :: Lens' ModifyCertificatesResponse (Maybe Certificate)
mcrsCertificate = lens _mcrsCertificate (\s a -> s {_mcrsCertificate = a})

-- | -- | The response status code.
mcrsResponseStatus :: Lens' ModifyCertificatesResponse Int
mcrsResponseStatus = lens _mcrsResponseStatus (\s a -> s {_mcrsResponseStatus = a})

instance NFData ModifyCertificatesResponse
