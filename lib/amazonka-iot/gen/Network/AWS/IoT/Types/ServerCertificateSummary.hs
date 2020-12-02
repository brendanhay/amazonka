{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ServerCertificateSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ServerCertificateSummary where

import Network.AWS.IoT.Types.ServerCertificateStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object that contains information about a server certificate.
--
--
--
-- /See:/ 'serverCertificateSummary' smart constructor.
data ServerCertificateSummary = ServerCertificateSummary'
  { _scsServerCertificateStatusDetail ::
      !(Maybe Text),
    _scsServerCertificateStatus ::
      !(Maybe ServerCertificateStatus),
    _scsServerCertificateARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ServerCertificateSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scsServerCertificateStatusDetail' - Details that explain the status of the server certificate.
--
-- * 'scsServerCertificateStatus' - The status of the server certificate.
--
-- * 'scsServerCertificateARN' - The ARN of the server certificate.
serverCertificateSummary ::
  ServerCertificateSummary
serverCertificateSummary =
  ServerCertificateSummary'
    { _scsServerCertificateStatusDetail =
        Nothing,
      _scsServerCertificateStatus = Nothing,
      _scsServerCertificateARN = Nothing
    }

-- | Details that explain the status of the server certificate.
scsServerCertificateStatusDetail :: Lens' ServerCertificateSummary (Maybe Text)
scsServerCertificateStatusDetail = lens _scsServerCertificateStatusDetail (\s a -> s {_scsServerCertificateStatusDetail = a})

-- | The status of the server certificate.
scsServerCertificateStatus :: Lens' ServerCertificateSummary (Maybe ServerCertificateStatus)
scsServerCertificateStatus = lens _scsServerCertificateStatus (\s a -> s {_scsServerCertificateStatus = a})

-- | The ARN of the server certificate.
scsServerCertificateARN :: Lens' ServerCertificateSummary (Maybe Text)
scsServerCertificateARN = lens _scsServerCertificateARN (\s a -> s {_scsServerCertificateARN = a})

instance FromJSON ServerCertificateSummary where
  parseJSON =
    withObject
      "ServerCertificateSummary"
      ( \x ->
          ServerCertificateSummary'
            <$> (x .:? "serverCertificateStatusDetail")
            <*> (x .:? "serverCertificateStatus")
            <*> (x .:? "serverCertificateArn")
      )

instance Hashable ServerCertificateSummary

instance NFData ServerCertificateSummary
