{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.RenewalSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RenewalSummary where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.DomainValidationRecord
import Network.AWS.Lightsail.Types.RenewalStatus
import Network.AWS.Prelude

-- | Describes the status of a SSL/TLS certificate renewal managed by Amazon Lightsail.
--
--
--
-- /See:/ 'renewalSummary' smart constructor.
data RenewalSummary = RenewalSummary'
  { _rsRenewalStatus ::
      !(Maybe RenewalStatus),
    _rsDomainValidationRecords ::
      !(Maybe [DomainValidationRecord]),
    _rsUpdatedAt :: !(Maybe POSIX),
    _rsRenewalStatusReason :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RenewalSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsRenewalStatus' - The renewal status of the certificate. The following renewal status are possible:     * __@PendingAutoRenewal@ __ - Lightsail is attempting to automatically validate the domain names of the certificate. No further action is required.      * __@PendingValidation@ __ - Lightsail couldn't automatically validate one or more domain names of the certificate. You must take action to validate these domain names or the certificate won't be renewed. Check to make sure your certificate's domain validation records exist in your domain's DNS, and that your certificate remains in use.     * __@Success@ __ - All domain names in the certificate are validated, and Lightsail renewed the certificate. No further action is required.      * __@Failed@ __ - One or more domain names were not validated before the certificate expired, and Lightsail did not renew the certificate. You can request a new certificate using the @CreateCertificate@ action.
--
-- * 'rsDomainValidationRecords' - An array of objects that describe the domain validation records of the certificate.
--
-- * 'rsUpdatedAt' - The timestamp when the certificate was last updated.
--
-- * 'rsRenewalStatusReason' - The reason for the renewal status of the certificate.
renewalSummary ::
  RenewalSummary
renewalSummary =
  RenewalSummary'
    { _rsRenewalStatus = Nothing,
      _rsDomainValidationRecords = Nothing,
      _rsUpdatedAt = Nothing,
      _rsRenewalStatusReason = Nothing
    }

-- | The renewal status of the certificate. The following renewal status are possible:     * __@PendingAutoRenewal@ __ - Lightsail is attempting to automatically validate the domain names of the certificate. No further action is required.      * __@PendingValidation@ __ - Lightsail couldn't automatically validate one or more domain names of the certificate. You must take action to validate these domain names or the certificate won't be renewed. Check to make sure your certificate's domain validation records exist in your domain's DNS, and that your certificate remains in use.     * __@Success@ __ - All domain names in the certificate are validated, and Lightsail renewed the certificate. No further action is required.      * __@Failed@ __ - One or more domain names were not validated before the certificate expired, and Lightsail did not renew the certificate. You can request a new certificate using the @CreateCertificate@ action.
rsRenewalStatus :: Lens' RenewalSummary (Maybe RenewalStatus)
rsRenewalStatus = lens _rsRenewalStatus (\s a -> s {_rsRenewalStatus = a})

-- | An array of objects that describe the domain validation records of the certificate.
rsDomainValidationRecords :: Lens' RenewalSummary [DomainValidationRecord]
rsDomainValidationRecords = lens _rsDomainValidationRecords (\s a -> s {_rsDomainValidationRecords = a}) . _Default . _Coerce

-- | The timestamp when the certificate was last updated.
rsUpdatedAt :: Lens' RenewalSummary (Maybe UTCTime)
rsUpdatedAt = lens _rsUpdatedAt (\s a -> s {_rsUpdatedAt = a}) . mapping _Time

-- | The reason for the renewal status of the certificate.
rsRenewalStatusReason :: Lens' RenewalSummary (Maybe Text)
rsRenewalStatusReason = lens _rsRenewalStatusReason (\s a -> s {_rsRenewalStatusReason = a})

instance FromJSON RenewalSummary where
  parseJSON =
    withObject
      "RenewalSummary"
      ( \x ->
          RenewalSummary'
            <$> (x .:? "renewalStatus")
            <*> (x .:? "domainValidationRecords" .!= mempty)
            <*> (x .:? "updatedAt")
            <*> (x .:? "renewalStatusReason")
      )

instance Hashable RenewalSummary

instance NFData RenewalSummary
