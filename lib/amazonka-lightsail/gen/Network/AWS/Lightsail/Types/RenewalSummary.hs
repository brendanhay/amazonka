-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.RenewalSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RenewalSummary
  ( RenewalSummary (..),

    -- * Smart constructor
    mkRenewalSummary,

    -- * Lenses
    rsRenewalStatus,
    rsDomainValidationRecords,
    rsUpdatedAt,
    rsRenewalStatusReason,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.DomainValidationRecord
import Network.AWS.Lightsail.Types.RenewalStatus
import qualified Network.AWS.Prelude as Lude

-- | Describes the status of a SSL/TLS certificate renewal managed by Amazon Lightsail.
--
-- /See:/ 'mkRenewalSummary' smart constructor.
data RenewalSummary = RenewalSummary'
  { renewalStatus ::
      Lude.Maybe RenewalStatus,
    domainValidationRecords ::
      Lude.Maybe [DomainValidationRecord],
    updatedAt :: Lude.Maybe Lude.Timestamp,
    renewalStatusReason :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RenewalSummary' with the minimum fields required to make a request.
--
-- * 'domainValidationRecords' - An array of objects that describe the domain validation records of the certificate.
-- * 'renewalStatus' - The renewal status of the certificate.
--
-- The following renewal status are possible:
--
--     * __@PendingAutoRenewal@ __ - Lightsail is attempting to automatically validate the domain names of the certificate. No further action is required.
--
--
--     * __@PendingValidation@ __ - Lightsail couldn't automatically validate one or more domain names of the certificate. You must take action to validate these domain names or the certificate won't be renewed. Check to make sure your certificate's domain validation records exist in your domain's DNS, and that your certificate remains in use.
--
--
--     * __@Success@ __ - All domain names in the certificate are validated, and Lightsail renewed the certificate. No further action is required.
--
--
--     * __@Failed@ __ - One or more domain names were not validated before the certificate expired, and Lightsail did not renew the certificate. You can request a new certificate using the @CreateCertificate@ action.
--
--
-- * 'renewalStatusReason' - The reason for the renewal status of the certificate.
-- * 'updatedAt' - The timestamp when the certificate was last updated.
mkRenewalSummary ::
  RenewalSummary
mkRenewalSummary =
  RenewalSummary'
    { renewalStatus = Lude.Nothing,
      domainValidationRecords = Lude.Nothing,
      updatedAt = Lude.Nothing,
      renewalStatusReason = Lude.Nothing
    }

-- | The renewal status of the certificate.
--
-- The following renewal status are possible:
--
--     * __@PendingAutoRenewal@ __ - Lightsail is attempting to automatically validate the domain names of the certificate. No further action is required.
--
--
--     * __@PendingValidation@ __ - Lightsail couldn't automatically validate one or more domain names of the certificate. You must take action to validate these domain names or the certificate won't be renewed. Check to make sure your certificate's domain validation records exist in your domain's DNS, and that your certificate remains in use.
--
--
--     * __@Success@ __ - All domain names in the certificate are validated, and Lightsail renewed the certificate. No further action is required.
--
--
--     * __@Failed@ __ - One or more domain names were not validated before the certificate expired, and Lightsail did not renew the certificate. You can request a new certificate using the @CreateCertificate@ action.
--
--
--
-- /Note:/ Consider using 'renewalStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsRenewalStatus :: Lens.Lens' RenewalSummary (Lude.Maybe RenewalStatus)
rsRenewalStatus = Lens.lens (renewalStatus :: RenewalSummary -> Lude.Maybe RenewalStatus) (\s a -> s {renewalStatus = a} :: RenewalSummary)
{-# DEPRECATED rsRenewalStatus "Use generic-lens or generic-optics with 'renewalStatus' instead." #-}

-- | An array of objects that describe the domain validation records of the certificate.
--
-- /Note:/ Consider using 'domainValidationRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsDomainValidationRecords :: Lens.Lens' RenewalSummary (Lude.Maybe [DomainValidationRecord])
rsDomainValidationRecords = Lens.lens (domainValidationRecords :: RenewalSummary -> Lude.Maybe [DomainValidationRecord]) (\s a -> s {domainValidationRecords = a} :: RenewalSummary)
{-# DEPRECATED rsDomainValidationRecords "Use generic-lens or generic-optics with 'domainValidationRecords' instead." #-}

-- | The timestamp when the certificate was last updated.
--
-- /Note:/ Consider using 'updatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsUpdatedAt :: Lens.Lens' RenewalSummary (Lude.Maybe Lude.Timestamp)
rsUpdatedAt = Lens.lens (updatedAt :: RenewalSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {updatedAt = a} :: RenewalSummary)
{-# DEPRECATED rsUpdatedAt "Use generic-lens or generic-optics with 'updatedAt' instead." #-}

-- | The reason for the renewal status of the certificate.
--
-- /Note:/ Consider using 'renewalStatusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsRenewalStatusReason :: Lens.Lens' RenewalSummary (Lude.Maybe Lude.Text)
rsRenewalStatusReason = Lens.lens (renewalStatusReason :: RenewalSummary -> Lude.Maybe Lude.Text) (\s a -> s {renewalStatusReason = a} :: RenewalSummary)
{-# DEPRECATED rsRenewalStatusReason "Use generic-lens or generic-optics with 'renewalStatusReason' instead." #-}

instance Lude.FromJSON RenewalSummary where
  parseJSON =
    Lude.withObject
      "RenewalSummary"
      ( \x ->
          RenewalSummary'
            Lude.<$> (x Lude..:? "renewalStatus")
            Lude.<*> (x Lude..:? "domainValidationRecords" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "updatedAt")
            Lude.<*> (x Lude..:? "renewalStatusReason")
      )
