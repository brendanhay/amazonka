{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SESV2.Types.DomainIspPlacement
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SESV2.Types.DomainIspPlacement where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object that contains inbox placement data for email sent from one of
-- your email domains to a specific email provider.
--
-- /See:/ 'newDomainIspPlacement' smart constructor.
data DomainIspPlacement = DomainIspPlacement'
  { -- | The percentage of messages that were sent from the selected domain to
    -- the specified email provider that arrived in recipients\' spam or junk
    -- mail folders.
    spamPercentage :: Prelude.Maybe Prelude.Double,
    -- | The total number of messages that were sent from the selected domain to
    -- the specified email provider that arrived in recipients\' inboxes.
    inboxRawCount :: Prelude.Maybe Prelude.Integer,
    -- | The name of the email provider that the inbox placement data applies to.
    ispName :: Prelude.Maybe Prelude.Text,
    -- | The percentage of messages that were sent from the selected domain to
    -- the specified email provider that arrived in recipients\' inboxes.
    inboxPercentage :: Prelude.Maybe Prelude.Double,
    -- | The total number of messages that were sent from the selected domain to
    -- the specified email provider that arrived in recipients\' spam or junk
    -- mail folders.
    spamRawCount :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DomainIspPlacement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'spamPercentage', 'domainIspPlacement_spamPercentage' - The percentage of messages that were sent from the selected domain to
-- the specified email provider that arrived in recipients\' spam or junk
-- mail folders.
--
-- 'inboxRawCount', 'domainIspPlacement_inboxRawCount' - The total number of messages that were sent from the selected domain to
-- the specified email provider that arrived in recipients\' inboxes.
--
-- 'ispName', 'domainIspPlacement_ispName' - The name of the email provider that the inbox placement data applies to.
--
-- 'inboxPercentage', 'domainIspPlacement_inboxPercentage' - The percentage of messages that were sent from the selected domain to
-- the specified email provider that arrived in recipients\' inboxes.
--
-- 'spamRawCount', 'domainIspPlacement_spamRawCount' - The total number of messages that were sent from the selected domain to
-- the specified email provider that arrived in recipients\' spam or junk
-- mail folders.
newDomainIspPlacement ::
  DomainIspPlacement
newDomainIspPlacement =
  DomainIspPlacement'
    { spamPercentage =
        Prelude.Nothing,
      inboxRawCount = Prelude.Nothing,
      ispName = Prelude.Nothing,
      inboxPercentage = Prelude.Nothing,
      spamRawCount = Prelude.Nothing
    }

-- | The percentage of messages that were sent from the selected domain to
-- the specified email provider that arrived in recipients\' spam or junk
-- mail folders.
domainIspPlacement_spamPercentage :: Lens.Lens' DomainIspPlacement (Prelude.Maybe Prelude.Double)
domainIspPlacement_spamPercentage = Lens.lens (\DomainIspPlacement' {spamPercentage} -> spamPercentage) (\s@DomainIspPlacement' {} a -> s {spamPercentage = a} :: DomainIspPlacement)

-- | The total number of messages that were sent from the selected domain to
-- the specified email provider that arrived in recipients\' inboxes.
domainIspPlacement_inboxRawCount :: Lens.Lens' DomainIspPlacement (Prelude.Maybe Prelude.Integer)
domainIspPlacement_inboxRawCount = Lens.lens (\DomainIspPlacement' {inboxRawCount} -> inboxRawCount) (\s@DomainIspPlacement' {} a -> s {inboxRawCount = a} :: DomainIspPlacement)

-- | The name of the email provider that the inbox placement data applies to.
domainIspPlacement_ispName :: Lens.Lens' DomainIspPlacement (Prelude.Maybe Prelude.Text)
domainIspPlacement_ispName = Lens.lens (\DomainIspPlacement' {ispName} -> ispName) (\s@DomainIspPlacement' {} a -> s {ispName = a} :: DomainIspPlacement)

-- | The percentage of messages that were sent from the selected domain to
-- the specified email provider that arrived in recipients\' inboxes.
domainIspPlacement_inboxPercentage :: Lens.Lens' DomainIspPlacement (Prelude.Maybe Prelude.Double)
domainIspPlacement_inboxPercentage = Lens.lens (\DomainIspPlacement' {inboxPercentage} -> inboxPercentage) (\s@DomainIspPlacement' {} a -> s {inboxPercentage = a} :: DomainIspPlacement)

-- | The total number of messages that were sent from the selected domain to
-- the specified email provider that arrived in recipients\' spam or junk
-- mail folders.
domainIspPlacement_spamRawCount :: Lens.Lens' DomainIspPlacement (Prelude.Maybe Prelude.Integer)
domainIspPlacement_spamRawCount = Lens.lens (\DomainIspPlacement' {spamRawCount} -> spamRawCount) (\s@DomainIspPlacement' {} a -> s {spamRawCount = a} :: DomainIspPlacement)

instance Core.FromJSON DomainIspPlacement where
  parseJSON =
    Core.withObject
      "DomainIspPlacement"
      ( \x ->
          DomainIspPlacement'
            Prelude.<$> (x Core..:? "SpamPercentage")
            Prelude.<*> (x Core..:? "InboxRawCount")
            Prelude.<*> (x Core..:? "IspName")
            Prelude.<*> (x Core..:? "InboxPercentage")
            Prelude.<*> (x Core..:? "SpamRawCount")
      )

instance Prelude.Hashable DomainIspPlacement

instance Prelude.NFData DomainIspPlacement
