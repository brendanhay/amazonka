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
-- Module      : Amazonka.SESV2.Types.DomainIspPlacement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.DomainIspPlacement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that contains inbox placement data for email sent from one of
-- your email domains to a specific email provider.
--
-- /See:/ 'newDomainIspPlacement' smart constructor.
data DomainIspPlacement = DomainIspPlacement'
  { -- | The total number of messages that were sent from the selected domain to
    -- the specified email provider that arrived in recipients\' inboxes.
    inboxRawCount :: Prelude.Maybe Prelude.Integer,
    -- | The percentage of messages that were sent from the selected domain to
    -- the specified email provider that arrived in recipients\' spam or junk
    -- mail folders.
    spamPercentage :: Prelude.Maybe Prelude.Double,
    -- | The percentage of messages that were sent from the selected domain to
    -- the specified email provider that arrived in recipients\' inboxes.
    inboxPercentage :: Prelude.Maybe Prelude.Double,
    -- | The total number of messages that were sent from the selected domain to
    -- the specified email provider that arrived in recipients\' spam or junk
    -- mail folders.
    spamRawCount :: Prelude.Maybe Prelude.Integer,
    -- | The name of the email provider that the inbox placement data applies to.
    ispName :: Prelude.Maybe Prelude.Text
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
-- 'inboxRawCount', 'domainIspPlacement_inboxRawCount' - The total number of messages that were sent from the selected domain to
-- the specified email provider that arrived in recipients\' inboxes.
--
-- 'spamPercentage', 'domainIspPlacement_spamPercentage' - The percentage of messages that were sent from the selected domain to
-- the specified email provider that arrived in recipients\' spam or junk
-- mail folders.
--
-- 'inboxPercentage', 'domainIspPlacement_inboxPercentage' - The percentage of messages that were sent from the selected domain to
-- the specified email provider that arrived in recipients\' inboxes.
--
-- 'spamRawCount', 'domainIspPlacement_spamRawCount' - The total number of messages that were sent from the selected domain to
-- the specified email provider that arrived in recipients\' spam or junk
-- mail folders.
--
-- 'ispName', 'domainIspPlacement_ispName' - The name of the email provider that the inbox placement data applies to.
newDomainIspPlacement ::
  DomainIspPlacement
newDomainIspPlacement =
  DomainIspPlacement'
    { inboxRawCount =
        Prelude.Nothing,
      spamPercentage = Prelude.Nothing,
      inboxPercentage = Prelude.Nothing,
      spamRawCount = Prelude.Nothing,
      ispName = Prelude.Nothing
    }

-- | The total number of messages that were sent from the selected domain to
-- the specified email provider that arrived in recipients\' inboxes.
domainIspPlacement_inboxRawCount :: Lens.Lens' DomainIspPlacement (Prelude.Maybe Prelude.Integer)
domainIspPlacement_inboxRawCount = Lens.lens (\DomainIspPlacement' {inboxRawCount} -> inboxRawCount) (\s@DomainIspPlacement' {} a -> s {inboxRawCount = a} :: DomainIspPlacement)

-- | The percentage of messages that were sent from the selected domain to
-- the specified email provider that arrived in recipients\' spam or junk
-- mail folders.
domainIspPlacement_spamPercentage :: Lens.Lens' DomainIspPlacement (Prelude.Maybe Prelude.Double)
domainIspPlacement_spamPercentage = Lens.lens (\DomainIspPlacement' {spamPercentage} -> spamPercentage) (\s@DomainIspPlacement' {} a -> s {spamPercentage = a} :: DomainIspPlacement)

-- | The percentage of messages that were sent from the selected domain to
-- the specified email provider that arrived in recipients\' inboxes.
domainIspPlacement_inboxPercentage :: Lens.Lens' DomainIspPlacement (Prelude.Maybe Prelude.Double)
domainIspPlacement_inboxPercentage = Lens.lens (\DomainIspPlacement' {inboxPercentage} -> inboxPercentage) (\s@DomainIspPlacement' {} a -> s {inboxPercentage = a} :: DomainIspPlacement)

-- | The total number of messages that were sent from the selected domain to
-- the specified email provider that arrived in recipients\' spam or junk
-- mail folders.
domainIspPlacement_spamRawCount :: Lens.Lens' DomainIspPlacement (Prelude.Maybe Prelude.Integer)
domainIspPlacement_spamRawCount = Lens.lens (\DomainIspPlacement' {spamRawCount} -> spamRawCount) (\s@DomainIspPlacement' {} a -> s {spamRawCount = a} :: DomainIspPlacement)

-- | The name of the email provider that the inbox placement data applies to.
domainIspPlacement_ispName :: Lens.Lens' DomainIspPlacement (Prelude.Maybe Prelude.Text)
domainIspPlacement_ispName = Lens.lens (\DomainIspPlacement' {ispName} -> ispName) (\s@DomainIspPlacement' {} a -> s {ispName = a} :: DomainIspPlacement)

instance Core.FromJSON DomainIspPlacement where
  parseJSON =
    Core.withObject
      "DomainIspPlacement"
      ( \x ->
          DomainIspPlacement'
            Prelude.<$> (x Core..:? "InboxRawCount")
            Prelude.<*> (x Core..:? "SpamPercentage")
            Prelude.<*> (x Core..:? "InboxPercentage")
            Prelude.<*> (x Core..:? "SpamRawCount")
            Prelude.<*> (x Core..:? "IspName")
      )

instance Prelude.Hashable DomainIspPlacement where
  hashWithSalt _salt DomainIspPlacement' {..} =
    _salt `Prelude.hashWithSalt` inboxRawCount
      `Prelude.hashWithSalt` spamPercentage
      `Prelude.hashWithSalt` inboxPercentage
      `Prelude.hashWithSalt` spamRawCount
      `Prelude.hashWithSalt` ispName

instance Prelude.NFData DomainIspPlacement where
  rnf DomainIspPlacement' {..} =
    Prelude.rnf inboxRawCount
      `Prelude.seq` Prelude.rnf spamPercentage
      `Prelude.seq` Prelude.rnf inboxPercentage
      `Prelude.seq` Prelude.rnf spamRawCount
      `Prelude.seq` Prelude.rnf ispName
