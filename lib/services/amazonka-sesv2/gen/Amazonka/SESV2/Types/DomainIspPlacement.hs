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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.DomainIspPlacement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that contains inbox placement data for email sent from one of
-- your email domains to a specific email provider.
--
-- /See:/ 'newDomainIspPlacement' smart constructor.
data DomainIspPlacement = DomainIspPlacement'
  { -- | The percentage of messages that were sent from the selected domain to
    -- the specified email provider that arrived in recipients\' inboxes.
    inboxPercentage :: Prelude.Maybe Prelude.Double,
    -- | The total number of messages that were sent from the selected domain to
    -- the specified email provider that arrived in recipients\' inboxes.
    inboxRawCount :: Prelude.Maybe Prelude.Integer,
    -- | The name of the email provider that the inbox placement data applies to.
    ispName :: Prelude.Maybe Prelude.Text,
    -- | The percentage of messages that were sent from the selected domain to
    -- the specified email provider that arrived in recipients\' spam or junk
    -- mail folders.
    spamPercentage :: Prelude.Maybe Prelude.Double,
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
-- 'inboxPercentage', 'domainIspPlacement_inboxPercentage' - The percentage of messages that were sent from the selected domain to
-- the specified email provider that arrived in recipients\' inboxes.
--
-- 'inboxRawCount', 'domainIspPlacement_inboxRawCount' - The total number of messages that were sent from the selected domain to
-- the specified email provider that arrived in recipients\' inboxes.
--
-- 'ispName', 'domainIspPlacement_ispName' - The name of the email provider that the inbox placement data applies to.
--
-- 'spamPercentage', 'domainIspPlacement_spamPercentage' - The percentage of messages that were sent from the selected domain to
-- the specified email provider that arrived in recipients\' spam or junk
-- mail folders.
--
-- 'spamRawCount', 'domainIspPlacement_spamRawCount' - The total number of messages that were sent from the selected domain to
-- the specified email provider that arrived in recipients\' spam or junk
-- mail folders.
newDomainIspPlacement ::
  DomainIspPlacement
newDomainIspPlacement =
  DomainIspPlacement'
    { inboxPercentage =
        Prelude.Nothing,
      inboxRawCount = Prelude.Nothing,
      ispName = Prelude.Nothing,
      spamPercentage = Prelude.Nothing,
      spamRawCount = Prelude.Nothing
    }

-- | The percentage of messages that were sent from the selected domain to
-- the specified email provider that arrived in recipients\' inboxes.
domainIspPlacement_inboxPercentage :: Lens.Lens' DomainIspPlacement (Prelude.Maybe Prelude.Double)
domainIspPlacement_inboxPercentage = Lens.lens (\DomainIspPlacement' {inboxPercentage} -> inboxPercentage) (\s@DomainIspPlacement' {} a -> s {inboxPercentage = a} :: DomainIspPlacement)

-- | The total number of messages that were sent from the selected domain to
-- the specified email provider that arrived in recipients\' inboxes.
domainIspPlacement_inboxRawCount :: Lens.Lens' DomainIspPlacement (Prelude.Maybe Prelude.Integer)
domainIspPlacement_inboxRawCount = Lens.lens (\DomainIspPlacement' {inboxRawCount} -> inboxRawCount) (\s@DomainIspPlacement' {} a -> s {inboxRawCount = a} :: DomainIspPlacement)

-- | The name of the email provider that the inbox placement data applies to.
domainIspPlacement_ispName :: Lens.Lens' DomainIspPlacement (Prelude.Maybe Prelude.Text)
domainIspPlacement_ispName = Lens.lens (\DomainIspPlacement' {ispName} -> ispName) (\s@DomainIspPlacement' {} a -> s {ispName = a} :: DomainIspPlacement)

-- | The percentage of messages that were sent from the selected domain to
-- the specified email provider that arrived in recipients\' spam or junk
-- mail folders.
domainIspPlacement_spamPercentage :: Lens.Lens' DomainIspPlacement (Prelude.Maybe Prelude.Double)
domainIspPlacement_spamPercentage = Lens.lens (\DomainIspPlacement' {spamPercentage} -> spamPercentage) (\s@DomainIspPlacement' {} a -> s {spamPercentage = a} :: DomainIspPlacement)

-- | The total number of messages that were sent from the selected domain to
-- the specified email provider that arrived in recipients\' spam or junk
-- mail folders.
domainIspPlacement_spamRawCount :: Lens.Lens' DomainIspPlacement (Prelude.Maybe Prelude.Integer)
domainIspPlacement_spamRawCount = Lens.lens (\DomainIspPlacement' {spamRawCount} -> spamRawCount) (\s@DomainIspPlacement' {} a -> s {spamRawCount = a} :: DomainIspPlacement)

instance Data.FromJSON DomainIspPlacement where
  parseJSON =
    Data.withObject
      "DomainIspPlacement"
      ( \x ->
          DomainIspPlacement'
            Prelude.<$> (x Data..:? "InboxPercentage")
            Prelude.<*> (x Data..:? "InboxRawCount")
            Prelude.<*> (x Data..:? "IspName")
            Prelude.<*> (x Data..:? "SpamPercentage")
            Prelude.<*> (x Data..:? "SpamRawCount")
      )

instance Prelude.Hashable DomainIspPlacement where
  hashWithSalt _salt DomainIspPlacement' {..} =
    _salt
      `Prelude.hashWithSalt` inboxPercentage
      `Prelude.hashWithSalt` inboxRawCount
      `Prelude.hashWithSalt` ispName
      `Prelude.hashWithSalt` spamPercentage
      `Prelude.hashWithSalt` spamRawCount

instance Prelude.NFData DomainIspPlacement where
  rnf DomainIspPlacement' {..} =
    Prelude.rnf inboxPercentage
      `Prelude.seq` Prelude.rnf inboxRawCount
      `Prelude.seq` Prelude.rnf ispName
      `Prelude.seq` Prelude.rnf spamPercentage
      `Prelude.seq` Prelude.rnf spamRawCount
