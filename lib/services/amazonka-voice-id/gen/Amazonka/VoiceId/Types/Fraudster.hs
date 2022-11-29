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
-- Module      : Amazonka.VoiceId.Types.Fraudster
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VoiceId.Types.Fraudster where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains all the information about a fraudster.
--
-- /See:/ 'newFraudster' smart constructor.
data Fraudster = Fraudster'
  { -- | The service-generated identifier for the fraudster.
    generatedFraudsterId :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the domain containing the fraudster.
    domainId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when Voice ID identified the fraudster.
    createdAt :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Fraudster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'generatedFraudsterId', 'fraudster_generatedFraudsterId' - The service-generated identifier for the fraudster.
--
-- 'domainId', 'fraudster_domainId' - The identifier for the domain containing the fraudster.
--
-- 'createdAt', 'fraudster_createdAt' - The timestamp when Voice ID identified the fraudster.
newFraudster ::
  Fraudster
newFraudster =
  Fraudster'
    { generatedFraudsterId = Prelude.Nothing,
      domainId = Prelude.Nothing,
      createdAt = Prelude.Nothing
    }

-- | The service-generated identifier for the fraudster.
fraudster_generatedFraudsterId :: Lens.Lens' Fraudster (Prelude.Maybe Prelude.Text)
fraudster_generatedFraudsterId = Lens.lens (\Fraudster' {generatedFraudsterId} -> generatedFraudsterId) (\s@Fraudster' {} a -> s {generatedFraudsterId = a} :: Fraudster)

-- | The identifier for the domain containing the fraudster.
fraudster_domainId :: Lens.Lens' Fraudster (Prelude.Maybe Prelude.Text)
fraudster_domainId = Lens.lens (\Fraudster' {domainId} -> domainId) (\s@Fraudster' {} a -> s {domainId = a} :: Fraudster)

-- | The timestamp when Voice ID identified the fraudster.
fraudster_createdAt :: Lens.Lens' Fraudster (Prelude.Maybe Prelude.UTCTime)
fraudster_createdAt = Lens.lens (\Fraudster' {createdAt} -> createdAt) (\s@Fraudster' {} a -> s {createdAt = a} :: Fraudster) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON Fraudster where
  parseJSON =
    Core.withObject
      "Fraudster"
      ( \x ->
          Fraudster'
            Prelude.<$> (x Core..:? "GeneratedFraudsterId")
            Prelude.<*> (x Core..:? "DomainId")
            Prelude.<*> (x Core..:? "CreatedAt")
      )

instance Prelude.Hashable Fraudster where
  hashWithSalt _salt Fraudster' {..} =
    _salt `Prelude.hashWithSalt` generatedFraudsterId
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` createdAt

instance Prelude.NFData Fraudster where
  rnf Fraudster' {..} =
    Prelude.rnf generatedFraudsterId
      `Prelude.seq` Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf createdAt
