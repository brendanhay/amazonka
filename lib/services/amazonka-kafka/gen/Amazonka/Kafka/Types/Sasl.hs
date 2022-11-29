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
-- Module      : Amazonka.Kafka.Types.Sasl
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.Sasl where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Kafka.Types.Iam
import Amazonka.Kafka.Types.Scram
import qualified Amazonka.Prelude as Prelude

-- | Details for client authentication using SASL.
--
-- /See:/ 'newSasl' smart constructor.
data Sasl = Sasl'
  { -- | Details for SASL\/SCRAM client authentication.
    scram :: Prelude.Maybe Scram,
    -- | Indicates whether IAM access control is enabled.
    iam :: Prelude.Maybe Iam
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Sasl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scram', 'sasl_scram' - Details for SASL\/SCRAM client authentication.
--
-- 'iam', 'sasl_iam' - Indicates whether IAM access control is enabled.
newSasl ::
  Sasl
newSasl =
  Sasl'
    { scram = Prelude.Nothing,
      iam = Prelude.Nothing
    }

-- | Details for SASL\/SCRAM client authentication.
sasl_scram :: Lens.Lens' Sasl (Prelude.Maybe Scram)
sasl_scram = Lens.lens (\Sasl' {scram} -> scram) (\s@Sasl' {} a -> s {scram = a} :: Sasl)

-- | Indicates whether IAM access control is enabled.
sasl_iam :: Lens.Lens' Sasl (Prelude.Maybe Iam)
sasl_iam = Lens.lens (\Sasl' {iam} -> iam) (\s@Sasl' {} a -> s {iam = a} :: Sasl)

instance Core.FromJSON Sasl where
  parseJSON =
    Core.withObject
      "Sasl"
      ( \x ->
          Sasl'
            Prelude.<$> (x Core..:? "scram") Prelude.<*> (x Core..:? "iam")
      )

instance Prelude.Hashable Sasl where
  hashWithSalt _salt Sasl' {..} =
    _salt `Prelude.hashWithSalt` scram
      `Prelude.hashWithSalt` iam

instance Prelude.NFData Sasl where
  rnf Sasl' {..} =
    Prelude.rnf scram `Prelude.seq` Prelude.rnf iam

instance Core.ToJSON Sasl where
  toJSON Sasl' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("scram" Core..=) Prelude.<$> scram,
            ("iam" Core..=) Prelude.<$> iam
          ]
      )
