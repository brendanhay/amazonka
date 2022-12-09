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
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types.Iam
import Amazonka.Kafka.Types.Scram
import qualified Amazonka.Prelude as Prelude

-- | Details for client authentication using SASL.
--
-- /See:/ 'newSasl' smart constructor.
data Sasl = Sasl'
  { -- | Indicates whether IAM access control is enabled.
    iam :: Prelude.Maybe Iam,
    -- | Details for SASL\/SCRAM client authentication.
    scram :: Prelude.Maybe Scram
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
-- 'iam', 'sasl_iam' - Indicates whether IAM access control is enabled.
--
-- 'scram', 'sasl_scram' - Details for SASL\/SCRAM client authentication.
newSasl ::
  Sasl
newSasl =
  Sasl'
    { iam = Prelude.Nothing,
      scram = Prelude.Nothing
    }

-- | Indicates whether IAM access control is enabled.
sasl_iam :: Lens.Lens' Sasl (Prelude.Maybe Iam)
sasl_iam = Lens.lens (\Sasl' {iam} -> iam) (\s@Sasl' {} a -> s {iam = a} :: Sasl)

-- | Details for SASL\/SCRAM client authentication.
sasl_scram :: Lens.Lens' Sasl (Prelude.Maybe Scram)
sasl_scram = Lens.lens (\Sasl' {scram} -> scram) (\s@Sasl' {} a -> s {scram = a} :: Sasl)

instance Data.FromJSON Sasl where
  parseJSON =
    Data.withObject
      "Sasl"
      ( \x ->
          Sasl'
            Prelude.<$> (x Data..:? "iam") Prelude.<*> (x Data..:? "scram")
      )

instance Prelude.Hashable Sasl where
  hashWithSalt _salt Sasl' {..} =
    _salt `Prelude.hashWithSalt` iam
      `Prelude.hashWithSalt` scram

instance Prelude.NFData Sasl where
  rnf Sasl' {..} =
    Prelude.rnf iam `Prelude.seq` Prelude.rnf scram

instance Data.ToJSON Sasl where
  toJSON Sasl' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("iam" Data..=) Prelude.<$> iam,
            ("scram" Data..=) Prelude.<$> scram
          ]
      )
