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
-- Module      : Amazonka.Kafka.Types.Scram
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.Scram where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details for SASL\/SCRAM client authentication.
--
-- /See:/ 'newScram' smart constructor.
data Scram = Scram'
  { -- | SASL\/SCRAM authentication is enabled or not.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Scram' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'scram_enabled' - SASL\/SCRAM authentication is enabled or not.
newScram ::
  Scram
newScram = Scram' {enabled = Prelude.Nothing}

-- | SASL\/SCRAM authentication is enabled or not.
scram_enabled :: Lens.Lens' Scram (Prelude.Maybe Prelude.Bool)
scram_enabled = Lens.lens (\Scram' {enabled} -> enabled) (\s@Scram' {} a -> s {enabled = a} :: Scram)

instance Core.FromJSON Scram where
  parseJSON =
    Core.withObject
      "Scram"
      (\x -> Scram' Prelude.<$> (x Core..:? "enabled"))

instance Prelude.Hashable Scram where
  hashWithSalt _salt Scram' {..} =
    _salt `Prelude.hashWithSalt` enabled

instance Prelude.NFData Scram where
  rnf Scram' {..} = Prelude.rnf enabled

instance Core.ToJSON Scram where
  toJSON Scram' {..} =
    Core.object
      ( Prelude.catMaybes
          [("enabled" Core..=) Prelude.<$> enabled]
      )
