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
-- Module      : Amazonka.IoT.Types.AuditCheckConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.AuditCheckConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Which audit checks are enabled and disabled for this account.
--
-- /See:/ 'newAuditCheckConfiguration' smart constructor.
data AuditCheckConfiguration = AuditCheckConfiguration'
  { -- | True if this audit check is enabled for this account.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuditCheckConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'auditCheckConfiguration_enabled' - True if this audit check is enabled for this account.
newAuditCheckConfiguration ::
  AuditCheckConfiguration
newAuditCheckConfiguration =
  AuditCheckConfiguration' {enabled = Prelude.Nothing}

-- | True if this audit check is enabled for this account.
auditCheckConfiguration_enabled :: Lens.Lens' AuditCheckConfiguration (Prelude.Maybe Prelude.Bool)
auditCheckConfiguration_enabled = Lens.lens (\AuditCheckConfiguration' {enabled} -> enabled) (\s@AuditCheckConfiguration' {} a -> s {enabled = a} :: AuditCheckConfiguration)

instance Data.FromJSON AuditCheckConfiguration where
  parseJSON =
    Data.withObject
      "AuditCheckConfiguration"
      ( \x ->
          AuditCheckConfiguration'
            Prelude.<$> (x Data..:? "enabled")
      )

instance Prelude.Hashable AuditCheckConfiguration where
  hashWithSalt _salt AuditCheckConfiguration' {..} =
    _salt `Prelude.hashWithSalt` enabled

instance Prelude.NFData AuditCheckConfiguration where
  rnf AuditCheckConfiguration' {..} =
    Prelude.rnf enabled

instance Data.ToJSON AuditCheckConfiguration where
  toJSON AuditCheckConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [("enabled" Data..=) Prelude.<$> enabled]
      )
