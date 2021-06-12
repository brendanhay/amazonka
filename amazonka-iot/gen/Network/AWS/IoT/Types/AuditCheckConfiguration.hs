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
-- Module      : Network.AWS.IoT.Types.AuditCheckConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditCheckConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Which audit checks are enabled and disabled for this account.
--
-- /See:/ 'newAuditCheckConfiguration' smart constructor.
data AuditCheckConfiguration = AuditCheckConfiguration'
  { -- | True if this audit check is enabled for this account.
    enabled :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  AuditCheckConfiguration' {enabled = Core.Nothing}

-- | True if this audit check is enabled for this account.
auditCheckConfiguration_enabled :: Lens.Lens' AuditCheckConfiguration (Core.Maybe Core.Bool)
auditCheckConfiguration_enabled = Lens.lens (\AuditCheckConfiguration' {enabled} -> enabled) (\s@AuditCheckConfiguration' {} a -> s {enabled = a} :: AuditCheckConfiguration)

instance Core.FromJSON AuditCheckConfiguration where
  parseJSON =
    Core.withObject
      "AuditCheckConfiguration"
      ( \x ->
          AuditCheckConfiguration'
            Core.<$> (x Core..:? "enabled")
      )

instance Core.Hashable AuditCheckConfiguration

instance Core.NFData AuditCheckConfiguration

instance Core.ToJSON AuditCheckConfiguration where
  toJSON AuditCheckConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [("enabled" Core..=) Core.<$> enabled]
      )
