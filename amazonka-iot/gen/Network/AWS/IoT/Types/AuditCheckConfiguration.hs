{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Which audit checks are enabled and disabled for this account.
--
-- /See:/ 'newAuditCheckConfiguration' smart constructor.
data AuditCheckConfiguration = AuditCheckConfiguration'
  { -- | True if this audit check is enabled for this account.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON AuditCheckConfiguration where
  parseJSON =
    Prelude.withObject
      "AuditCheckConfiguration"
      ( \x ->
          AuditCheckConfiguration'
            Prelude.<$> (x Prelude..:? "enabled")
      )

instance Prelude.Hashable AuditCheckConfiguration

instance Prelude.NFData AuditCheckConfiguration

instance Prelude.ToJSON AuditCheckConfiguration where
  toJSON AuditCheckConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("enabled" Prelude..=) Prelude.<$> enabled]
      )
