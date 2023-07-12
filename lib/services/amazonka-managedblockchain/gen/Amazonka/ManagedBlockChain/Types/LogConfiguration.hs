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
-- Module      : Amazonka.ManagedBlockChain.Types.LogConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ManagedBlockChain.Types.LogConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A configuration for logging events.
--
-- /See:/ 'newLogConfiguration' smart constructor.
data LogConfiguration = LogConfiguration'
  { -- | Indicates whether logging is enabled.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LogConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'logConfiguration_enabled' - Indicates whether logging is enabled.
newLogConfiguration ::
  LogConfiguration
newLogConfiguration =
  LogConfiguration' {enabled = Prelude.Nothing}

-- | Indicates whether logging is enabled.
logConfiguration_enabled :: Lens.Lens' LogConfiguration (Prelude.Maybe Prelude.Bool)
logConfiguration_enabled = Lens.lens (\LogConfiguration' {enabled} -> enabled) (\s@LogConfiguration' {} a -> s {enabled = a} :: LogConfiguration)

instance Data.FromJSON LogConfiguration where
  parseJSON =
    Data.withObject
      "LogConfiguration"
      ( \x ->
          LogConfiguration' Prelude.<$> (x Data..:? "Enabled")
      )

instance Prelude.Hashable LogConfiguration where
  hashWithSalt _salt LogConfiguration' {..} =
    _salt `Prelude.hashWithSalt` enabled

instance Prelude.NFData LogConfiguration where
  rnf LogConfiguration' {..} = Prelude.rnf enabled

instance Data.ToJSON LogConfiguration where
  toJSON LogConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Enabled" Data..=) Prelude.<$> enabled]
      )
