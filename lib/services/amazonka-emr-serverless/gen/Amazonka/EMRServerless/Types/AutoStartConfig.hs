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
-- Module      : Amazonka.EMRServerless.Types.AutoStartConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRServerless.Types.AutoStartConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The configuration for an application to automatically start on job
-- submission.
--
-- /See:/ 'newAutoStartConfig' smart constructor.
data AutoStartConfig = AutoStartConfig'
  { -- | Enables the application to automatically start on job submission.
    -- Defaults to true.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoStartConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'autoStartConfig_enabled' - Enables the application to automatically start on job submission.
-- Defaults to true.
newAutoStartConfig ::
  AutoStartConfig
newAutoStartConfig =
  AutoStartConfig' {enabled = Prelude.Nothing}

-- | Enables the application to automatically start on job submission.
-- Defaults to true.
autoStartConfig_enabled :: Lens.Lens' AutoStartConfig (Prelude.Maybe Prelude.Bool)
autoStartConfig_enabled = Lens.lens (\AutoStartConfig' {enabled} -> enabled) (\s@AutoStartConfig' {} a -> s {enabled = a} :: AutoStartConfig)

instance Core.FromJSON AutoStartConfig where
  parseJSON =
    Core.withObject
      "AutoStartConfig"
      ( \x ->
          AutoStartConfig' Prelude.<$> (x Core..:? "enabled")
      )

instance Prelude.Hashable AutoStartConfig where
  hashWithSalt _salt AutoStartConfig' {..} =
    _salt `Prelude.hashWithSalt` enabled

instance Prelude.NFData AutoStartConfig where
  rnf AutoStartConfig' {..} = Prelude.rnf enabled

instance Core.ToJSON AutoStartConfig where
  toJSON AutoStartConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [("enabled" Core..=) Prelude.<$> enabled]
      )
