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
-- Module      : Network.AWS.IoT.Types.Configuration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.Configuration where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Configuration.
--
-- /See:/ 'newConfiguration' smart constructor.
data Configuration = Configuration'
  { -- | True to enable the configuration.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Configuration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'configuration_enabled' - True to enable the configuration.
newConfiguration ::
  Configuration
newConfiguration =
  Configuration' {enabled = Prelude.Nothing}

-- | True to enable the configuration.
configuration_enabled :: Lens.Lens' Configuration (Prelude.Maybe Prelude.Bool)
configuration_enabled = Lens.lens (\Configuration' {enabled} -> enabled) (\s@Configuration' {} a -> s {enabled = a} :: Configuration)

instance Prelude.FromJSON Configuration where
  parseJSON =
    Prelude.withObject
      "Configuration"
      ( \x ->
          Configuration' Prelude.<$> (x Prelude..:? "Enabled")
      )

instance Prelude.Hashable Configuration

instance Prelude.NFData Configuration

instance Prelude.ToJSON Configuration where
  toJSON Configuration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("Enabled" Prelude..=) Prelude.<$> enabled]
      )
