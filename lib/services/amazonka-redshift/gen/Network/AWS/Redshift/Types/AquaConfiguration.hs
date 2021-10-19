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
-- Module      : Network.AWS.Redshift.Types.AquaConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.AquaConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.AquaConfigurationStatus
import Network.AWS.Redshift.Types.AquaStatus

-- | The AQUA (Advanced Query Accelerator) configuration of the cluster.
--
-- /See:/ 'newAquaConfiguration' smart constructor.
data AquaConfiguration = AquaConfiguration'
  { -- | The value represents how the cluster is configured to use AQUA. Possible
    -- values include the following.
    --
    -- -   enabled - Use AQUA if it is available for the current Amazon Web
    --     Services Region and Amazon Redshift node type.
    --
    -- -   disabled - Don\'t use AQUA.
    --
    -- -   auto - Amazon Redshift determines whether to use AQUA.
    aquaConfigurationStatus :: Prelude.Maybe AquaConfigurationStatus,
    -- | The value indicates the status of AQUA on the cluster. Possible values
    -- include the following.
    --
    -- -   enabled - AQUA is enabled.
    --
    -- -   disabled - AQUA is not enabled.
    --
    -- -   applying - AQUA status is being applied.
    aquaStatus :: Prelude.Maybe AquaStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AquaConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aquaConfigurationStatus', 'aquaConfiguration_aquaConfigurationStatus' - The value represents how the cluster is configured to use AQUA. Possible
-- values include the following.
--
-- -   enabled - Use AQUA if it is available for the current Amazon Web
--     Services Region and Amazon Redshift node type.
--
-- -   disabled - Don\'t use AQUA.
--
-- -   auto - Amazon Redshift determines whether to use AQUA.
--
-- 'aquaStatus', 'aquaConfiguration_aquaStatus' - The value indicates the status of AQUA on the cluster. Possible values
-- include the following.
--
-- -   enabled - AQUA is enabled.
--
-- -   disabled - AQUA is not enabled.
--
-- -   applying - AQUA status is being applied.
newAquaConfiguration ::
  AquaConfiguration
newAquaConfiguration =
  AquaConfiguration'
    { aquaConfigurationStatus =
        Prelude.Nothing,
      aquaStatus = Prelude.Nothing
    }

-- | The value represents how the cluster is configured to use AQUA. Possible
-- values include the following.
--
-- -   enabled - Use AQUA if it is available for the current Amazon Web
--     Services Region and Amazon Redshift node type.
--
-- -   disabled - Don\'t use AQUA.
--
-- -   auto - Amazon Redshift determines whether to use AQUA.
aquaConfiguration_aquaConfigurationStatus :: Lens.Lens' AquaConfiguration (Prelude.Maybe AquaConfigurationStatus)
aquaConfiguration_aquaConfigurationStatus = Lens.lens (\AquaConfiguration' {aquaConfigurationStatus} -> aquaConfigurationStatus) (\s@AquaConfiguration' {} a -> s {aquaConfigurationStatus = a} :: AquaConfiguration)

-- | The value indicates the status of AQUA on the cluster. Possible values
-- include the following.
--
-- -   enabled - AQUA is enabled.
--
-- -   disabled - AQUA is not enabled.
--
-- -   applying - AQUA status is being applied.
aquaConfiguration_aquaStatus :: Lens.Lens' AquaConfiguration (Prelude.Maybe AquaStatus)
aquaConfiguration_aquaStatus = Lens.lens (\AquaConfiguration' {aquaStatus} -> aquaStatus) (\s@AquaConfiguration' {} a -> s {aquaStatus = a} :: AquaConfiguration)

instance Core.FromXML AquaConfiguration where
  parseXML x =
    AquaConfiguration'
      Prelude.<$> (x Core..@? "AquaConfigurationStatus")
      Prelude.<*> (x Core..@? "AquaStatus")

instance Prelude.Hashable AquaConfiguration

instance Prelude.NFData AquaConfiguration
