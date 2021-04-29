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
-- Module      : Network.AWS.EC2.Types.LaunchTemplatesMonitoring
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplatesMonitoring where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the monitoring for the instance.
--
-- /See:/ 'newLaunchTemplatesMonitoring' smart constructor.
data LaunchTemplatesMonitoring = LaunchTemplatesMonitoring'
  { -- | Indicates whether detailed monitoring is enabled. Otherwise, basic
    -- monitoring is enabled.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplatesMonitoring' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'launchTemplatesMonitoring_enabled' - Indicates whether detailed monitoring is enabled. Otherwise, basic
-- monitoring is enabled.
newLaunchTemplatesMonitoring ::
  LaunchTemplatesMonitoring
newLaunchTemplatesMonitoring =
  LaunchTemplatesMonitoring'
    { enabled =
        Prelude.Nothing
    }

-- | Indicates whether detailed monitoring is enabled. Otherwise, basic
-- monitoring is enabled.
launchTemplatesMonitoring_enabled :: Lens.Lens' LaunchTemplatesMonitoring (Prelude.Maybe Prelude.Bool)
launchTemplatesMonitoring_enabled = Lens.lens (\LaunchTemplatesMonitoring' {enabled} -> enabled) (\s@LaunchTemplatesMonitoring' {} a -> s {enabled = a} :: LaunchTemplatesMonitoring)

instance Prelude.FromXML LaunchTemplatesMonitoring where
  parseXML x =
    LaunchTemplatesMonitoring'
      Prelude.<$> (x Prelude..@? "enabled")

instance Prelude.Hashable LaunchTemplatesMonitoring

instance Prelude.NFData LaunchTemplatesMonitoring
