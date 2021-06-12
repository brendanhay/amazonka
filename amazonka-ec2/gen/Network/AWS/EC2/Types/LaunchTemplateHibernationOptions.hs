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
-- Module      : Network.AWS.EC2.Types.LaunchTemplateHibernationOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateHibernationOptions where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Indicates whether an instance is configured for hibernation.
--
-- /See:/ 'newLaunchTemplateHibernationOptions' smart constructor.
data LaunchTemplateHibernationOptions = LaunchTemplateHibernationOptions'
  { -- | If this parameter is set to @true@, the instance is enabled for
    -- hibernation; otherwise, it is not enabled for hibernation.
    configured :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LaunchTemplateHibernationOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configured', 'launchTemplateHibernationOptions_configured' - If this parameter is set to @true@, the instance is enabled for
-- hibernation; otherwise, it is not enabled for hibernation.
newLaunchTemplateHibernationOptions ::
  LaunchTemplateHibernationOptions
newLaunchTemplateHibernationOptions =
  LaunchTemplateHibernationOptions'
    { configured =
        Core.Nothing
    }

-- | If this parameter is set to @true@, the instance is enabled for
-- hibernation; otherwise, it is not enabled for hibernation.
launchTemplateHibernationOptions_configured :: Lens.Lens' LaunchTemplateHibernationOptions (Core.Maybe Core.Bool)
launchTemplateHibernationOptions_configured = Lens.lens (\LaunchTemplateHibernationOptions' {configured} -> configured) (\s@LaunchTemplateHibernationOptions' {} a -> s {configured = a} :: LaunchTemplateHibernationOptions)

instance
  Core.FromXML
    LaunchTemplateHibernationOptions
  where
  parseXML x =
    LaunchTemplateHibernationOptions'
      Core.<$> (x Core..@? "configured")

instance
  Core.Hashable
    LaunchTemplateHibernationOptions

instance Core.NFData LaunchTemplateHibernationOptions
