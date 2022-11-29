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
-- Module      : Amazonka.EC2.Types.LaunchTemplateHibernationOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LaunchTemplateHibernationOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Indicates whether an instance is configured for hibernation.
--
-- /See:/ 'newLaunchTemplateHibernationOptions' smart constructor.
data LaunchTemplateHibernationOptions = LaunchTemplateHibernationOptions'
  { -- | If this parameter is set to @true@, the instance is enabled for
    -- hibernation; otherwise, it is not enabled for hibernation.
    configured :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing
    }

-- | If this parameter is set to @true@, the instance is enabled for
-- hibernation; otherwise, it is not enabled for hibernation.
launchTemplateHibernationOptions_configured :: Lens.Lens' LaunchTemplateHibernationOptions (Prelude.Maybe Prelude.Bool)
launchTemplateHibernationOptions_configured = Lens.lens (\LaunchTemplateHibernationOptions' {configured} -> configured) (\s@LaunchTemplateHibernationOptions' {} a -> s {configured = a} :: LaunchTemplateHibernationOptions)

instance
  Core.FromXML
    LaunchTemplateHibernationOptions
  where
  parseXML x =
    LaunchTemplateHibernationOptions'
      Prelude.<$> (x Core..@? "configured")

instance
  Prelude.Hashable
    LaunchTemplateHibernationOptions
  where
  hashWithSalt
    _salt
    LaunchTemplateHibernationOptions' {..} =
      _salt `Prelude.hashWithSalt` configured

instance
  Prelude.NFData
    LaunchTemplateHibernationOptions
  where
  rnf LaunchTemplateHibernationOptions' {..} =
    Prelude.rnf configured
