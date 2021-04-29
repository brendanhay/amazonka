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
-- Module      : Network.AWS.EC2.Types.LaunchTemplateHibernationOptionsRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateHibernationOptionsRequest where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Indicates whether the instance is configured for hibernation. This
-- parameter is valid only if the instance meets the
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html#hibernating-prerequisites hibernation prerequisites>.
--
-- /See:/ 'newLaunchTemplateHibernationOptionsRequest' smart constructor.
data LaunchTemplateHibernationOptionsRequest = LaunchTemplateHibernationOptionsRequest'
  { -- | If you set this parameter to @true@, the instance is enabled for
    -- hibernation.
    --
    -- Default: @false@
    configured :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplateHibernationOptionsRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configured', 'launchTemplateHibernationOptionsRequest_configured' - If you set this parameter to @true@, the instance is enabled for
-- hibernation.
--
-- Default: @false@
newLaunchTemplateHibernationOptionsRequest ::
  LaunchTemplateHibernationOptionsRequest
newLaunchTemplateHibernationOptionsRequest =
  LaunchTemplateHibernationOptionsRequest'
    { configured =
        Prelude.Nothing
    }

-- | If you set this parameter to @true@, the instance is enabled for
-- hibernation.
--
-- Default: @false@
launchTemplateHibernationOptionsRequest_configured :: Lens.Lens' LaunchTemplateHibernationOptionsRequest (Prelude.Maybe Prelude.Bool)
launchTemplateHibernationOptionsRequest_configured = Lens.lens (\LaunchTemplateHibernationOptionsRequest' {configured} -> configured) (\s@LaunchTemplateHibernationOptionsRequest' {} a -> s {configured = a} :: LaunchTemplateHibernationOptionsRequest)

instance
  Prelude.Hashable
    LaunchTemplateHibernationOptionsRequest

instance
  Prelude.NFData
    LaunchTemplateHibernationOptionsRequest

instance
  Prelude.ToQuery
    LaunchTemplateHibernationOptionsRequest
  where
  toQuery LaunchTemplateHibernationOptionsRequest' {..} =
    Prelude.mconcat
      ["Configured" Prelude.=: configured]
