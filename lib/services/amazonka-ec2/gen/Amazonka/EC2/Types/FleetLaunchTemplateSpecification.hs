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
-- Module      : Amazonka.EC2.Types.FleetLaunchTemplateSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.FleetLaunchTemplateSpecification where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the Amazon EC2 launch template and the launch template version
-- that can be used by a Spot Fleet request to configure Amazon EC2
-- instances. For information about launch templates, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html Launching an instance from a launch template>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- /See:/ 'newFleetLaunchTemplateSpecification' smart constructor.
data FleetLaunchTemplateSpecification = FleetLaunchTemplateSpecification'
  { -- | The name of the launch template. If you specify the template name, you
    -- can\'t specify the template ID.
    launchTemplateName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the launch template. If you specify the template ID, you
    -- can\'t specify the template name.
    launchTemplateId :: Prelude.Maybe Prelude.Text,
    -- | The launch template version number, @$Latest@, or @$Default@. You must
    -- specify a value, otherwise the request fails.
    --
    -- If the value is @$Latest@, Amazon EC2 uses the latest version of the
    -- launch template.
    --
    -- If the value is @$Default@, Amazon EC2 uses the default version of the
    -- launch template.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FleetLaunchTemplateSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchTemplateName', 'fleetLaunchTemplateSpecification_launchTemplateName' - The name of the launch template. If you specify the template name, you
-- can\'t specify the template ID.
--
-- 'launchTemplateId', 'fleetLaunchTemplateSpecification_launchTemplateId' - The ID of the launch template. If you specify the template ID, you
-- can\'t specify the template name.
--
-- 'version', 'fleetLaunchTemplateSpecification_version' - The launch template version number, @$Latest@, or @$Default@. You must
-- specify a value, otherwise the request fails.
--
-- If the value is @$Latest@, Amazon EC2 uses the latest version of the
-- launch template.
--
-- If the value is @$Default@, Amazon EC2 uses the default version of the
-- launch template.
newFleetLaunchTemplateSpecification ::
  FleetLaunchTemplateSpecification
newFleetLaunchTemplateSpecification =
  FleetLaunchTemplateSpecification'
    { launchTemplateName =
        Prelude.Nothing,
      launchTemplateId = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The name of the launch template. If you specify the template name, you
-- can\'t specify the template ID.
fleetLaunchTemplateSpecification_launchTemplateName :: Lens.Lens' FleetLaunchTemplateSpecification (Prelude.Maybe Prelude.Text)
fleetLaunchTemplateSpecification_launchTemplateName = Lens.lens (\FleetLaunchTemplateSpecification' {launchTemplateName} -> launchTemplateName) (\s@FleetLaunchTemplateSpecification' {} a -> s {launchTemplateName = a} :: FleetLaunchTemplateSpecification)

-- | The ID of the launch template. If you specify the template ID, you
-- can\'t specify the template name.
fleetLaunchTemplateSpecification_launchTemplateId :: Lens.Lens' FleetLaunchTemplateSpecification (Prelude.Maybe Prelude.Text)
fleetLaunchTemplateSpecification_launchTemplateId = Lens.lens (\FleetLaunchTemplateSpecification' {launchTemplateId} -> launchTemplateId) (\s@FleetLaunchTemplateSpecification' {} a -> s {launchTemplateId = a} :: FleetLaunchTemplateSpecification)

-- | The launch template version number, @$Latest@, or @$Default@. You must
-- specify a value, otherwise the request fails.
--
-- If the value is @$Latest@, Amazon EC2 uses the latest version of the
-- launch template.
--
-- If the value is @$Default@, Amazon EC2 uses the default version of the
-- launch template.
fleetLaunchTemplateSpecification_version :: Lens.Lens' FleetLaunchTemplateSpecification (Prelude.Maybe Prelude.Text)
fleetLaunchTemplateSpecification_version = Lens.lens (\FleetLaunchTemplateSpecification' {version} -> version) (\s@FleetLaunchTemplateSpecification' {} a -> s {version = a} :: FleetLaunchTemplateSpecification)

instance
  Core.FromXML
    FleetLaunchTemplateSpecification
  where
  parseXML x =
    FleetLaunchTemplateSpecification'
      Prelude.<$> (x Core..@? "launchTemplateName")
      Prelude.<*> (x Core..@? "launchTemplateId")
      Prelude.<*> (x Core..@? "version")

instance
  Prelude.Hashable
    FleetLaunchTemplateSpecification
  where
  hashWithSalt
    _salt
    FleetLaunchTemplateSpecification' {..} =
      _salt `Prelude.hashWithSalt` launchTemplateName
        `Prelude.hashWithSalt` launchTemplateId
        `Prelude.hashWithSalt` version

instance
  Prelude.NFData
    FleetLaunchTemplateSpecification
  where
  rnf FleetLaunchTemplateSpecification' {..} =
    Prelude.rnf launchTemplateName
      `Prelude.seq` Prelude.rnf launchTemplateId
      `Prelude.seq` Prelude.rnf version

instance
  Core.ToQuery
    FleetLaunchTemplateSpecification
  where
  toQuery FleetLaunchTemplateSpecification' {..} =
    Prelude.mconcat
      [ "LaunchTemplateName" Core.=: launchTemplateName,
        "LaunchTemplateId" Core.=: launchTemplateId,
        "Version" Core.=: version
      ]
