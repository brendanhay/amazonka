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
-- Module      : Network.AWS.EC2.Types.FleetLaunchTemplateSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FleetLaunchTemplateSpecification where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the Amazon EC2 launch template and the launch template version
-- that can be used by a Spot Fleet request to configure Amazon EC2
-- instances. For information about launch templates, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html Launching an instance from a launch template>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- /See:/ 'newFleetLaunchTemplateSpecification' smart constructor.
data FleetLaunchTemplateSpecification = FleetLaunchTemplateSpecification'
  { -- | The ID of the launch template. If you specify the template ID, you
    -- can\'t specify the template name.
    launchTemplateId :: Prelude.Maybe Prelude.Text,
    -- | The name of the launch template. If you specify the template name, you
    -- can\'t specify the template ID.
    launchTemplateName :: Prelude.Maybe Prelude.Text,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FleetLaunchTemplateSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchTemplateId', 'fleetLaunchTemplateSpecification_launchTemplateId' - The ID of the launch template. If you specify the template ID, you
-- can\'t specify the template name.
--
-- 'launchTemplateName', 'fleetLaunchTemplateSpecification_launchTemplateName' - The name of the launch template. If you specify the template name, you
-- can\'t specify the template ID.
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
    { launchTemplateId =
        Prelude.Nothing,
      launchTemplateName = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The ID of the launch template. If you specify the template ID, you
-- can\'t specify the template name.
fleetLaunchTemplateSpecification_launchTemplateId :: Lens.Lens' FleetLaunchTemplateSpecification (Prelude.Maybe Prelude.Text)
fleetLaunchTemplateSpecification_launchTemplateId = Lens.lens (\FleetLaunchTemplateSpecification' {launchTemplateId} -> launchTemplateId) (\s@FleetLaunchTemplateSpecification' {} a -> s {launchTemplateId = a} :: FleetLaunchTemplateSpecification)

-- | The name of the launch template. If you specify the template name, you
-- can\'t specify the template ID.
fleetLaunchTemplateSpecification_launchTemplateName :: Lens.Lens' FleetLaunchTemplateSpecification (Prelude.Maybe Prelude.Text)
fleetLaunchTemplateSpecification_launchTemplateName = Lens.lens (\FleetLaunchTemplateSpecification' {launchTemplateName} -> launchTemplateName) (\s@FleetLaunchTemplateSpecification' {} a -> s {launchTemplateName = a} :: FleetLaunchTemplateSpecification)

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
  Prelude.FromXML
    FleetLaunchTemplateSpecification
  where
  parseXML x =
    FleetLaunchTemplateSpecification'
      Prelude.<$> (x Prelude..@? "launchTemplateId")
      Prelude.<*> (x Prelude..@? "launchTemplateName")
      Prelude.<*> (x Prelude..@? "version")

instance
  Prelude.Hashable
    FleetLaunchTemplateSpecification

instance
  Prelude.NFData
    FleetLaunchTemplateSpecification

instance
  Prelude.ToQuery
    FleetLaunchTemplateSpecification
  where
  toQuery FleetLaunchTemplateSpecification' {..} =
    Prelude.mconcat
      [ "LaunchTemplateId" Prelude.=: launchTemplateId,
        "LaunchTemplateName" Prelude.=: launchTemplateName,
        "Version" Prelude.=: version
      ]
