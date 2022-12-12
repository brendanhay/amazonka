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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.FleetLaunchTemplateSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | The Amazon EC2 launch template that can be used by a Spot Fleet to
-- configure Amazon EC2 instances. You must specify either the ID or name
-- of the launch template in the request, but not both.
--
-- For information about launch templates, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html Launch an instance from a launch template>
-- in the /Amazon EC2 User Guide/.
--
-- /See:/ 'newFleetLaunchTemplateSpecification' smart constructor.
data FleetLaunchTemplateSpecification = FleetLaunchTemplateSpecification'
  { -- | The ID of the launch template.
    --
    -- You must specify the @LaunchTemplateId@ or the @LaunchTemplateName@, but
    -- not both.
    launchTemplateId :: Prelude.Maybe Prelude.Text,
    -- | The name of the launch template.
    --
    -- You must specify the @LaunchTemplateName@ or the @LaunchTemplateId@, but
    -- not both.
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FleetLaunchTemplateSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchTemplateId', 'fleetLaunchTemplateSpecification_launchTemplateId' - The ID of the launch template.
--
-- You must specify the @LaunchTemplateId@ or the @LaunchTemplateName@, but
-- not both.
--
-- 'launchTemplateName', 'fleetLaunchTemplateSpecification_launchTemplateName' - The name of the launch template.
--
-- You must specify the @LaunchTemplateName@ or the @LaunchTemplateId@, but
-- not both.
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

-- | The ID of the launch template.
--
-- You must specify the @LaunchTemplateId@ or the @LaunchTemplateName@, but
-- not both.
fleetLaunchTemplateSpecification_launchTemplateId :: Lens.Lens' FleetLaunchTemplateSpecification (Prelude.Maybe Prelude.Text)
fleetLaunchTemplateSpecification_launchTemplateId = Lens.lens (\FleetLaunchTemplateSpecification' {launchTemplateId} -> launchTemplateId) (\s@FleetLaunchTemplateSpecification' {} a -> s {launchTemplateId = a} :: FleetLaunchTemplateSpecification)

-- | The name of the launch template.
--
-- You must specify the @LaunchTemplateName@ or the @LaunchTemplateId@, but
-- not both.
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
  Data.FromXML
    FleetLaunchTemplateSpecification
  where
  parseXML x =
    FleetLaunchTemplateSpecification'
      Prelude.<$> (x Data..@? "launchTemplateId")
      Prelude.<*> (x Data..@? "launchTemplateName")
      Prelude.<*> (x Data..@? "version")

instance
  Prelude.Hashable
    FleetLaunchTemplateSpecification
  where
  hashWithSalt
    _salt
    FleetLaunchTemplateSpecification' {..} =
      _salt `Prelude.hashWithSalt` launchTemplateId
        `Prelude.hashWithSalt` launchTemplateName
        `Prelude.hashWithSalt` version

instance
  Prelude.NFData
    FleetLaunchTemplateSpecification
  where
  rnf FleetLaunchTemplateSpecification' {..} =
    Prelude.rnf launchTemplateId
      `Prelude.seq` Prelude.rnf launchTemplateName
      `Prelude.seq` Prelude.rnf version

instance
  Data.ToQuery
    FleetLaunchTemplateSpecification
  where
  toQuery FleetLaunchTemplateSpecification' {..} =
    Prelude.mconcat
      [ "LaunchTemplateId" Data.=: launchTemplateId,
        "LaunchTemplateName" Data.=: launchTemplateName,
        "Version" Data.=: version
      ]
