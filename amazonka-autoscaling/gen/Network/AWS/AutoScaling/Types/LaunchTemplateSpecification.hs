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
-- Module      : Network.AWS.AutoScaling.Types.LaunchTemplateSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.LaunchTemplateSpecification where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes the Amazon EC2 launch template and the launch template version
-- that can be used by an Auto Scaling group to configure Amazon EC2
-- instances.
--
-- The launch template that is specified must be configured for use with an
-- Auto Scaling group. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-launch-template.html Creating a launch template for an Auto Scaling group>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- /See:/ 'newLaunchTemplateSpecification' smart constructor.
data LaunchTemplateSpecification = LaunchTemplateSpecification'
  { -- | The ID of the launch template. To get the template ID, use the Amazon
    -- EC2
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeLaunchTemplates.html DescribeLaunchTemplates>
    -- API operation. New launch templates can be created using the Amazon EC2
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateLaunchTemplate.html CreateLaunchTemplate>
    -- API.
    --
    -- Conditional: You must specify either a @LaunchTemplateId@ or a
    -- @LaunchTemplateName@.
    launchTemplateId :: Core.Maybe Core.Text,
    -- | The name of the launch template. To get the template name, use the
    -- Amazon EC2
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeLaunchTemplates.html DescribeLaunchTemplates>
    -- API operation. New launch templates can be created using the Amazon EC2
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateLaunchTemplate.html CreateLaunchTemplate>
    -- API.
    --
    -- Conditional: You must specify either a @LaunchTemplateId@ or a
    -- @LaunchTemplateName@.
    launchTemplateName :: Core.Maybe Core.Text,
    -- | The version number, @$Latest@, or @$Default@. To get the version number,
    -- use the Amazon EC2
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeLaunchTemplateVersions.html DescribeLaunchTemplateVersions>
    -- API operation. New launch template versions can be created using the
    -- Amazon EC2
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateLaunchTemplateVersion.html CreateLaunchTemplateVersion>
    -- API. If the value is @$Latest@, Amazon EC2 Auto Scaling selects the
    -- latest version of the launch template when launching instances. If the
    -- value is @$Default@, Amazon EC2 Auto Scaling selects the default version
    -- of the launch template when launching instances. The default value is
    -- @$Default@.
    version :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LaunchTemplateSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchTemplateId', 'launchTemplateSpecification_launchTemplateId' - The ID of the launch template. To get the template ID, use the Amazon
-- EC2
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeLaunchTemplates.html DescribeLaunchTemplates>
-- API operation. New launch templates can be created using the Amazon EC2
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateLaunchTemplate.html CreateLaunchTemplate>
-- API.
--
-- Conditional: You must specify either a @LaunchTemplateId@ or a
-- @LaunchTemplateName@.
--
-- 'launchTemplateName', 'launchTemplateSpecification_launchTemplateName' - The name of the launch template. To get the template name, use the
-- Amazon EC2
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeLaunchTemplates.html DescribeLaunchTemplates>
-- API operation. New launch templates can be created using the Amazon EC2
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateLaunchTemplate.html CreateLaunchTemplate>
-- API.
--
-- Conditional: You must specify either a @LaunchTemplateId@ or a
-- @LaunchTemplateName@.
--
-- 'version', 'launchTemplateSpecification_version' - The version number, @$Latest@, or @$Default@. To get the version number,
-- use the Amazon EC2
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeLaunchTemplateVersions.html DescribeLaunchTemplateVersions>
-- API operation. New launch template versions can be created using the
-- Amazon EC2
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateLaunchTemplateVersion.html CreateLaunchTemplateVersion>
-- API. If the value is @$Latest@, Amazon EC2 Auto Scaling selects the
-- latest version of the launch template when launching instances. If the
-- value is @$Default@, Amazon EC2 Auto Scaling selects the default version
-- of the launch template when launching instances. The default value is
-- @$Default@.
newLaunchTemplateSpecification ::
  LaunchTemplateSpecification
newLaunchTemplateSpecification =
  LaunchTemplateSpecification'
    { launchTemplateId =
        Core.Nothing,
      launchTemplateName = Core.Nothing,
      version = Core.Nothing
    }

-- | The ID of the launch template. To get the template ID, use the Amazon
-- EC2
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeLaunchTemplates.html DescribeLaunchTemplates>
-- API operation. New launch templates can be created using the Amazon EC2
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateLaunchTemplate.html CreateLaunchTemplate>
-- API.
--
-- Conditional: You must specify either a @LaunchTemplateId@ or a
-- @LaunchTemplateName@.
launchTemplateSpecification_launchTemplateId :: Lens.Lens' LaunchTemplateSpecification (Core.Maybe Core.Text)
launchTemplateSpecification_launchTemplateId = Lens.lens (\LaunchTemplateSpecification' {launchTemplateId} -> launchTemplateId) (\s@LaunchTemplateSpecification' {} a -> s {launchTemplateId = a} :: LaunchTemplateSpecification)

-- | The name of the launch template. To get the template name, use the
-- Amazon EC2
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeLaunchTemplates.html DescribeLaunchTemplates>
-- API operation. New launch templates can be created using the Amazon EC2
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateLaunchTemplate.html CreateLaunchTemplate>
-- API.
--
-- Conditional: You must specify either a @LaunchTemplateId@ or a
-- @LaunchTemplateName@.
launchTemplateSpecification_launchTemplateName :: Lens.Lens' LaunchTemplateSpecification (Core.Maybe Core.Text)
launchTemplateSpecification_launchTemplateName = Lens.lens (\LaunchTemplateSpecification' {launchTemplateName} -> launchTemplateName) (\s@LaunchTemplateSpecification' {} a -> s {launchTemplateName = a} :: LaunchTemplateSpecification)

-- | The version number, @$Latest@, or @$Default@. To get the version number,
-- use the Amazon EC2
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeLaunchTemplateVersions.html DescribeLaunchTemplateVersions>
-- API operation. New launch template versions can be created using the
-- Amazon EC2
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateLaunchTemplateVersion.html CreateLaunchTemplateVersion>
-- API. If the value is @$Latest@, Amazon EC2 Auto Scaling selects the
-- latest version of the launch template when launching instances. If the
-- value is @$Default@, Amazon EC2 Auto Scaling selects the default version
-- of the launch template when launching instances. The default value is
-- @$Default@.
launchTemplateSpecification_version :: Lens.Lens' LaunchTemplateSpecification (Core.Maybe Core.Text)
launchTemplateSpecification_version = Lens.lens (\LaunchTemplateSpecification' {version} -> version) (\s@LaunchTemplateSpecification' {} a -> s {version = a} :: LaunchTemplateSpecification)

instance Core.FromXML LaunchTemplateSpecification where
  parseXML x =
    LaunchTemplateSpecification'
      Core.<$> (x Core..@? "LaunchTemplateId")
      Core.<*> (x Core..@? "LaunchTemplateName")
      Core.<*> (x Core..@? "Version")

instance Core.Hashable LaunchTemplateSpecification

instance Core.NFData LaunchTemplateSpecification

instance Core.ToQuery LaunchTemplateSpecification where
  toQuery LaunchTemplateSpecification' {..} =
    Core.mconcat
      [ "LaunchTemplateId" Core.=: launchTemplateId,
        "LaunchTemplateName" Core.=: launchTemplateName,
        "Version" Core.=: version
      ]
