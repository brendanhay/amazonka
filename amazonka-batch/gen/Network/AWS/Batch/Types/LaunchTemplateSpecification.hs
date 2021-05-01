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
-- Module      : Network.AWS.Batch.Types.LaunchTemplateSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.LaunchTemplateSpecification where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object representing a launch template associated with a compute
-- resource. You must specify either the launch template ID or launch
-- template name in the request, but not both.
--
-- If security groups are specified using both the @securityGroupIds@
-- parameter of @CreateComputeEnvironment@ and the launch template, the
-- values in the @securityGroupIds@ parameter of @CreateComputeEnvironment@
-- will be used.
--
-- This object isn\'t applicable to jobs running on Fargate resources.
--
-- /See:/ 'newLaunchTemplateSpecification' smart constructor.
data LaunchTemplateSpecification = LaunchTemplateSpecification'
  { -- | The ID of the launch template.
    launchTemplateId :: Prelude.Maybe Prelude.Text,
    -- | The name of the launch template.
    launchTemplateName :: Prelude.Maybe Prelude.Text,
    -- | The version number of the launch template, @$Latest@, or @$Default@.
    --
    -- If the value is @$Latest@, the latest version of the launch template is
    -- used. If the value is @$Default@, the default version of the launch
    -- template is used.
    --
    -- Default: @$Default@.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplateSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchTemplateId', 'launchTemplateSpecification_launchTemplateId' - The ID of the launch template.
--
-- 'launchTemplateName', 'launchTemplateSpecification_launchTemplateName' - The name of the launch template.
--
-- 'version', 'launchTemplateSpecification_version' - The version number of the launch template, @$Latest@, or @$Default@.
--
-- If the value is @$Latest@, the latest version of the launch template is
-- used. If the value is @$Default@, the default version of the launch
-- template is used.
--
-- Default: @$Default@.
newLaunchTemplateSpecification ::
  LaunchTemplateSpecification
newLaunchTemplateSpecification =
  LaunchTemplateSpecification'
    { launchTemplateId =
        Prelude.Nothing,
      launchTemplateName = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The ID of the launch template.
launchTemplateSpecification_launchTemplateId :: Lens.Lens' LaunchTemplateSpecification (Prelude.Maybe Prelude.Text)
launchTemplateSpecification_launchTemplateId = Lens.lens (\LaunchTemplateSpecification' {launchTemplateId} -> launchTemplateId) (\s@LaunchTemplateSpecification' {} a -> s {launchTemplateId = a} :: LaunchTemplateSpecification)

-- | The name of the launch template.
launchTemplateSpecification_launchTemplateName :: Lens.Lens' LaunchTemplateSpecification (Prelude.Maybe Prelude.Text)
launchTemplateSpecification_launchTemplateName = Lens.lens (\LaunchTemplateSpecification' {launchTemplateName} -> launchTemplateName) (\s@LaunchTemplateSpecification' {} a -> s {launchTemplateName = a} :: LaunchTemplateSpecification)

-- | The version number of the launch template, @$Latest@, or @$Default@.
--
-- If the value is @$Latest@, the latest version of the launch template is
-- used. If the value is @$Default@, the default version of the launch
-- template is used.
--
-- Default: @$Default@.
launchTemplateSpecification_version :: Lens.Lens' LaunchTemplateSpecification (Prelude.Maybe Prelude.Text)
launchTemplateSpecification_version = Lens.lens (\LaunchTemplateSpecification' {version} -> version) (\s@LaunchTemplateSpecification' {} a -> s {version = a} :: LaunchTemplateSpecification)

instance Prelude.FromJSON LaunchTemplateSpecification where
  parseJSON =
    Prelude.withObject
      "LaunchTemplateSpecification"
      ( \x ->
          LaunchTemplateSpecification'
            Prelude.<$> (x Prelude..:? "launchTemplateId")
            Prelude.<*> (x Prelude..:? "launchTemplateName")
            Prelude.<*> (x Prelude..:? "version")
      )

instance Prelude.Hashable LaunchTemplateSpecification

instance Prelude.NFData LaunchTemplateSpecification

instance Prelude.ToJSON LaunchTemplateSpecification where
  toJSON LaunchTemplateSpecification' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("launchTemplateId" Prelude..=)
              Prelude.<$> launchTemplateId,
            ("launchTemplateName" Prelude..=)
              Prelude.<$> launchTemplateName,
            ("version" Prelude..=) Prelude.<$> version
          ]
      )
