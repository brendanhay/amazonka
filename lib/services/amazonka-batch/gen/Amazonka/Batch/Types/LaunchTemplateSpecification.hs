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
-- Module      : Amazonka.Batch.Types.LaunchTemplateSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.LaunchTemplateSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object representing a launch template associated with a compute
-- resource. You must specify either the launch template ID or launch
-- template name in the request, but not both.
--
-- If security groups are specified using both the @securityGroupIds@
-- parameter of @CreateComputeEnvironment@ and the launch template, the
-- values in the @securityGroupIds@ parameter of @CreateComputeEnvironment@
-- will be used.
--
-- This object isn\'t applicable to jobs that are running on Fargate
-- resources.
--
-- /See:/ 'newLaunchTemplateSpecification' smart constructor.
data LaunchTemplateSpecification = LaunchTemplateSpecification'
  { -- | The name of the launch template.
    launchTemplateName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the launch template.
    launchTemplateId :: Prelude.Maybe Prelude.Text,
    -- | The version number of the launch template, @$Latest@, or @$Default@.
    --
    -- If the value is @$Latest@, the latest version of the launch template is
    -- used. If the value is @$Default@, the default version of the launch
    -- template is used.
    --
    -- After the compute environment is created, the launch template version
    -- that\'s used isn\'t changed, even if the @$Default@ or @$Latest@ version
    -- for the launch template is updated. To use a new launch template
    -- version, create a new compute environment, add the new compute
    -- environment to the existing job queue, remove the old compute
    -- environment from the job queue, and delete the old compute environment.
    --
    -- Default: @$Default@.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplateSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchTemplateName', 'launchTemplateSpecification_launchTemplateName' - The name of the launch template.
--
-- 'launchTemplateId', 'launchTemplateSpecification_launchTemplateId' - The ID of the launch template.
--
-- 'version', 'launchTemplateSpecification_version' - The version number of the launch template, @$Latest@, or @$Default@.
--
-- If the value is @$Latest@, the latest version of the launch template is
-- used. If the value is @$Default@, the default version of the launch
-- template is used.
--
-- After the compute environment is created, the launch template version
-- that\'s used isn\'t changed, even if the @$Default@ or @$Latest@ version
-- for the launch template is updated. To use a new launch template
-- version, create a new compute environment, add the new compute
-- environment to the existing job queue, remove the old compute
-- environment from the job queue, and delete the old compute environment.
--
-- Default: @$Default@.
newLaunchTemplateSpecification ::
  LaunchTemplateSpecification
newLaunchTemplateSpecification =
  LaunchTemplateSpecification'
    { launchTemplateName =
        Prelude.Nothing,
      launchTemplateId = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The name of the launch template.
launchTemplateSpecification_launchTemplateName :: Lens.Lens' LaunchTemplateSpecification (Prelude.Maybe Prelude.Text)
launchTemplateSpecification_launchTemplateName = Lens.lens (\LaunchTemplateSpecification' {launchTemplateName} -> launchTemplateName) (\s@LaunchTemplateSpecification' {} a -> s {launchTemplateName = a} :: LaunchTemplateSpecification)

-- | The ID of the launch template.
launchTemplateSpecification_launchTemplateId :: Lens.Lens' LaunchTemplateSpecification (Prelude.Maybe Prelude.Text)
launchTemplateSpecification_launchTemplateId = Lens.lens (\LaunchTemplateSpecification' {launchTemplateId} -> launchTemplateId) (\s@LaunchTemplateSpecification' {} a -> s {launchTemplateId = a} :: LaunchTemplateSpecification)

-- | The version number of the launch template, @$Latest@, or @$Default@.
--
-- If the value is @$Latest@, the latest version of the launch template is
-- used. If the value is @$Default@, the default version of the launch
-- template is used.
--
-- After the compute environment is created, the launch template version
-- that\'s used isn\'t changed, even if the @$Default@ or @$Latest@ version
-- for the launch template is updated. To use a new launch template
-- version, create a new compute environment, add the new compute
-- environment to the existing job queue, remove the old compute
-- environment from the job queue, and delete the old compute environment.
--
-- Default: @$Default@.
launchTemplateSpecification_version :: Lens.Lens' LaunchTemplateSpecification (Prelude.Maybe Prelude.Text)
launchTemplateSpecification_version = Lens.lens (\LaunchTemplateSpecification' {version} -> version) (\s@LaunchTemplateSpecification' {} a -> s {version = a} :: LaunchTemplateSpecification)

instance Core.FromJSON LaunchTemplateSpecification where
  parseJSON =
    Core.withObject
      "LaunchTemplateSpecification"
      ( \x ->
          LaunchTemplateSpecification'
            Prelude.<$> (x Core..:? "launchTemplateName")
            Prelude.<*> (x Core..:? "launchTemplateId")
            Prelude.<*> (x Core..:? "version")
      )

instance Prelude.Hashable LaunchTemplateSpecification where
  hashWithSalt salt' LaunchTemplateSpecification' {..} =
    salt' `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` launchTemplateId
      `Prelude.hashWithSalt` launchTemplateName

instance Prelude.NFData LaunchTemplateSpecification where
  rnf LaunchTemplateSpecification' {..} =
    Prelude.rnf launchTemplateName
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf launchTemplateId

instance Core.ToJSON LaunchTemplateSpecification where
  toJSON LaunchTemplateSpecification' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("launchTemplateName" Core..=)
              Prelude.<$> launchTemplateName,
            ("launchTemplateId" Core..=)
              Prelude.<$> launchTemplateId,
            ("version" Core..=) Prelude.<$> version
          ]
      )
