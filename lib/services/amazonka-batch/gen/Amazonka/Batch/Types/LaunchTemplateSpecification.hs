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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.LaunchTemplateSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a launch template that\'s associated with a
-- compute resource. You must specify either the launch template ID or
-- launch template name in the request, but not both.
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
  { -- | The ID of the launch template.
    launchTemplateId :: Prelude.Maybe Prelude.Text,
    -- | The version number of the launch template, @$Latest@, or @$Default@.
    --
    -- If the value is @$Latest@, the latest version of the launch template is
    -- used. If the value is @$Default@, the default version of the launch
    -- template is used.
    --
    -- If the AMI ID that\'s used in a compute environment is from the launch
    -- template, the AMI isn\'t changed when the compute environment is
    -- updated. It\'s only changed if the @updateToLatestImageVersion@
    -- parameter for the compute environment is set to @true@. During an
    -- infrastructure update, if either @$Latest@ or @$Default@ is specified,
    -- Batch re-evaluates the launch template version, and it might use a
    -- different version of the launch template. This is the case even if the
    -- launch template isn\'t specified in the update. When updating a compute
    -- environment, changing the launch template requires an infrastructure
    -- update of the compute environment. For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
    -- in the /Batch User Guide/.
    --
    -- Default: @$Default@.
    version :: Prelude.Maybe Prelude.Text,
    -- | The name of the launch template.
    launchTemplateName :: Prelude.Maybe Prelude.Text
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
-- 'launchTemplateId', 'launchTemplateSpecification_launchTemplateId' - The ID of the launch template.
--
-- 'version', 'launchTemplateSpecification_version' - The version number of the launch template, @$Latest@, or @$Default@.
--
-- If the value is @$Latest@, the latest version of the launch template is
-- used. If the value is @$Default@, the default version of the launch
-- template is used.
--
-- If the AMI ID that\'s used in a compute environment is from the launch
-- template, the AMI isn\'t changed when the compute environment is
-- updated. It\'s only changed if the @updateToLatestImageVersion@
-- parameter for the compute environment is set to @true@. During an
-- infrastructure update, if either @$Latest@ or @$Default@ is specified,
-- Batch re-evaluates the launch template version, and it might use a
-- different version of the launch template. This is the case even if the
-- launch template isn\'t specified in the update. When updating a compute
-- environment, changing the launch template requires an infrastructure
-- update of the compute environment. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
-- in the /Batch User Guide/.
--
-- Default: @$Default@.
--
-- 'launchTemplateName', 'launchTemplateSpecification_launchTemplateName' - The name of the launch template.
newLaunchTemplateSpecification ::
  LaunchTemplateSpecification
newLaunchTemplateSpecification =
  LaunchTemplateSpecification'
    { launchTemplateId =
        Prelude.Nothing,
      version = Prelude.Nothing,
      launchTemplateName = Prelude.Nothing
    }

-- | The ID of the launch template.
launchTemplateSpecification_launchTemplateId :: Lens.Lens' LaunchTemplateSpecification (Prelude.Maybe Prelude.Text)
launchTemplateSpecification_launchTemplateId = Lens.lens (\LaunchTemplateSpecification' {launchTemplateId} -> launchTemplateId) (\s@LaunchTemplateSpecification' {} a -> s {launchTemplateId = a} :: LaunchTemplateSpecification)

-- | The version number of the launch template, @$Latest@, or @$Default@.
--
-- If the value is @$Latest@, the latest version of the launch template is
-- used. If the value is @$Default@, the default version of the launch
-- template is used.
--
-- If the AMI ID that\'s used in a compute environment is from the launch
-- template, the AMI isn\'t changed when the compute environment is
-- updated. It\'s only changed if the @updateToLatestImageVersion@
-- parameter for the compute environment is set to @true@. During an
-- infrastructure update, if either @$Latest@ or @$Default@ is specified,
-- Batch re-evaluates the launch template version, and it might use a
-- different version of the launch template. This is the case even if the
-- launch template isn\'t specified in the update. When updating a compute
-- environment, changing the launch template requires an infrastructure
-- update of the compute environment. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/updating-compute-environments.html Updating compute environments>
-- in the /Batch User Guide/.
--
-- Default: @$Default@.
launchTemplateSpecification_version :: Lens.Lens' LaunchTemplateSpecification (Prelude.Maybe Prelude.Text)
launchTemplateSpecification_version = Lens.lens (\LaunchTemplateSpecification' {version} -> version) (\s@LaunchTemplateSpecification' {} a -> s {version = a} :: LaunchTemplateSpecification)

-- | The name of the launch template.
launchTemplateSpecification_launchTemplateName :: Lens.Lens' LaunchTemplateSpecification (Prelude.Maybe Prelude.Text)
launchTemplateSpecification_launchTemplateName = Lens.lens (\LaunchTemplateSpecification' {launchTemplateName} -> launchTemplateName) (\s@LaunchTemplateSpecification' {} a -> s {launchTemplateName = a} :: LaunchTemplateSpecification)

instance Core.FromJSON LaunchTemplateSpecification where
  parseJSON =
    Core.withObject
      "LaunchTemplateSpecification"
      ( \x ->
          LaunchTemplateSpecification'
            Prelude.<$> (x Core..:? "launchTemplateId")
            Prelude.<*> (x Core..:? "version")
            Prelude.<*> (x Core..:? "launchTemplateName")
      )

instance Prelude.Hashable LaunchTemplateSpecification where
  hashWithSalt _salt LaunchTemplateSpecification' {..} =
    _salt `Prelude.hashWithSalt` launchTemplateId
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` launchTemplateName

instance Prelude.NFData LaunchTemplateSpecification where
  rnf LaunchTemplateSpecification' {..} =
    Prelude.rnf launchTemplateId
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf launchTemplateName

instance Core.ToJSON LaunchTemplateSpecification where
  toJSON LaunchTemplateSpecification' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("launchTemplateId" Core..=)
              Prelude.<$> launchTemplateId,
            ("version" Core..=) Prelude.<$> version,
            ("launchTemplateName" Core..=)
              Prelude.<$> launchTemplateName
          ]
      )
