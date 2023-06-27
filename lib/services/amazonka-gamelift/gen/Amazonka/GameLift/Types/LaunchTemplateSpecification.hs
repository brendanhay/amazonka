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
-- Module      : Amazonka.GameLift.Types.LaunchTemplateSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.LaunchTemplateSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | __This data type is used with the Amazon GameLift FleetIQ and game
-- server groups.__
--
-- An Amazon Elastic Compute Cloud launch template that contains
-- configuration settings and game server code to be deployed to all
-- instances in a game server group. The launch template is specified when
-- creating a new game server group.
--
-- /See:/ 'newLaunchTemplateSpecification' smart constructor.
data LaunchTemplateSpecification = LaunchTemplateSpecification'
  { -- | A unique identifier for an existing Amazon EC2 launch template.
    launchTemplateId :: Prelude.Maybe Prelude.Text,
    -- | A readable identifier for an existing Amazon EC2 launch template.
    launchTemplateName :: Prelude.Maybe Prelude.Text,
    -- | The version of the Amazon EC2 launch template to use. If no version is
    -- specified, the default version will be used. With Amazon EC2, you can
    -- specify a default version for a launch template. If none is set, the
    -- default is the first version created.
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
-- 'launchTemplateId', 'launchTemplateSpecification_launchTemplateId' - A unique identifier for an existing Amazon EC2 launch template.
--
-- 'launchTemplateName', 'launchTemplateSpecification_launchTemplateName' - A readable identifier for an existing Amazon EC2 launch template.
--
-- 'version', 'launchTemplateSpecification_version' - The version of the Amazon EC2 launch template to use. If no version is
-- specified, the default version will be used. With Amazon EC2, you can
-- specify a default version for a launch template. If none is set, the
-- default is the first version created.
newLaunchTemplateSpecification ::
  LaunchTemplateSpecification
newLaunchTemplateSpecification =
  LaunchTemplateSpecification'
    { launchTemplateId =
        Prelude.Nothing,
      launchTemplateName = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | A unique identifier for an existing Amazon EC2 launch template.
launchTemplateSpecification_launchTemplateId :: Lens.Lens' LaunchTemplateSpecification (Prelude.Maybe Prelude.Text)
launchTemplateSpecification_launchTemplateId = Lens.lens (\LaunchTemplateSpecification' {launchTemplateId} -> launchTemplateId) (\s@LaunchTemplateSpecification' {} a -> s {launchTemplateId = a} :: LaunchTemplateSpecification)

-- | A readable identifier for an existing Amazon EC2 launch template.
launchTemplateSpecification_launchTemplateName :: Lens.Lens' LaunchTemplateSpecification (Prelude.Maybe Prelude.Text)
launchTemplateSpecification_launchTemplateName = Lens.lens (\LaunchTemplateSpecification' {launchTemplateName} -> launchTemplateName) (\s@LaunchTemplateSpecification' {} a -> s {launchTemplateName = a} :: LaunchTemplateSpecification)

-- | The version of the Amazon EC2 launch template to use. If no version is
-- specified, the default version will be used. With Amazon EC2, you can
-- specify a default version for a launch template. If none is set, the
-- default is the first version created.
launchTemplateSpecification_version :: Lens.Lens' LaunchTemplateSpecification (Prelude.Maybe Prelude.Text)
launchTemplateSpecification_version = Lens.lens (\LaunchTemplateSpecification' {version} -> version) (\s@LaunchTemplateSpecification' {} a -> s {version = a} :: LaunchTemplateSpecification)

instance Prelude.Hashable LaunchTemplateSpecification where
  hashWithSalt _salt LaunchTemplateSpecification' {..} =
    _salt
      `Prelude.hashWithSalt` launchTemplateId
      `Prelude.hashWithSalt` launchTemplateName
      `Prelude.hashWithSalt` version

instance Prelude.NFData LaunchTemplateSpecification where
  rnf LaunchTemplateSpecification' {..} =
    Prelude.rnf launchTemplateId
      `Prelude.seq` Prelude.rnf launchTemplateName
      `Prelude.seq` Prelude.rnf version

instance Data.ToJSON LaunchTemplateSpecification where
  toJSON LaunchTemplateSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LaunchTemplateId" Data..=)
              Prelude.<$> launchTemplateId,
            ("LaunchTemplateName" Data..=)
              Prelude.<$> launchTemplateName,
            ("Version" Data..=) Prelude.<$> version
          ]
      )
