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
-- Module      : Amazonka.EC2.Types.LaunchTemplateSpecification
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LaunchTemplateSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | The launch template to use. You must specify either the launch template
-- ID or launch template name in the request, but not both.
--
-- /See:/ 'newLaunchTemplateSpecification' smart constructor.
data LaunchTemplateSpecification = LaunchTemplateSpecification'
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
    -- | The launch template version number, @$Latest@, or @$Default@.
    --
    -- If the value is @$Latest@, Amazon EC2 uses the latest version of the
    -- launch template.
    --
    -- If the value is @$Default@, Amazon EC2 uses the default version of the
    -- launch template.
    --
    -- Default: The default version of the launch template.
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
-- 'launchTemplateId', 'launchTemplateSpecification_launchTemplateId' - The ID of the launch template.
--
-- You must specify the @LaunchTemplateId@ or the @LaunchTemplateName@, but
-- not both.
--
-- 'launchTemplateName', 'launchTemplateSpecification_launchTemplateName' - The name of the launch template.
--
-- You must specify the @LaunchTemplateName@ or the @LaunchTemplateId@, but
-- not both.
--
-- 'version', 'launchTemplateSpecification_version' - The launch template version number, @$Latest@, or @$Default@.
--
-- If the value is @$Latest@, Amazon EC2 uses the latest version of the
-- launch template.
--
-- If the value is @$Default@, Amazon EC2 uses the default version of the
-- launch template.
--
-- Default: The default version of the launch template.
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
--
-- You must specify the @LaunchTemplateId@ or the @LaunchTemplateName@, but
-- not both.
launchTemplateSpecification_launchTemplateId :: Lens.Lens' LaunchTemplateSpecification (Prelude.Maybe Prelude.Text)
launchTemplateSpecification_launchTemplateId = Lens.lens (\LaunchTemplateSpecification' {launchTemplateId} -> launchTemplateId) (\s@LaunchTemplateSpecification' {} a -> s {launchTemplateId = a} :: LaunchTemplateSpecification)

-- | The name of the launch template.
--
-- You must specify the @LaunchTemplateName@ or the @LaunchTemplateId@, but
-- not both.
launchTemplateSpecification_launchTemplateName :: Lens.Lens' LaunchTemplateSpecification (Prelude.Maybe Prelude.Text)
launchTemplateSpecification_launchTemplateName = Lens.lens (\LaunchTemplateSpecification' {launchTemplateName} -> launchTemplateName) (\s@LaunchTemplateSpecification' {} a -> s {launchTemplateName = a} :: LaunchTemplateSpecification)

-- | The launch template version number, @$Latest@, or @$Default@.
--
-- If the value is @$Latest@, Amazon EC2 uses the latest version of the
-- launch template.
--
-- If the value is @$Default@, Amazon EC2 uses the default version of the
-- launch template.
--
-- Default: The default version of the launch template.
launchTemplateSpecification_version :: Lens.Lens' LaunchTemplateSpecification (Prelude.Maybe Prelude.Text)
launchTemplateSpecification_version = Lens.lens (\LaunchTemplateSpecification' {version} -> version) (\s@LaunchTemplateSpecification' {} a -> s {version = a} :: LaunchTemplateSpecification)

instance Prelude.Hashable LaunchTemplateSpecification where
  hashWithSalt _salt LaunchTemplateSpecification' {..} =
    _salt `Prelude.hashWithSalt` launchTemplateId
      `Prelude.hashWithSalt` launchTemplateName
      `Prelude.hashWithSalt` version

instance Prelude.NFData LaunchTemplateSpecification where
  rnf LaunchTemplateSpecification' {..} =
    Prelude.rnf launchTemplateId
      `Prelude.seq` Prelude.rnf launchTemplateName
      `Prelude.seq` Prelude.rnf version

instance Data.ToQuery LaunchTemplateSpecification where
  toQuery LaunchTemplateSpecification' {..} =
    Prelude.mconcat
      [ "LaunchTemplateId" Data.=: launchTemplateId,
        "LaunchTemplateName" Data.=: launchTemplateName,
        "Version" Data.=: version
      ]
