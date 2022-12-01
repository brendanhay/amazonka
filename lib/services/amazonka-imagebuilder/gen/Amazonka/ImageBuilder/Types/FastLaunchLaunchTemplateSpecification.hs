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
-- Module      : Amazonka.ImageBuilder.Types.FastLaunchLaunchTemplateSpecification
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.FastLaunchLaunchTemplateSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Identifies the launch template that the associated Windows AMI uses for
-- launching an instance when faster launching is enabled.
--
-- You can specify either the @launchTemplateName@ or the
-- @launchTemplateId@, but not both.
--
-- /See:/ 'newFastLaunchLaunchTemplateSpecification' smart constructor.
data FastLaunchLaunchTemplateSpecification = FastLaunchLaunchTemplateSpecification'
  { -- | The version of the launch template to use for faster launching for a
    -- Windows AMI.
    launchTemplateVersion :: Prelude.Maybe Prelude.Text,
    -- | The ID of the launch template to use for faster launching for a Windows
    -- AMI.
    launchTemplateId :: Prelude.Maybe Prelude.Text,
    -- | The name of the launch template to use for faster launching for a
    -- Windows AMI.
    launchTemplateName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FastLaunchLaunchTemplateSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchTemplateVersion', 'fastLaunchLaunchTemplateSpecification_launchTemplateVersion' - The version of the launch template to use for faster launching for a
-- Windows AMI.
--
-- 'launchTemplateId', 'fastLaunchLaunchTemplateSpecification_launchTemplateId' - The ID of the launch template to use for faster launching for a Windows
-- AMI.
--
-- 'launchTemplateName', 'fastLaunchLaunchTemplateSpecification_launchTemplateName' - The name of the launch template to use for faster launching for a
-- Windows AMI.
newFastLaunchLaunchTemplateSpecification ::
  FastLaunchLaunchTemplateSpecification
newFastLaunchLaunchTemplateSpecification =
  FastLaunchLaunchTemplateSpecification'
    { launchTemplateVersion =
        Prelude.Nothing,
      launchTemplateId = Prelude.Nothing,
      launchTemplateName = Prelude.Nothing
    }

-- | The version of the launch template to use for faster launching for a
-- Windows AMI.
fastLaunchLaunchTemplateSpecification_launchTemplateVersion :: Lens.Lens' FastLaunchLaunchTemplateSpecification (Prelude.Maybe Prelude.Text)
fastLaunchLaunchTemplateSpecification_launchTemplateVersion = Lens.lens (\FastLaunchLaunchTemplateSpecification' {launchTemplateVersion} -> launchTemplateVersion) (\s@FastLaunchLaunchTemplateSpecification' {} a -> s {launchTemplateVersion = a} :: FastLaunchLaunchTemplateSpecification)

-- | The ID of the launch template to use for faster launching for a Windows
-- AMI.
fastLaunchLaunchTemplateSpecification_launchTemplateId :: Lens.Lens' FastLaunchLaunchTemplateSpecification (Prelude.Maybe Prelude.Text)
fastLaunchLaunchTemplateSpecification_launchTemplateId = Lens.lens (\FastLaunchLaunchTemplateSpecification' {launchTemplateId} -> launchTemplateId) (\s@FastLaunchLaunchTemplateSpecification' {} a -> s {launchTemplateId = a} :: FastLaunchLaunchTemplateSpecification)

-- | The name of the launch template to use for faster launching for a
-- Windows AMI.
fastLaunchLaunchTemplateSpecification_launchTemplateName :: Lens.Lens' FastLaunchLaunchTemplateSpecification (Prelude.Maybe Prelude.Text)
fastLaunchLaunchTemplateSpecification_launchTemplateName = Lens.lens (\FastLaunchLaunchTemplateSpecification' {launchTemplateName} -> launchTemplateName) (\s@FastLaunchLaunchTemplateSpecification' {} a -> s {launchTemplateName = a} :: FastLaunchLaunchTemplateSpecification)

instance
  Core.FromJSON
    FastLaunchLaunchTemplateSpecification
  where
  parseJSON =
    Core.withObject
      "FastLaunchLaunchTemplateSpecification"
      ( \x ->
          FastLaunchLaunchTemplateSpecification'
            Prelude.<$> (x Core..:? "launchTemplateVersion")
            Prelude.<*> (x Core..:? "launchTemplateId")
            Prelude.<*> (x Core..:? "launchTemplateName")
      )

instance
  Prelude.Hashable
    FastLaunchLaunchTemplateSpecification
  where
  hashWithSalt
    _salt
    FastLaunchLaunchTemplateSpecification' {..} =
      _salt `Prelude.hashWithSalt` launchTemplateVersion
        `Prelude.hashWithSalt` launchTemplateId
        `Prelude.hashWithSalt` launchTemplateName

instance
  Prelude.NFData
    FastLaunchLaunchTemplateSpecification
  where
  rnf FastLaunchLaunchTemplateSpecification' {..} =
    Prelude.rnf launchTemplateVersion
      `Prelude.seq` Prelude.rnf launchTemplateId
      `Prelude.seq` Prelude.rnf launchTemplateName

instance
  Core.ToJSON
    FastLaunchLaunchTemplateSpecification
  where
  toJSON FastLaunchLaunchTemplateSpecification' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("launchTemplateVersion" Core..=)
              Prelude.<$> launchTemplateVersion,
            ("launchTemplateId" Core..=)
              Prelude.<$> launchTemplateId,
            ("launchTemplateName" Core..=)
              Prelude.<$> launchTemplateName
          ]
      )
