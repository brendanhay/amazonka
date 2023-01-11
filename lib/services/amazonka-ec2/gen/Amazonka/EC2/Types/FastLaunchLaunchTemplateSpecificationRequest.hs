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
-- Module      : Amazonka.EC2.Types.FastLaunchLaunchTemplateSpecificationRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.FastLaunchLaunchTemplateSpecificationRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Request to create a launch template for a fast-launch enabled Windows
-- AMI.
--
-- Note - You can specify either the @LaunchTemplateName@ or the
-- @LaunchTemplateId@, but not both.
--
-- /See:/ 'newFastLaunchLaunchTemplateSpecificationRequest' smart constructor.
data FastLaunchLaunchTemplateSpecificationRequest = FastLaunchLaunchTemplateSpecificationRequest'
  { -- | The ID of the launch template to use for faster launching for a Windows
    -- AMI.
    launchTemplateId :: Prelude.Maybe Prelude.Text,
    -- | The name of the launch template to use for faster launching for a
    -- Windows AMI.
    launchTemplateName :: Prelude.Maybe Prelude.Text,
    -- | The version of the launch template to use for faster launching for a
    -- Windows AMI.
    version :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FastLaunchLaunchTemplateSpecificationRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchTemplateId', 'fastLaunchLaunchTemplateSpecificationRequest_launchTemplateId' - The ID of the launch template to use for faster launching for a Windows
-- AMI.
--
-- 'launchTemplateName', 'fastLaunchLaunchTemplateSpecificationRequest_launchTemplateName' - The name of the launch template to use for faster launching for a
-- Windows AMI.
--
-- 'version', 'fastLaunchLaunchTemplateSpecificationRequest_version' - The version of the launch template to use for faster launching for a
-- Windows AMI.
newFastLaunchLaunchTemplateSpecificationRequest ::
  -- | 'version'
  Prelude.Text ->
  FastLaunchLaunchTemplateSpecificationRequest
newFastLaunchLaunchTemplateSpecificationRequest
  pVersion_ =
    FastLaunchLaunchTemplateSpecificationRequest'
      { launchTemplateId =
          Prelude.Nothing,
        launchTemplateName =
          Prelude.Nothing,
        version = pVersion_
      }

-- | The ID of the launch template to use for faster launching for a Windows
-- AMI.
fastLaunchLaunchTemplateSpecificationRequest_launchTemplateId :: Lens.Lens' FastLaunchLaunchTemplateSpecificationRequest (Prelude.Maybe Prelude.Text)
fastLaunchLaunchTemplateSpecificationRequest_launchTemplateId = Lens.lens (\FastLaunchLaunchTemplateSpecificationRequest' {launchTemplateId} -> launchTemplateId) (\s@FastLaunchLaunchTemplateSpecificationRequest' {} a -> s {launchTemplateId = a} :: FastLaunchLaunchTemplateSpecificationRequest)

-- | The name of the launch template to use for faster launching for a
-- Windows AMI.
fastLaunchLaunchTemplateSpecificationRequest_launchTemplateName :: Lens.Lens' FastLaunchLaunchTemplateSpecificationRequest (Prelude.Maybe Prelude.Text)
fastLaunchLaunchTemplateSpecificationRequest_launchTemplateName = Lens.lens (\FastLaunchLaunchTemplateSpecificationRequest' {launchTemplateName} -> launchTemplateName) (\s@FastLaunchLaunchTemplateSpecificationRequest' {} a -> s {launchTemplateName = a} :: FastLaunchLaunchTemplateSpecificationRequest)

-- | The version of the launch template to use for faster launching for a
-- Windows AMI.
fastLaunchLaunchTemplateSpecificationRequest_version :: Lens.Lens' FastLaunchLaunchTemplateSpecificationRequest Prelude.Text
fastLaunchLaunchTemplateSpecificationRequest_version = Lens.lens (\FastLaunchLaunchTemplateSpecificationRequest' {version} -> version) (\s@FastLaunchLaunchTemplateSpecificationRequest' {} a -> s {version = a} :: FastLaunchLaunchTemplateSpecificationRequest)

instance
  Prelude.Hashable
    FastLaunchLaunchTemplateSpecificationRequest
  where
  hashWithSalt
    _salt
    FastLaunchLaunchTemplateSpecificationRequest' {..} =
      _salt `Prelude.hashWithSalt` launchTemplateId
        `Prelude.hashWithSalt` launchTemplateName
        `Prelude.hashWithSalt` version

instance
  Prelude.NFData
    FastLaunchLaunchTemplateSpecificationRequest
  where
  rnf FastLaunchLaunchTemplateSpecificationRequest' {..} =
    Prelude.rnf launchTemplateId
      `Prelude.seq` Prelude.rnf launchTemplateName
      `Prelude.seq` Prelude.rnf version

instance
  Data.ToQuery
    FastLaunchLaunchTemplateSpecificationRequest
  where
  toQuery
    FastLaunchLaunchTemplateSpecificationRequest' {..} =
      Prelude.mconcat
        [ "LaunchTemplateId" Data.=: launchTemplateId,
          "LaunchTemplateName" Data.=: launchTemplateName,
          "Version" Data.=: version
        ]
