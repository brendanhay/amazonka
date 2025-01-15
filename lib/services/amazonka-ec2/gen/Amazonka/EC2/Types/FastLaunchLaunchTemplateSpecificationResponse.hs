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
-- Module      : Amazonka.EC2.Types.FastLaunchLaunchTemplateSpecificationResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.FastLaunchLaunchTemplateSpecificationResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Identifies the launch template to use for faster launching of the
-- Windows AMI.
--
-- /See:/ 'newFastLaunchLaunchTemplateSpecificationResponse' smart constructor.
data FastLaunchLaunchTemplateSpecificationResponse = FastLaunchLaunchTemplateSpecificationResponse'
  { -- | The ID of the launch template for faster launching of the associated
    -- Windows AMI.
    launchTemplateId :: Prelude.Maybe Prelude.Text,
    -- | The name of the launch template for faster launching of the associated
    -- Windows AMI.
    launchTemplateName :: Prelude.Maybe Prelude.Text,
    -- | The version of the launch template for faster launching of the
    -- associated Windows AMI.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FastLaunchLaunchTemplateSpecificationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchTemplateId', 'fastLaunchLaunchTemplateSpecificationResponse_launchTemplateId' - The ID of the launch template for faster launching of the associated
-- Windows AMI.
--
-- 'launchTemplateName', 'fastLaunchLaunchTemplateSpecificationResponse_launchTemplateName' - The name of the launch template for faster launching of the associated
-- Windows AMI.
--
-- 'version', 'fastLaunchLaunchTemplateSpecificationResponse_version' - The version of the launch template for faster launching of the
-- associated Windows AMI.
newFastLaunchLaunchTemplateSpecificationResponse ::
  FastLaunchLaunchTemplateSpecificationResponse
newFastLaunchLaunchTemplateSpecificationResponse =
  FastLaunchLaunchTemplateSpecificationResponse'
    { launchTemplateId =
        Prelude.Nothing,
      launchTemplateName =
        Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The ID of the launch template for faster launching of the associated
-- Windows AMI.
fastLaunchLaunchTemplateSpecificationResponse_launchTemplateId :: Lens.Lens' FastLaunchLaunchTemplateSpecificationResponse (Prelude.Maybe Prelude.Text)
fastLaunchLaunchTemplateSpecificationResponse_launchTemplateId = Lens.lens (\FastLaunchLaunchTemplateSpecificationResponse' {launchTemplateId} -> launchTemplateId) (\s@FastLaunchLaunchTemplateSpecificationResponse' {} a -> s {launchTemplateId = a} :: FastLaunchLaunchTemplateSpecificationResponse)

-- | The name of the launch template for faster launching of the associated
-- Windows AMI.
fastLaunchLaunchTemplateSpecificationResponse_launchTemplateName :: Lens.Lens' FastLaunchLaunchTemplateSpecificationResponse (Prelude.Maybe Prelude.Text)
fastLaunchLaunchTemplateSpecificationResponse_launchTemplateName = Lens.lens (\FastLaunchLaunchTemplateSpecificationResponse' {launchTemplateName} -> launchTemplateName) (\s@FastLaunchLaunchTemplateSpecificationResponse' {} a -> s {launchTemplateName = a} :: FastLaunchLaunchTemplateSpecificationResponse)

-- | The version of the launch template for faster launching of the
-- associated Windows AMI.
fastLaunchLaunchTemplateSpecificationResponse_version :: Lens.Lens' FastLaunchLaunchTemplateSpecificationResponse (Prelude.Maybe Prelude.Text)
fastLaunchLaunchTemplateSpecificationResponse_version = Lens.lens (\FastLaunchLaunchTemplateSpecificationResponse' {version} -> version) (\s@FastLaunchLaunchTemplateSpecificationResponse' {} a -> s {version = a} :: FastLaunchLaunchTemplateSpecificationResponse)

instance
  Data.FromXML
    FastLaunchLaunchTemplateSpecificationResponse
  where
  parseXML x =
    FastLaunchLaunchTemplateSpecificationResponse'
      Prelude.<$> (x Data..@? "launchTemplateId")
      Prelude.<*> (x Data..@? "launchTemplateName")
      Prelude.<*> (x Data..@? "version")

instance
  Prelude.Hashable
    FastLaunchLaunchTemplateSpecificationResponse
  where
  hashWithSalt
    _salt
    FastLaunchLaunchTemplateSpecificationResponse' {..} =
      _salt
        `Prelude.hashWithSalt` launchTemplateId
        `Prelude.hashWithSalt` launchTemplateName
        `Prelude.hashWithSalt` version

instance
  Prelude.NFData
    FastLaunchLaunchTemplateSpecificationResponse
  where
  rnf
    FastLaunchLaunchTemplateSpecificationResponse' {..} =
      Prelude.rnf launchTemplateId `Prelude.seq`
        Prelude.rnf launchTemplateName `Prelude.seq`
          Prelude.rnf version
