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
-- Module      : Amazonka.LookoutVision.Types.ModelPackagingOutputDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutVision.Types.ModelPackagingOutputDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LookoutVision.Types.GreengrassOutputDetails
import qualified Amazonka.Prelude as Prelude

-- | Information about the output from a model packaging job.
--
-- /See:/ 'newModelPackagingOutputDetails' smart constructor.
data ModelPackagingOutputDetails = ModelPackagingOutputDetails'
  { -- | Information about the AWS IoT Greengrass component in a model packaging
    -- job.
    greengrass :: Prelude.Maybe GreengrassOutputDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelPackagingOutputDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'greengrass', 'modelPackagingOutputDetails_greengrass' - Information about the AWS IoT Greengrass component in a model packaging
-- job.
newModelPackagingOutputDetails ::
  ModelPackagingOutputDetails
newModelPackagingOutputDetails =
  ModelPackagingOutputDetails'
    { greengrass =
        Prelude.Nothing
    }

-- | Information about the AWS IoT Greengrass component in a model packaging
-- job.
modelPackagingOutputDetails_greengrass :: Lens.Lens' ModelPackagingOutputDetails (Prelude.Maybe GreengrassOutputDetails)
modelPackagingOutputDetails_greengrass = Lens.lens (\ModelPackagingOutputDetails' {greengrass} -> greengrass) (\s@ModelPackagingOutputDetails' {} a -> s {greengrass = a} :: ModelPackagingOutputDetails)

instance Core.FromJSON ModelPackagingOutputDetails where
  parseJSON =
    Core.withObject
      "ModelPackagingOutputDetails"
      ( \x ->
          ModelPackagingOutputDetails'
            Prelude.<$> (x Core..:? "Greengrass")
      )

instance Prelude.Hashable ModelPackagingOutputDetails where
  hashWithSalt _salt ModelPackagingOutputDetails' {..} =
    _salt `Prelude.hashWithSalt` greengrass

instance Prelude.NFData ModelPackagingOutputDetails where
  rnf ModelPackagingOutputDetails' {..} =
    Prelude.rnf greengrass
