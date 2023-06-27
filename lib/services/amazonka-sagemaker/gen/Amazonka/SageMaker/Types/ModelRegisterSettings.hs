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
-- Module      : Amazonka.SageMaker.Types.ModelRegisterSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelRegisterSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.FeatureStatus

-- | The model registry settings for the SageMaker Canvas application.
--
-- /See:/ 'newModelRegisterSettings' smart constructor.
data ModelRegisterSettings = ModelRegisterSettings'
  { -- | The Amazon Resource Name (ARN) of the SageMaker model registry account.
    -- Required only to register model versions created by a different
    -- SageMaker Canvas Amazon Web Services account than the Amazon Web
    -- Services account in which SageMaker model registry is set up.
    crossAccountModelRegisterRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Describes whether the integration to the model registry is enabled or
    -- disabled in the Canvas application.
    status :: Prelude.Maybe FeatureStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelRegisterSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crossAccountModelRegisterRoleArn', 'modelRegisterSettings_crossAccountModelRegisterRoleArn' - The Amazon Resource Name (ARN) of the SageMaker model registry account.
-- Required only to register model versions created by a different
-- SageMaker Canvas Amazon Web Services account than the Amazon Web
-- Services account in which SageMaker model registry is set up.
--
-- 'status', 'modelRegisterSettings_status' - Describes whether the integration to the model registry is enabled or
-- disabled in the Canvas application.
newModelRegisterSettings ::
  ModelRegisterSettings
newModelRegisterSettings =
  ModelRegisterSettings'
    { crossAccountModelRegisterRoleArn =
        Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the SageMaker model registry account.
-- Required only to register model versions created by a different
-- SageMaker Canvas Amazon Web Services account than the Amazon Web
-- Services account in which SageMaker model registry is set up.
modelRegisterSettings_crossAccountModelRegisterRoleArn :: Lens.Lens' ModelRegisterSettings (Prelude.Maybe Prelude.Text)
modelRegisterSettings_crossAccountModelRegisterRoleArn = Lens.lens (\ModelRegisterSettings' {crossAccountModelRegisterRoleArn} -> crossAccountModelRegisterRoleArn) (\s@ModelRegisterSettings' {} a -> s {crossAccountModelRegisterRoleArn = a} :: ModelRegisterSettings)

-- | Describes whether the integration to the model registry is enabled or
-- disabled in the Canvas application.
modelRegisterSettings_status :: Lens.Lens' ModelRegisterSettings (Prelude.Maybe FeatureStatus)
modelRegisterSettings_status = Lens.lens (\ModelRegisterSettings' {status} -> status) (\s@ModelRegisterSettings' {} a -> s {status = a} :: ModelRegisterSettings)

instance Data.FromJSON ModelRegisterSettings where
  parseJSON =
    Data.withObject
      "ModelRegisterSettings"
      ( \x ->
          ModelRegisterSettings'
            Prelude.<$> (x Data..:? "CrossAccountModelRegisterRoleArn")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable ModelRegisterSettings where
  hashWithSalt _salt ModelRegisterSettings' {..} =
    _salt
      `Prelude.hashWithSalt` crossAccountModelRegisterRoleArn
      `Prelude.hashWithSalt` status

instance Prelude.NFData ModelRegisterSettings where
  rnf ModelRegisterSettings' {..} =
    Prelude.rnf crossAccountModelRegisterRoleArn
      `Prelude.seq` Prelude.rnf status

instance Data.ToJSON ModelRegisterSettings where
  toJSON ModelRegisterSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CrossAccountModelRegisterRoleArn" Data..=)
              Prelude.<$> crossAccountModelRegisterRoleArn,
            ("Status" Data..=) Prelude.<$> status
          ]
      )
