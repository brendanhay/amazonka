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
-- Module      : Amazonka.SageMaker.Types.TensorBoardAppSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TensorBoardAppSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ResourceSpec

-- | The TensorBoard app settings.
--
-- /See:/ 'newTensorBoardAppSettings' smart constructor.
data TensorBoardAppSettings = TensorBoardAppSettings'
  { -- | The default instance type and the Amazon Resource Name (ARN) of the
    -- SageMaker image created on the instance.
    defaultResourceSpec :: Prelude.Maybe ResourceSpec
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TensorBoardAppSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultResourceSpec', 'tensorBoardAppSettings_defaultResourceSpec' - The default instance type and the Amazon Resource Name (ARN) of the
-- SageMaker image created on the instance.
newTensorBoardAppSettings ::
  TensorBoardAppSettings
newTensorBoardAppSettings =
  TensorBoardAppSettings'
    { defaultResourceSpec =
        Prelude.Nothing
    }

-- | The default instance type and the Amazon Resource Name (ARN) of the
-- SageMaker image created on the instance.
tensorBoardAppSettings_defaultResourceSpec :: Lens.Lens' TensorBoardAppSettings (Prelude.Maybe ResourceSpec)
tensorBoardAppSettings_defaultResourceSpec = Lens.lens (\TensorBoardAppSettings' {defaultResourceSpec} -> defaultResourceSpec) (\s@TensorBoardAppSettings' {} a -> s {defaultResourceSpec = a} :: TensorBoardAppSettings)

instance Data.FromJSON TensorBoardAppSettings where
  parseJSON =
    Data.withObject
      "TensorBoardAppSettings"
      ( \x ->
          TensorBoardAppSettings'
            Prelude.<$> (x Data..:? "DefaultResourceSpec")
      )

instance Prelude.Hashable TensorBoardAppSettings where
  hashWithSalt _salt TensorBoardAppSettings' {..} =
    _salt `Prelude.hashWithSalt` defaultResourceSpec

instance Prelude.NFData TensorBoardAppSettings where
  rnf TensorBoardAppSettings' {..} =
    Prelude.rnf defaultResourceSpec

instance Data.ToJSON TensorBoardAppSettings where
  toJSON TensorBoardAppSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DefaultResourceSpec" Data..=)
              Prelude.<$> defaultResourceSpec
          ]
      )
