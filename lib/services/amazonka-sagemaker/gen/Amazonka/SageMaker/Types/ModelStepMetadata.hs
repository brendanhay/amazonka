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
-- Module      : Amazonka.SageMaker.Types.ModelStepMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelStepMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Metadata for Model steps.
--
-- /See:/ 'newModelStepMetadata' smart constructor.
data ModelStepMetadata = ModelStepMetadata'
  { -- | The Amazon Resource Name (ARN) of the created model.
    arn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelStepMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'modelStepMetadata_arn' - The Amazon Resource Name (ARN) of the created model.
newModelStepMetadata ::
  ModelStepMetadata
newModelStepMetadata =
  ModelStepMetadata' {arn = Prelude.Nothing}

-- | The Amazon Resource Name (ARN) of the created model.
modelStepMetadata_arn :: Lens.Lens' ModelStepMetadata (Prelude.Maybe Prelude.Text)
modelStepMetadata_arn = Lens.lens (\ModelStepMetadata' {arn} -> arn) (\s@ModelStepMetadata' {} a -> s {arn = a} :: ModelStepMetadata)

instance Data.FromJSON ModelStepMetadata where
  parseJSON =
    Data.withObject
      "ModelStepMetadata"
      ( \x ->
          ModelStepMetadata' Prelude.<$> (x Data..:? "Arn")
      )

instance Prelude.Hashable ModelStepMetadata where
  hashWithSalt _salt ModelStepMetadata' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData ModelStepMetadata where
  rnf ModelStepMetadata' {..} = Prelude.rnf arn
