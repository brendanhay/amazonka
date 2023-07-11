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
-- Module      : Amazonka.SageMaker.Types.TransformJobStepMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TransformJobStepMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Metadata for a transform job step.
--
-- /See:/ 'newTransformJobStepMetadata' smart constructor.
data TransformJobStepMetadata = TransformJobStepMetadata'
  { -- | The Amazon Resource Name (ARN) of the transform job that was run by this
    -- step execution.
    arn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransformJobStepMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'transformJobStepMetadata_arn' - The Amazon Resource Name (ARN) of the transform job that was run by this
-- step execution.
newTransformJobStepMetadata ::
  TransformJobStepMetadata
newTransformJobStepMetadata =
  TransformJobStepMetadata' {arn = Prelude.Nothing}

-- | The Amazon Resource Name (ARN) of the transform job that was run by this
-- step execution.
transformJobStepMetadata_arn :: Lens.Lens' TransformJobStepMetadata (Prelude.Maybe Prelude.Text)
transformJobStepMetadata_arn = Lens.lens (\TransformJobStepMetadata' {arn} -> arn) (\s@TransformJobStepMetadata' {} a -> s {arn = a} :: TransformJobStepMetadata)

instance Data.FromJSON TransformJobStepMetadata where
  parseJSON =
    Data.withObject
      "TransformJobStepMetadata"
      ( \x ->
          TransformJobStepMetadata'
            Prelude.<$> (x Data..:? "Arn")
      )

instance Prelude.Hashable TransformJobStepMetadata where
  hashWithSalt _salt TransformJobStepMetadata' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData TransformJobStepMetadata where
  rnf TransformJobStepMetadata' {..} = Prelude.rnf arn
