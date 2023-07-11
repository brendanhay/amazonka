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
-- Module      : Amazonka.SageMaker.Types.AutoMLJobStepMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AutoMLJobStepMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Metadata for an AutoML job step.
--
-- /See:/ 'newAutoMLJobStepMetadata' smart constructor.
data AutoMLJobStepMetadata = AutoMLJobStepMetadata'
  { -- | The Amazon Resource Name (ARN) of the AutoML job.
    arn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoMLJobStepMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'autoMLJobStepMetadata_arn' - The Amazon Resource Name (ARN) of the AutoML job.
newAutoMLJobStepMetadata ::
  AutoMLJobStepMetadata
newAutoMLJobStepMetadata =
  AutoMLJobStepMetadata' {arn = Prelude.Nothing}

-- | The Amazon Resource Name (ARN) of the AutoML job.
autoMLJobStepMetadata_arn :: Lens.Lens' AutoMLJobStepMetadata (Prelude.Maybe Prelude.Text)
autoMLJobStepMetadata_arn = Lens.lens (\AutoMLJobStepMetadata' {arn} -> arn) (\s@AutoMLJobStepMetadata' {} a -> s {arn = a} :: AutoMLJobStepMetadata)

instance Data.FromJSON AutoMLJobStepMetadata where
  parseJSON =
    Data.withObject
      "AutoMLJobStepMetadata"
      ( \x ->
          AutoMLJobStepMetadata'
            Prelude.<$> (x Data..:? "Arn")
      )

instance Prelude.Hashable AutoMLJobStepMetadata where
  hashWithSalt _salt AutoMLJobStepMetadata' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData AutoMLJobStepMetadata where
  rnf AutoMLJobStepMetadata' {..} = Prelude.rnf arn
