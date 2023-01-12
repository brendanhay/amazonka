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
-- Module      : Amazonka.SageMaker.Types.TuningJobStepMetaData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TuningJobStepMetaData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Metadata for a tuning step.
--
-- /See:/ 'newTuningJobStepMetaData' smart constructor.
data TuningJobStepMetaData = TuningJobStepMetaData'
  { -- | The Amazon Resource Name (ARN) of the tuning job that was run by this
    -- step execution.
    arn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TuningJobStepMetaData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'tuningJobStepMetaData_arn' - The Amazon Resource Name (ARN) of the tuning job that was run by this
-- step execution.
newTuningJobStepMetaData ::
  TuningJobStepMetaData
newTuningJobStepMetaData =
  TuningJobStepMetaData' {arn = Prelude.Nothing}

-- | The Amazon Resource Name (ARN) of the tuning job that was run by this
-- step execution.
tuningJobStepMetaData_arn :: Lens.Lens' TuningJobStepMetaData (Prelude.Maybe Prelude.Text)
tuningJobStepMetaData_arn = Lens.lens (\TuningJobStepMetaData' {arn} -> arn) (\s@TuningJobStepMetaData' {} a -> s {arn = a} :: TuningJobStepMetaData)

instance Data.FromJSON TuningJobStepMetaData where
  parseJSON =
    Data.withObject
      "TuningJobStepMetaData"
      ( \x ->
          TuningJobStepMetaData'
            Prelude.<$> (x Data..:? "Arn")
      )

instance Prelude.Hashable TuningJobStepMetaData where
  hashWithSalt _salt TuningJobStepMetaData' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData TuningJobStepMetaData where
  rnf TuningJobStepMetaData' {..} = Prelude.rnf arn
