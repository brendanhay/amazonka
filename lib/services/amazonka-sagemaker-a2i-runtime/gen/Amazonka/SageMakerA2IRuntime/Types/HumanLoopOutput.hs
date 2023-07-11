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
-- Module      : Amazonka.SageMakerA2IRuntime.Types.HumanLoopOutput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerA2IRuntime.Types.HumanLoopOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about where the human output will be stored.
--
-- /See:/ 'newHumanLoopOutput' smart constructor.
data HumanLoopOutput = HumanLoopOutput'
  { -- | The location of the Amazon S3 object where Amazon Augmented AI stores
    -- your human loop output.
    outputS3Uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HumanLoopOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputS3Uri', 'humanLoopOutput_outputS3Uri' - The location of the Amazon S3 object where Amazon Augmented AI stores
-- your human loop output.
newHumanLoopOutput ::
  -- | 'outputS3Uri'
  Prelude.Text ->
  HumanLoopOutput
newHumanLoopOutput pOutputS3Uri_ =
  HumanLoopOutput' {outputS3Uri = pOutputS3Uri_}

-- | The location of the Amazon S3 object where Amazon Augmented AI stores
-- your human loop output.
humanLoopOutput_outputS3Uri :: Lens.Lens' HumanLoopOutput Prelude.Text
humanLoopOutput_outputS3Uri = Lens.lens (\HumanLoopOutput' {outputS3Uri} -> outputS3Uri) (\s@HumanLoopOutput' {} a -> s {outputS3Uri = a} :: HumanLoopOutput)

instance Data.FromJSON HumanLoopOutput where
  parseJSON =
    Data.withObject
      "HumanLoopOutput"
      ( \x ->
          HumanLoopOutput'
            Prelude.<$> (x Data..: "OutputS3Uri")
      )

instance Prelude.Hashable HumanLoopOutput where
  hashWithSalt _salt HumanLoopOutput' {..} =
    _salt `Prelude.hashWithSalt` outputS3Uri

instance Prelude.NFData HumanLoopOutput where
  rnf HumanLoopOutput' {..} = Prelude.rnf outputS3Uri
