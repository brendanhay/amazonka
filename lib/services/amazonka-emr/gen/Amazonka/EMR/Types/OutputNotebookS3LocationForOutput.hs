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
-- Module      : Amazonka.EMR.Types.OutputNotebookS3LocationForOutput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.OutputNotebookS3LocationForOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Amazon S3 location that stores the notebook execution output.
--
-- /See:/ 'newOutputNotebookS3LocationForOutput' smart constructor.
data OutputNotebookS3LocationForOutput = OutputNotebookS3LocationForOutput'
  { -- | The Amazon S3 bucket that stores the notebook execution output.
    bucket :: Prelude.Maybe Prelude.Text,
    -- | The key to the Amazon S3 location that stores the notebook execution
    -- output.
    key :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OutputNotebookS3LocationForOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucket', 'outputNotebookS3LocationForOutput_bucket' - The Amazon S3 bucket that stores the notebook execution output.
--
-- 'key', 'outputNotebookS3LocationForOutput_key' - The key to the Amazon S3 location that stores the notebook execution
-- output.
newOutputNotebookS3LocationForOutput ::
  OutputNotebookS3LocationForOutput
newOutputNotebookS3LocationForOutput =
  OutputNotebookS3LocationForOutput'
    { bucket =
        Prelude.Nothing,
      key = Prelude.Nothing
    }

-- | The Amazon S3 bucket that stores the notebook execution output.
outputNotebookS3LocationForOutput_bucket :: Lens.Lens' OutputNotebookS3LocationForOutput (Prelude.Maybe Prelude.Text)
outputNotebookS3LocationForOutput_bucket = Lens.lens (\OutputNotebookS3LocationForOutput' {bucket} -> bucket) (\s@OutputNotebookS3LocationForOutput' {} a -> s {bucket = a} :: OutputNotebookS3LocationForOutput)

-- | The key to the Amazon S3 location that stores the notebook execution
-- output.
outputNotebookS3LocationForOutput_key :: Lens.Lens' OutputNotebookS3LocationForOutput (Prelude.Maybe Prelude.Text)
outputNotebookS3LocationForOutput_key = Lens.lens (\OutputNotebookS3LocationForOutput' {key} -> key) (\s@OutputNotebookS3LocationForOutput' {} a -> s {key = a} :: OutputNotebookS3LocationForOutput)

instance
  Data.FromJSON
    OutputNotebookS3LocationForOutput
  where
  parseJSON =
    Data.withObject
      "OutputNotebookS3LocationForOutput"
      ( \x ->
          OutputNotebookS3LocationForOutput'
            Prelude.<$> (x Data..:? "Bucket")
            Prelude.<*> (x Data..:? "Key")
      )

instance
  Prelude.Hashable
    OutputNotebookS3LocationForOutput
  where
  hashWithSalt
    _salt
    OutputNotebookS3LocationForOutput' {..} =
      _salt
        `Prelude.hashWithSalt` bucket
        `Prelude.hashWithSalt` key

instance
  Prelude.NFData
    OutputNotebookS3LocationForOutput
  where
  rnf OutputNotebookS3LocationForOutput' {..} =
    Prelude.rnf bucket `Prelude.seq` Prelude.rnf key
