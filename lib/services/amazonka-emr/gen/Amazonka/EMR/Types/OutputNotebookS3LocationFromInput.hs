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
-- Module      : Amazonka.EMR.Types.OutputNotebookS3LocationFromInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.OutputNotebookS3LocationFromInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Amazon S3 location that stores the notebook execution output.
--
-- /See:/ 'newOutputNotebookS3LocationFromInput' smart constructor.
data OutputNotebookS3LocationFromInput = OutputNotebookS3LocationFromInput'
  { -- | The Amazon S3 bucket that stores the notebook execution output.
    bucket :: Prelude.Maybe Prelude.Text,
    -- | The key to the Amazon S3 location that stores the notebook execution
    -- output.
    key :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OutputNotebookS3LocationFromInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucket', 'outputNotebookS3LocationFromInput_bucket' - The Amazon S3 bucket that stores the notebook execution output.
--
-- 'key', 'outputNotebookS3LocationFromInput_key' - The key to the Amazon S3 location that stores the notebook execution
-- output.
newOutputNotebookS3LocationFromInput ::
  OutputNotebookS3LocationFromInput
newOutputNotebookS3LocationFromInput =
  OutputNotebookS3LocationFromInput'
    { bucket =
        Prelude.Nothing,
      key = Prelude.Nothing
    }

-- | The Amazon S3 bucket that stores the notebook execution output.
outputNotebookS3LocationFromInput_bucket :: Lens.Lens' OutputNotebookS3LocationFromInput (Prelude.Maybe Prelude.Text)
outputNotebookS3LocationFromInput_bucket = Lens.lens (\OutputNotebookS3LocationFromInput' {bucket} -> bucket) (\s@OutputNotebookS3LocationFromInput' {} a -> s {bucket = a} :: OutputNotebookS3LocationFromInput)

-- | The key to the Amazon S3 location that stores the notebook execution
-- output.
outputNotebookS3LocationFromInput_key :: Lens.Lens' OutputNotebookS3LocationFromInput (Prelude.Maybe Prelude.Text)
outputNotebookS3LocationFromInput_key = Lens.lens (\OutputNotebookS3LocationFromInput' {key} -> key) (\s@OutputNotebookS3LocationFromInput' {} a -> s {key = a} :: OutputNotebookS3LocationFromInput)

instance
  Prelude.Hashable
    OutputNotebookS3LocationFromInput
  where
  hashWithSalt
    _salt
    OutputNotebookS3LocationFromInput' {..} =
      _salt
        `Prelude.hashWithSalt` bucket
        `Prelude.hashWithSalt` key

instance
  Prelude.NFData
    OutputNotebookS3LocationFromInput
  where
  rnf OutputNotebookS3LocationFromInput' {..} =
    Prelude.rnf bucket `Prelude.seq` Prelude.rnf key

instance
  Data.ToJSON
    OutputNotebookS3LocationFromInput
  where
  toJSON OutputNotebookS3LocationFromInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Bucket" Data..=) Prelude.<$> bucket,
            ("Key" Data..=) Prelude.<$> key
          ]
      )
