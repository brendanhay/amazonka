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
-- Module      : Amazonka.EMR.Types.NotebookS3LocationForOutput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.NotebookS3LocationForOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Amazon S3 location that stores the notebook execution input.
--
-- /See:/ 'newNotebookS3LocationForOutput' smart constructor.
data NotebookS3LocationForOutput = NotebookS3LocationForOutput'
  { -- | The Amazon S3 bucket that stores the notebook execution input.
    bucket :: Prelude.Maybe Prelude.Text,
    -- | The key to the Amazon S3 location that stores the notebook execution
    -- input.
    key :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotebookS3LocationForOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucket', 'notebookS3LocationForOutput_bucket' - The Amazon S3 bucket that stores the notebook execution input.
--
-- 'key', 'notebookS3LocationForOutput_key' - The key to the Amazon S3 location that stores the notebook execution
-- input.
newNotebookS3LocationForOutput ::
  NotebookS3LocationForOutput
newNotebookS3LocationForOutput =
  NotebookS3LocationForOutput'
    { bucket =
        Prelude.Nothing,
      key = Prelude.Nothing
    }

-- | The Amazon S3 bucket that stores the notebook execution input.
notebookS3LocationForOutput_bucket :: Lens.Lens' NotebookS3LocationForOutput (Prelude.Maybe Prelude.Text)
notebookS3LocationForOutput_bucket = Lens.lens (\NotebookS3LocationForOutput' {bucket} -> bucket) (\s@NotebookS3LocationForOutput' {} a -> s {bucket = a} :: NotebookS3LocationForOutput)

-- | The key to the Amazon S3 location that stores the notebook execution
-- input.
notebookS3LocationForOutput_key :: Lens.Lens' NotebookS3LocationForOutput (Prelude.Maybe Prelude.Text)
notebookS3LocationForOutput_key = Lens.lens (\NotebookS3LocationForOutput' {key} -> key) (\s@NotebookS3LocationForOutput' {} a -> s {key = a} :: NotebookS3LocationForOutput)

instance Data.FromJSON NotebookS3LocationForOutput where
  parseJSON =
    Data.withObject
      "NotebookS3LocationForOutput"
      ( \x ->
          NotebookS3LocationForOutput'
            Prelude.<$> (x Data..:? "Bucket")
            Prelude.<*> (x Data..:? "Key")
      )

instance Prelude.Hashable NotebookS3LocationForOutput where
  hashWithSalt _salt NotebookS3LocationForOutput' {..} =
    _salt
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` key

instance Prelude.NFData NotebookS3LocationForOutput where
  rnf NotebookS3LocationForOutput' {..} =
    Prelude.rnf bucket `Prelude.seq` Prelude.rnf key
