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
-- Module      : Amazonka.EMR.Types.NotebookS3LocationFromInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.NotebookS3LocationFromInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Amazon S3 location that stores the notebook execution input.
--
-- /See:/ 'newNotebookS3LocationFromInput' smart constructor.
data NotebookS3LocationFromInput = NotebookS3LocationFromInput'
  { -- | The Amazon S3 bucket that stores the notebook execution input.
    bucket :: Prelude.Maybe Prelude.Text,
    -- | The key to the Amazon S3 location that stores the notebook execution
    -- input.
    key :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotebookS3LocationFromInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucket', 'notebookS3LocationFromInput_bucket' - The Amazon S3 bucket that stores the notebook execution input.
--
-- 'key', 'notebookS3LocationFromInput_key' - The key to the Amazon S3 location that stores the notebook execution
-- input.
newNotebookS3LocationFromInput ::
  NotebookS3LocationFromInput
newNotebookS3LocationFromInput =
  NotebookS3LocationFromInput'
    { bucket =
        Prelude.Nothing,
      key = Prelude.Nothing
    }

-- | The Amazon S3 bucket that stores the notebook execution input.
notebookS3LocationFromInput_bucket :: Lens.Lens' NotebookS3LocationFromInput (Prelude.Maybe Prelude.Text)
notebookS3LocationFromInput_bucket = Lens.lens (\NotebookS3LocationFromInput' {bucket} -> bucket) (\s@NotebookS3LocationFromInput' {} a -> s {bucket = a} :: NotebookS3LocationFromInput)

-- | The key to the Amazon S3 location that stores the notebook execution
-- input.
notebookS3LocationFromInput_key :: Lens.Lens' NotebookS3LocationFromInput (Prelude.Maybe Prelude.Text)
notebookS3LocationFromInput_key = Lens.lens (\NotebookS3LocationFromInput' {key} -> key) (\s@NotebookS3LocationFromInput' {} a -> s {key = a} :: NotebookS3LocationFromInput)

instance Prelude.Hashable NotebookS3LocationFromInput where
  hashWithSalt _salt NotebookS3LocationFromInput' {..} =
    _salt
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` key

instance Prelude.NFData NotebookS3LocationFromInput where
  rnf NotebookS3LocationFromInput' {..} =
    Prelude.rnf bucket `Prelude.seq` Prelude.rnf key

instance Data.ToJSON NotebookS3LocationFromInput where
  toJSON NotebookS3LocationFromInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Bucket" Data..=) Prelude.<$> bucket,
            ("Key" Data..=) Prelude.<$> key
          ]
      )
