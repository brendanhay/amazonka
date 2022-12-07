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
-- Module      : Amazonka.FSx.Types.FileSystemFailureDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.FileSystemFailureDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure providing details of any failures that occurred.
--
-- /See:/ 'newFileSystemFailureDetails' smart constructor.
data FileSystemFailureDetails = FileSystemFailureDetails'
  { -- | A message describing any failures that occurred.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FileSystemFailureDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'fileSystemFailureDetails_message' - A message describing any failures that occurred.
newFileSystemFailureDetails ::
  FileSystemFailureDetails
newFileSystemFailureDetails =
  FileSystemFailureDetails'
    { message =
        Prelude.Nothing
    }

-- | A message describing any failures that occurred.
fileSystemFailureDetails_message :: Lens.Lens' FileSystemFailureDetails (Prelude.Maybe Prelude.Text)
fileSystemFailureDetails_message = Lens.lens (\FileSystemFailureDetails' {message} -> message) (\s@FileSystemFailureDetails' {} a -> s {message = a} :: FileSystemFailureDetails)

instance Data.FromJSON FileSystemFailureDetails where
  parseJSON =
    Data.withObject
      "FileSystemFailureDetails"
      ( \x ->
          FileSystemFailureDetails'
            Prelude.<$> (x Data..:? "Message")
      )

instance Prelude.Hashable FileSystemFailureDetails where
  hashWithSalt _salt FileSystemFailureDetails' {..} =
    _salt `Prelude.hashWithSalt` message

instance Prelude.NFData FileSystemFailureDetails where
  rnf FileSystemFailureDetails' {..} =
    Prelude.rnf message
