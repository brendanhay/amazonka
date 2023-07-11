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
-- Module      : Amazonka.FSx.Types.FileCacheFailureDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.FileCacheFailureDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure providing details of any failures that occurred.
--
-- /See:/ 'newFileCacheFailureDetails' smart constructor.
data FileCacheFailureDetails = FileCacheFailureDetails'
  { -- | A message describing any failures that occurred.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FileCacheFailureDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'fileCacheFailureDetails_message' - A message describing any failures that occurred.
newFileCacheFailureDetails ::
  FileCacheFailureDetails
newFileCacheFailureDetails =
  FileCacheFailureDetails' {message = Prelude.Nothing}

-- | A message describing any failures that occurred.
fileCacheFailureDetails_message :: Lens.Lens' FileCacheFailureDetails (Prelude.Maybe Prelude.Text)
fileCacheFailureDetails_message = Lens.lens (\FileCacheFailureDetails' {message} -> message) (\s@FileCacheFailureDetails' {} a -> s {message = a} :: FileCacheFailureDetails)

instance Data.FromJSON FileCacheFailureDetails where
  parseJSON =
    Data.withObject
      "FileCacheFailureDetails"
      ( \x ->
          FileCacheFailureDetails'
            Prelude.<$> (x Data..:? "Message")
      )

instance Prelude.Hashable FileCacheFailureDetails where
  hashWithSalt _salt FileCacheFailureDetails' {..} =
    _salt `Prelude.hashWithSalt` message

instance Prelude.NFData FileCacheFailureDetails where
  rnf FileCacheFailureDetails' {..} =
    Prelude.rnf message
