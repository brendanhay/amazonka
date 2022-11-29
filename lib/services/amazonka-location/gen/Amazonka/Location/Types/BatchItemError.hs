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
-- Module      : Amazonka.Location.Types.BatchItemError
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.BatchItemError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Location.Types.BatchItemErrorCode
import qualified Amazonka.Prelude as Prelude

-- | Contains the batch request error details associated with the request.
--
-- /See:/ 'newBatchItemError' smart constructor.
data BatchItemError = BatchItemError'
  { -- | A message with the reason for the batch request error.
    message :: Prelude.Maybe Prelude.Text,
    -- | The error code associated with the batch request error.
    code :: Prelude.Maybe BatchItemErrorCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchItemError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'batchItemError_message' - A message with the reason for the batch request error.
--
-- 'code', 'batchItemError_code' - The error code associated with the batch request error.
newBatchItemError ::
  BatchItemError
newBatchItemError =
  BatchItemError'
    { message = Prelude.Nothing,
      code = Prelude.Nothing
    }

-- | A message with the reason for the batch request error.
batchItemError_message :: Lens.Lens' BatchItemError (Prelude.Maybe Prelude.Text)
batchItemError_message = Lens.lens (\BatchItemError' {message} -> message) (\s@BatchItemError' {} a -> s {message = a} :: BatchItemError)

-- | The error code associated with the batch request error.
batchItemError_code :: Lens.Lens' BatchItemError (Prelude.Maybe BatchItemErrorCode)
batchItemError_code = Lens.lens (\BatchItemError' {code} -> code) (\s@BatchItemError' {} a -> s {code = a} :: BatchItemError)

instance Core.FromJSON BatchItemError where
  parseJSON =
    Core.withObject
      "BatchItemError"
      ( \x ->
          BatchItemError'
            Prelude.<$> (x Core..:? "Message")
            Prelude.<*> (x Core..:? "Code")
      )

instance Prelude.Hashable BatchItemError where
  hashWithSalt _salt BatchItemError' {..} =
    _salt `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` code

instance Prelude.NFData BatchItemError where
  rnf BatchItemError' {..} =
    Prelude.rnf message `Prelude.seq` Prelude.rnf code
