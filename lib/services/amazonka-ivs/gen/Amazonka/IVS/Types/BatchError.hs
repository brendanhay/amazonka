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
-- Module      : Amazonka.IVS.Types.BatchError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IVS.Types.BatchError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Error related to a specific channel, specified by its ARN.
--
-- /See:/ 'newBatchError' smart constructor.
data BatchError = BatchError'
  { -- | Channel ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Error code.
    code :: Prelude.Maybe Prelude.Text,
    -- | Error message, determined by the application.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'batchError_arn' - Channel ARN.
--
-- 'code', 'batchError_code' - Error code.
--
-- 'message', 'batchError_message' - Error message, determined by the application.
newBatchError ::
  BatchError
newBatchError =
  BatchError'
    { arn = Prelude.Nothing,
      code = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | Channel ARN.
batchError_arn :: Lens.Lens' BatchError (Prelude.Maybe Prelude.Text)
batchError_arn = Lens.lens (\BatchError' {arn} -> arn) (\s@BatchError' {} a -> s {arn = a} :: BatchError)

-- | Error code.
batchError_code :: Lens.Lens' BatchError (Prelude.Maybe Prelude.Text)
batchError_code = Lens.lens (\BatchError' {code} -> code) (\s@BatchError' {} a -> s {code = a} :: BatchError)

-- | Error message, determined by the application.
batchError_message :: Lens.Lens' BatchError (Prelude.Maybe Prelude.Text)
batchError_message = Lens.lens (\BatchError' {message} -> message) (\s@BatchError' {} a -> s {message = a} :: BatchError)

instance Data.FromJSON BatchError where
  parseJSON =
    Data.withObject
      "BatchError"
      ( \x ->
          BatchError'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "code")
            Prelude.<*> (x Data..:? "message")
      )

instance Prelude.Hashable BatchError where
  hashWithSalt _salt BatchError' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` message

instance Prelude.NFData BatchError where
  rnf BatchError' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf code
      `Prelude.seq` Prelude.rnf message
