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
-- Module      : Amazonka.CleanRooms.Types.BatchGetSchemaError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Types.BatchGetSchemaError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An error describing why a schema could not be fetched.
--
-- /See:/ 'newBatchGetSchemaError' smart constructor.
data BatchGetSchemaError = BatchGetSchemaError'
  { -- | An error name for the error.
    name :: Prelude.Text,
    -- | An error code for the error.
    code :: Prelude.Text,
    -- | An error message for the error.
    message :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetSchemaError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'batchGetSchemaError_name' - An error name for the error.
--
-- 'code', 'batchGetSchemaError_code' - An error code for the error.
--
-- 'message', 'batchGetSchemaError_message' - An error message for the error.
newBatchGetSchemaError ::
  -- | 'name'
  Prelude.Text ->
  -- | 'code'
  Prelude.Text ->
  -- | 'message'
  Prelude.Text ->
  BatchGetSchemaError
newBatchGetSchemaError pName_ pCode_ pMessage_ =
  BatchGetSchemaError'
    { name = pName_,
      code = pCode_,
      message = pMessage_
    }

-- | An error name for the error.
batchGetSchemaError_name :: Lens.Lens' BatchGetSchemaError Prelude.Text
batchGetSchemaError_name = Lens.lens (\BatchGetSchemaError' {name} -> name) (\s@BatchGetSchemaError' {} a -> s {name = a} :: BatchGetSchemaError)

-- | An error code for the error.
batchGetSchemaError_code :: Lens.Lens' BatchGetSchemaError Prelude.Text
batchGetSchemaError_code = Lens.lens (\BatchGetSchemaError' {code} -> code) (\s@BatchGetSchemaError' {} a -> s {code = a} :: BatchGetSchemaError)

-- | An error message for the error.
batchGetSchemaError_message :: Lens.Lens' BatchGetSchemaError Prelude.Text
batchGetSchemaError_message = Lens.lens (\BatchGetSchemaError' {message} -> message) (\s@BatchGetSchemaError' {} a -> s {message = a} :: BatchGetSchemaError)

instance Data.FromJSON BatchGetSchemaError where
  parseJSON =
    Data.withObject
      "BatchGetSchemaError"
      ( \x ->
          BatchGetSchemaError'
            Prelude.<$> (x Data..: "name")
            Prelude.<*> (x Data..: "code")
            Prelude.<*> (x Data..: "message")
      )

instance Prelude.Hashable BatchGetSchemaError where
  hashWithSalt _salt BatchGetSchemaError' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` message

instance Prelude.NFData BatchGetSchemaError where
  rnf BatchGetSchemaError' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf code
      `Prelude.seq` Prelude.rnf message
