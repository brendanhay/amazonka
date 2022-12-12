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
-- Module      : Amazonka.Omics.Types.ReadSetBatchError
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.ReadSetBatchError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An error from a batch read set operation.
--
-- /See:/ 'newReadSetBatchError' smart constructor.
data ReadSetBatchError = ReadSetBatchError'
  { -- | The error\'s code.
    code :: Prelude.Text,
    -- | The error\'s ID.
    id :: Prelude.Text,
    -- | The error\'s message.
    message :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReadSetBatchError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'readSetBatchError_code' - The error\'s code.
--
-- 'id', 'readSetBatchError_id' - The error\'s ID.
--
-- 'message', 'readSetBatchError_message' - The error\'s message.
newReadSetBatchError ::
  -- | 'code'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'message'
  Prelude.Text ->
  ReadSetBatchError
newReadSetBatchError pCode_ pId_ pMessage_ =
  ReadSetBatchError'
    { code = pCode_,
      id = pId_,
      message = pMessage_
    }

-- | The error\'s code.
readSetBatchError_code :: Lens.Lens' ReadSetBatchError Prelude.Text
readSetBatchError_code = Lens.lens (\ReadSetBatchError' {code} -> code) (\s@ReadSetBatchError' {} a -> s {code = a} :: ReadSetBatchError)

-- | The error\'s ID.
readSetBatchError_id :: Lens.Lens' ReadSetBatchError Prelude.Text
readSetBatchError_id = Lens.lens (\ReadSetBatchError' {id} -> id) (\s@ReadSetBatchError' {} a -> s {id = a} :: ReadSetBatchError)

-- | The error\'s message.
readSetBatchError_message :: Lens.Lens' ReadSetBatchError Prelude.Text
readSetBatchError_message = Lens.lens (\ReadSetBatchError' {message} -> message) (\s@ReadSetBatchError' {} a -> s {message = a} :: ReadSetBatchError)

instance Data.FromJSON ReadSetBatchError where
  parseJSON =
    Data.withObject
      "ReadSetBatchError"
      ( \x ->
          ReadSetBatchError'
            Prelude.<$> (x Data..: "code")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "message")
      )

instance Prelude.Hashable ReadSetBatchError where
  hashWithSalt _salt ReadSetBatchError' {..} =
    _salt `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` message

instance Prelude.NFData ReadSetBatchError where
  rnf ReadSetBatchError' {..} =
    Prelude.rnf code
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf message
