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
-- Module      : Amazonka.Transfer.Types.ExecutionError
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.ExecutionError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transfer.Types.ExecutionErrorType

-- | Specifies the error message and type, for an error that occurs during
-- the execution of the workflow.
--
-- /See:/ 'newExecutionError' smart constructor.
data ExecutionError = ExecutionError'
  { -- | Specifies the error type.
    --
    -- -   @ALREADY_EXISTS@: occurs for a copy step, if the overwrite option is
    --     not selected and a file with the same name already exists in the
    --     target location.
    --
    -- -   @BAD_REQUEST@: a general bad request: for example, a step that
    --     attempts to tag an EFS file returns @BAD_REQUEST@, as only S3 files
    --     can be tagged.
    --
    -- -   @CUSTOM_STEP_FAILED@: occurs when the custom step provided a
    --     callback that indicates failure.
    --
    -- -   @INTERNAL_SERVER_ERROR@: a catch-all error that can occur for a
    --     variety of reasons.
    --
    -- -   @NOT_FOUND@: occurs when a requested entity, for example a source
    --     file for a copy step, does not exist.
    --
    -- -   @PERMISSION_DENIED@: occurs if your policy does not contain the
    --     correct permissions to complete one or more of the steps in the
    --     workflow.
    --
    -- -   @TIMEOUT@: occurs when the execution times out.
    --
    --     You can set the @TimeoutSeconds@ for a custom step, anywhere from 1
    --     second to 1800 seconds (30 minutes).
    --
    -- -   @THROTTLED@: occurs if you exceed the new execution refill rate of
    --     one workflow per second.
    type' :: ExecutionErrorType,
    -- | Specifies the descriptive message that corresponds to the @ErrorType@.
    message :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExecutionError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'executionError_type' - Specifies the error type.
--
-- -   @ALREADY_EXISTS@: occurs for a copy step, if the overwrite option is
--     not selected and a file with the same name already exists in the
--     target location.
--
-- -   @BAD_REQUEST@: a general bad request: for example, a step that
--     attempts to tag an EFS file returns @BAD_REQUEST@, as only S3 files
--     can be tagged.
--
-- -   @CUSTOM_STEP_FAILED@: occurs when the custom step provided a
--     callback that indicates failure.
--
-- -   @INTERNAL_SERVER_ERROR@: a catch-all error that can occur for a
--     variety of reasons.
--
-- -   @NOT_FOUND@: occurs when a requested entity, for example a source
--     file for a copy step, does not exist.
--
-- -   @PERMISSION_DENIED@: occurs if your policy does not contain the
--     correct permissions to complete one or more of the steps in the
--     workflow.
--
-- -   @TIMEOUT@: occurs when the execution times out.
--
--     You can set the @TimeoutSeconds@ for a custom step, anywhere from 1
--     second to 1800 seconds (30 minutes).
--
-- -   @THROTTLED@: occurs if you exceed the new execution refill rate of
--     one workflow per second.
--
-- 'message', 'executionError_message' - Specifies the descriptive message that corresponds to the @ErrorType@.
newExecutionError ::
  -- | 'type''
  ExecutionErrorType ->
  -- | 'message'
  Prelude.Text ->
  ExecutionError
newExecutionError pType_ pMessage_ =
  ExecutionError'
    { type' = pType_,
      message = pMessage_
    }

-- | Specifies the error type.
--
-- -   @ALREADY_EXISTS@: occurs for a copy step, if the overwrite option is
--     not selected and a file with the same name already exists in the
--     target location.
--
-- -   @BAD_REQUEST@: a general bad request: for example, a step that
--     attempts to tag an EFS file returns @BAD_REQUEST@, as only S3 files
--     can be tagged.
--
-- -   @CUSTOM_STEP_FAILED@: occurs when the custom step provided a
--     callback that indicates failure.
--
-- -   @INTERNAL_SERVER_ERROR@: a catch-all error that can occur for a
--     variety of reasons.
--
-- -   @NOT_FOUND@: occurs when a requested entity, for example a source
--     file for a copy step, does not exist.
--
-- -   @PERMISSION_DENIED@: occurs if your policy does not contain the
--     correct permissions to complete one or more of the steps in the
--     workflow.
--
-- -   @TIMEOUT@: occurs when the execution times out.
--
--     You can set the @TimeoutSeconds@ for a custom step, anywhere from 1
--     second to 1800 seconds (30 minutes).
--
-- -   @THROTTLED@: occurs if you exceed the new execution refill rate of
--     one workflow per second.
executionError_type :: Lens.Lens' ExecutionError ExecutionErrorType
executionError_type = Lens.lens (\ExecutionError' {type'} -> type') (\s@ExecutionError' {} a -> s {type' = a} :: ExecutionError)

-- | Specifies the descriptive message that corresponds to the @ErrorType@.
executionError_message :: Lens.Lens' ExecutionError Prelude.Text
executionError_message = Lens.lens (\ExecutionError' {message} -> message) (\s@ExecutionError' {} a -> s {message = a} :: ExecutionError)

instance Data.FromJSON ExecutionError where
  parseJSON =
    Data.withObject
      "ExecutionError"
      ( \x ->
          ExecutionError'
            Prelude.<$> (x Data..: "Type") Prelude.<*> (x Data..: "Message")
      )

instance Prelude.Hashable ExecutionError where
  hashWithSalt _salt ExecutionError' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` message

instance Prelude.NFData ExecutionError where
  rnf ExecutionError' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf message
