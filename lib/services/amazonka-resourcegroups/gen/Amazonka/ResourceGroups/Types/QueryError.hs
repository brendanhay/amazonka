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
-- Module      : Amazonka.ResourceGroups.Types.QueryError
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResourceGroups.Types.QueryError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ResourceGroups.Types.QueryErrorCode

-- | A two-part error structure that can occur in @ListGroupResources@ or
-- @SearchResources@ operations on CloudFormation stack-based queries. The
-- error occurs if the CloudFormation stack on which the query is based
-- either does not exist, or has a status that renders the stack inactive.
-- A @QueryError@ occurrence does not necessarily mean that AWS Resource
-- Groups could not complete the operation, but the resulting group might
-- have no member resources.
--
-- /See:/ 'newQueryError' smart constructor.
data QueryError = QueryError'
  { -- | A message that explains the @ErrorCode@ value. Messages might state that
    -- the specified CloudFormation stack does not exist (or no longer exists).
    -- For @CLOUDFORMATION_STACK_INACTIVE@, the message typically states that
    -- the CloudFormation stack has a status that is not (or no longer) active,
    -- such as @CREATE_FAILED@.
    message :: Prelude.Maybe Prelude.Text,
    -- | Possible values are @CLOUDFORMATION_STACK_INACTIVE@ and
    -- @CLOUDFORMATION_STACK_NOT_EXISTING@.
    errorCode :: Prelude.Maybe QueryErrorCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QueryError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'queryError_message' - A message that explains the @ErrorCode@ value. Messages might state that
-- the specified CloudFormation stack does not exist (or no longer exists).
-- For @CLOUDFORMATION_STACK_INACTIVE@, the message typically states that
-- the CloudFormation stack has a status that is not (or no longer) active,
-- such as @CREATE_FAILED@.
--
-- 'errorCode', 'queryError_errorCode' - Possible values are @CLOUDFORMATION_STACK_INACTIVE@ and
-- @CLOUDFORMATION_STACK_NOT_EXISTING@.
newQueryError ::
  QueryError
newQueryError =
  QueryError'
    { message = Prelude.Nothing,
      errorCode = Prelude.Nothing
    }

-- | A message that explains the @ErrorCode@ value. Messages might state that
-- the specified CloudFormation stack does not exist (or no longer exists).
-- For @CLOUDFORMATION_STACK_INACTIVE@, the message typically states that
-- the CloudFormation stack has a status that is not (or no longer) active,
-- such as @CREATE_FAILED@.
queryError_message :: Lens.Lens' QueryError (Prelude.Maybe Prelude.Text)
queryError_message = Lens.lens (\QueryError' {message} -> message) (\s@QueryError' {} a -> s {message = a} :: QueryError)

-- | Possible values are @CLOUDFORMATION_STACK_INACTIVE@ and
-- @CLOUDFORMATION_STACK_NOT_EXISTING@.
queryError_errorCode :: Lens.Lens' QueryError (Prelude.Maybe QueryErrorCode)
queryError_errorCode = Lens.lens (\QueryError' {errorCode} -> errorCode) (\s@QueryError' {} a -> s {errorCode = a} :: QueryError)

instance Data.FromJSON QueryError where
  parseJSON =
    Data.withObject
      "QueryError"
      ( \x ->
          QueryError'
            Prelude.<$> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "ErrorCode")
      )

instance Prelude.Hashable QueryError where
  hashWithSalt _salt QueryError' {..} =
    _salt `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` errorCode

instance Prelude.NFData QueryError where
  rnf QueryError' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf errorCode
