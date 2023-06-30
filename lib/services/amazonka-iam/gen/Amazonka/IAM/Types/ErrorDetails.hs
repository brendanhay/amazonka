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
-- Module      : Amazonka.IAM.Types.ErrorDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.ErrorDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the reason that the operation failed.
--
-- This data type is used as a response element in the
-- GetOrganizationsAccessReport, GetServiceLastAccessedDetails, and
-- GetServiceLastAccessedDetailsWithEntities operations.
--
-- /See:/ 'newErrorDetails' smart constructor.
data ErrorDetails = ErrorDetails'
  { -- | Detailed information about the reason that the operation failed.
    message :: Prelude.Text,
    -- | The error code associated with the operation failure.
    code :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ErrorDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'errorDetails_message' - Detailed information about the reason that the operation failed.
--
-- 'code', 'errorDetails_code' - The error code associated with the operation failure.
newErrorDetails ::
  -- | 'message'
  Prelude.Text ->
  -- | 'code'
  Prelude.Text ->
  ErrorDetails
newErrorDetails pMessage_ pCode_ =
  ErrorDetails' {message = pMessage_, code = pCode_}

-- | Detailed information about the reason that the operation failed.
errorDetails_message :: Lens.Lens' ErrorDetails Prelude.Text
errorDetails_message = Lens.lens (\ErrorDetails' {message} -> message) (\s@ErrorDetails' {} a -> s {message = a} :: ErrorDetails)

-- | The error code associated with the operation failure.
errorDetails_code :: Lens.Lens' ErrorDetails Prelude.Text
errorDetails_code = Lens.lens (\ErrorDetails' {code} -> code) (\s@ErrorDetails' {} a -> s {code = a} :: ErrorDetails)

instance Data.FromXML ErrorDetails where
  parseXML x =
    ErrorDetails'
      Prelude.<$> (x Data..@ "Message")
      Prelude.<*> (x Data..@ "Code")

instance Prelude.Hashable ErrorDetails where
  hashWithSalt _salt ErrorDetails' {..} =
    _salt
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` code

instance Prelude.NFData ErrorDetails where
  rnf ErrorDetails' {..} =
    Prelude.rnf message `Prelude.seq` Prelude.rnf code
