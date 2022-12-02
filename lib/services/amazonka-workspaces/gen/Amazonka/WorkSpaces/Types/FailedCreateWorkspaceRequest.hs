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
-- Module      : Amazonka.WorkSpaces.Types.FailedCreateWorkspaceRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.FailedCreateWorkspaceRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkSpaces.Types.WorkspaceRequest

-- | Describes a WorkSpace that cannot be created.
--
-- /See:/ 'newFailedCreateWorkspaceRequest' smart constructor.
data FailedCreateWorkspaceRequest = FailedCreateWorkspaceRequest'
  { -- | The text of the error message that is returned if the WorkSpace cannot
    -- be created.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | Information about the WorkSpace.
    workspaceRequest :: Prelude.Maybe WorkspaceRequest,
    -- | The error code that is returned if the WorkSpace cannot be created.
    errorCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailedCreateWorkspaceRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorMessage', 'failedCreateWorkspaceRequest_errorMessage' - The text of the error message that is returned if the WorkSpace cannot
-- be created.
--
-- 'workspaceRequest', 'failedCreateWorkspaceRequest_workspaceRequest' - Information about the WorkSpace.
--
-- 'errorCode', 'failedCreateWorkspaceRequest_errorCode' - The error code that is returned if the WorkSpace cannot be created.
newFailedCreateWorkspaceRequest ::
  FailedCreateWorkspaceRequest
newFailedCreateWorkspaceRequest =
  FailedCreateWorkspaceRequest'
    { errorMessage =
        Prelude.Nothing,
      workspaceRequest = Prelude.Nothing,
      errorCode = Prelude.Nothing
    }

-- | The text of the error message that is returned if the WorkSpace cannot
-- be created.
failedCreateWorkspaceRequest_errorMessage :: Lens.Lens' FailedCreateWorkspaceRequest (Prelude.Maybe Prelude.Text)
failedCreateWorkspaceRequest_errorMessage = Lens.lens (\FailedCreateWorkspaceRequest' {errorMessage} -> errorMessage) (\s@FailedCreateWorkspaceRequest' {} a -> s {errorMessage = a} :: FailedCreateWorkspaceRequest)

-- | Information about the WorkSpace.
failedCreateWorkspaceRequest_workspaceRequest :: Lens.Lens' FailedCreateWorkspaceRequest (Prelude.Maybe WorkspaceRequest)
failedCreateWorkspaceRequest_workspaceRequest = Lens.lens (\FailedCreateWorkspaceRequest' {workspaceRequest} -> workspaceRequest) (\s@FailedCreateWorkspaceRequest' {} a -> s {workspaceRequest = a} :: FailedCreateWorkspaceRequest)

-- | The error code that is returned if the WorkSpace cannot be created.
failedCreateWorkspaceRequest_errorCode :: Lens.Lens' FailedCreateWorkspaceRequest (Prelude.Maybe Prelude.Text)
failedCreateWorkspaceRequest_errorCode = Lens.lens (\FailedCreateWorkspaceRequest' {errorCode} -> errorCode) (\s@FailedCreateWorkspaceRequest' {} a -> s {errorCode = a} :: FailedCreateWorkspaceRequest)

instance Data.FromJSON FailedCreateWorkspaceRequest where
  parseJSON =
    Data.withObject
      "FailedCreateWorkspaceRequest"
      ( \x ->
          FailedCreateWorkspaceRequest'
            Prelude.<$> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "WorkspaceRequest")
            Prelude.<*> (x Data..:? "ErrorCode")
      )

instance
  Prelude.Hashable
    FailedCreateWorkspaceRequest
  where
  hashWithSalt _salt FailedCreateWorkspaceRequest' {..} =
    _salt `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` workspaceRequest
      `Prelude.hashWithSalt` errorCode

instance Prelude.NFData FailedCreateWorkspaceRequest where
  rnf FailedCreateWorkspaceRequest' {..} =
    Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf workspaceRequest
      `Prelude.seq` Prelude.rnf errorCode
