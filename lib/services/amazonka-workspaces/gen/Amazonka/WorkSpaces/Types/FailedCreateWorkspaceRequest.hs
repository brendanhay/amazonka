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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
  { -- | The error code that is returned if the WorkSpace cannot be created.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | The text of the error message that is returned if the WorkSpace cannot
    -- be created.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | Information about the WorkSpace.
    workspaceRequest :: Prelude.Maybe WorkspaceRequest
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
-- 'errorCode', 'failedCreateWorkspaceRequest_errorCode' - The error code that is returned if the WorkSpace cannot be created.
--
-- 'errorMessage', 'failedCreateWorkspaceRequest_errorMessage' - The text of the error message that is returned if the WorkSpace cannot
-- be created.
--
-- 'workspaceRequest', 'failedCreateWorkspaceRequest_workspaceRequest' - Information about the WorkSpace.
newFailedCreateWorkspaceRequest ::
  FailedCreateWorkspaceRequest
newFailedCreateWorkspaceRequest =
  FailedCreateWorkspaceRequest'
    { errorCode =
        Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      workspaceRequest = Prelude.Nothing
    }

-- | The error code that is returned if the WorkSpace cannot be created.
failedCreateWorkspaceRequest_errorCode :: Lens.Lens' FailedCreateWorkspaceRequest (Prelude.Maybe Prelude.Text)
failedCreateWorkspaceRequest_errorCode = Lens.lens (\FailedCreateWorkspaceRequest' {errorCode} -> errorCode) (\s@FailedCreateWorkspaceRequest' {} a -> s {errorCode = a} :: FailedCreateWorkspaceRequest)

-- | The text of the error message that is returned if the WorkSpace cannot
-- be created.
failedCreateWorkspaceRequest_errorMessage :: Lens.Lens' FailedCreateWorkspaceRequest (Prelude.Maybe Prelude.Text)
failedCreateWorkspaceRequest_errorMessage = Lens.lens (\FailedCreateWorkspaceRequest' {errorMessage} -> errorMessage) (\s@FailedCreateWorkspaceRequest' {} a -> s {errorMessage = a} :: FailedCreateWorkspaceRequest)

-- | Information about the WorkSpace.
failedCreateWorkspaceRequest_workspaceRequest :: Lens.Lens' FailedCreateWorkspaceRequest (Prelude.Maybe WorkspaceRequest)
failedCreateWorkspaceRequest_workspaceRequest = Lens.lens (\FailedCreateWorkspaceRequest' {workspaceRequest} -> workspaceRequest) (\s@FailedCreateWorkspaceRequest' {} a -> s {workspaceRequest = a} :: FailedCreateWorkspaceRequest)

instance Data.FromJSON FailedCreateWorkspaceRequest where
  parseJSON =
    Data.withObject
      "FailedCreateWorkspaceRequest"
      ( \x ->
          FailedCreateWorkspaceRequest'
            Prelude.<$> (x Data..:? "ErrorCode")
            Prelude.<*> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "WorkspaceRequest")
      )

instance
  Prelude.Hashable
    FailedCreateWorkspaceRequest
  where
  hashWithSalt _salt FailedCreateWorkspaceRequest' {..} =
    _salt `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` workspaceRequest

instance Prelude.NFData FailedCreateWorkspaceRequest where
  rnf FailedCreateWorkspaceRequest' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf workspaceRequest
