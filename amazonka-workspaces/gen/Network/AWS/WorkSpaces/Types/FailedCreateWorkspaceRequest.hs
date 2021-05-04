{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.WorkSpaces.Types.FailedCreateWorkspaceRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.FailedCreateWorkspaceRequest where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WorkSpaces.Types.WorkspaceRequest

-- | Describes a WorkSpace that cannot be created.
--
-- /See:/ 'newFailedCreateWorkspaceRequest' smart constructor.
data FailedCreateWorkspaceRequest = FailedCreateWorkspaceRequest'
  { -- | Information about the WorkSpace.
    workspaceRequest :: Prelude.Maybe WorkspaceRequest,
    -- | The text of the error message that is returned if the WorkSpace cannot
    -- be created.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The error code that is returned if the WorkSpace cannot be created.
    errorCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FailedCreateWorkspaceRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workspaceRequest', 'failedCreateWorkspaceRequest_workspaceRequest' - Information about the WorkSpace.
--
-- 'errorMessage', 'failedCreateWorkspaceRequest_errorMessage' - The text of the error message that is returned if the WorkSpace cannot
-- be created.
--
-- 'errorCode', 'failedCreateWorkspaceRequest_errorCode' - The error code that is returned if the WorkSpace cannot be created.
newFailedCreateWorkspaceRequest ::
  FailedCreateWorkspaceRequest
newFailedCreateWorkspaceRequest =
  FailedCreateWorkspaceRequest'
    { workspaceRequest =
        Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      errorCode = Prelude.Nothing
    }

-- | Information about the WorkSpace.
failedCreateWorkspaceRequest_workspaceRequest :: Lens.Lens' FailedCreateWorkspaceRequest (Prelude.Maybe WorkspaceRequest)
failedCreateWorkspaceRequest_workspaceRequest = Lens.lens (\FailedCreateWorkspaceRequest' {workspaceRequest} -> workspaceRequest) (\s@FailedCreateWorkspaceRequest' {} a -> s {workspaceRequest = a} :: FailedCreateWorkspaceRequest)

-- | The text of the error message that is returned if the WorkSpace cannot
-- be created.
failedCreateWorkspaceRequest_errorMessage :: Lens.Lens' FailedCreateWorkspaceRequest (Prelude.Maybe Prelude.Text)
failedCreateWorkspaceRequest_errorMessage = Lens.lens (\FailedCreateWorkspaceRequest' {errorMessage} -> errorMessage) (\s@FailedCreateWorkspaceRequest' {} a -> s {errorMessage = a} :: FailedCreateWorkspaceRequest)

-- | The error code that is returned if the WorkSpace cannot be created.
failedCreateWorkspaceRequest_errorCode :: Lens.Lens' FailedCreateWorkspaceRequest (Prelude.Maybe Prelude.Text)
failedCreateWorkspaceRequest_errorCode = Lens.lens (\FailedCreateWorkspaceRequest' {errorCode} -> errorCode) (\s@FailedCreateWorkspaceRequest' {} a -> s {errorCode = a} :: FailedCreateWorkspaceRequest)

instance
  Prelude.FromJSON
    FailedCreateWorkspaceRequest
  where
  parseJSON =
    Prelude.withObject
      "FailedCreateWorkspaceRequest"
      ( \x ->
          FailedCreateWorkspaceRequest'
            Prelude.<$> (x Prelude..:? "WorkspaceRequest")
            Prelude.<*> (x Prelude..:? "ErrorMessage")
            Prelude.<*> (x Prelude..:? "ErrorCode")
      )

instance
  Prelude.Hashable
    FailedCreateWorkspaceRequest

instance Prelude.NFData FailedCreateWorkspaceRequest
