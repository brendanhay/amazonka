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
-- Module      : Amazonka.WorkSpaces.Types.FailedCreateStandbyWorkspacesRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.FailedCreateStandbyWorkspacesRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkSpaces.Types.StandbyWorkspace

-- | Describes the Standby WorkSpace that could not be created.
--
-- /See:/ 'newFailedCreateStandbyWorkspacesRequest' smart constructor.
data FailedCreateStandbyWorkspacesRequest = FailedCreateStandbyWorkspacesRequest'
  { -- | The error code that is returned if the Standby WorkSpace could not be
    -- created.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | The text of the error message that is returned if the Standby WorkSpace
    -- could not be created.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | Information about the Standby WorkSpace that could not be created.
    standbyWorkspaceRequest :: Prelude.Maybe StandbyWorkspace
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailedCreateStandbyWorkspacesRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'failedCreateStandbyWorkspacesRequest_errorCode' - The error code that is returned if the Standby WorkSpace could not be
-- created.
--
-- 'errorMessage', 'failedCreateStandbyWorkspacesRequest_errorMessage' - The text of the error message that is returned if the Standby WorkSpace
-- could not be created.
--
-- 'standbyWorkspaceRequest', 'failedCreateStandbyWorkspacesRequest_standbyWorkspaceRequest' - Information about the Standby WorkSpace that could not be created.
newFailedCreateStandbyWorkspacesRequest ::
  FailedCreateStandbyWorkspacesRequest
newFailedCreateStandbyWorkspacesRequest =
  FailedCreateStandbyWorkspacesRequest'
    { errorCode =
        Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      standbyWorkspaceRequest =
        Prelude.Nothing
    }

-- | The error code that is returned if the Standby WorkSpace could not be
-- created.
failedCreateStandbyWorkspacesRequest_errorCode :: Lens.Lens' FailedCreateStandbyWorkspacesRequest (Prelude.Maybe Prelude.Text)
failedCreateStandbyWorkspacesRequest_errorCode = Lens.lens (\FailedCreateStandbyWorkspacesRequest' {errorCode} -> errorCode) (\s@FailedCreateStandbyWorkspacesRequest' {} a -> s {errorCode = a} :: FailedCreateStandbyWorkspacesRequest)

-- | The text of the error message that is returned if the Standby WorkSpace
-- could not be created.
failedCreateStandbyWorkspacesRequest_errorMessage :: Lens.Lens' FailedCreateStandbyWorkspacesRequest (Prelude.Maybe Prelude.Text)
failedCreateStandbyWorkspacesRequest_errorMessage = Lens.lens (\FailedCreateStandbyWorkspacesRequest' {errorMessage} -> errorMessage) (\s@FailedCreateStandbyWorkspacesRequest' {} a -> s {errorMessage = a} :: FailedCreateStandbyWorkspacesRequest)

-- | Information about the Standby WorkSpace that could not be created.
failedCreateStandbyWorkspacesRequest_standbyWorkspaceRequest :: Lens.Lens' FailedCreateStandbyWorkspacesRequest (Prelude.Maybe StandbyWorkspace)
failedCreateStandbyWorkspacesRequest_standbyWorkspaceRequest = Lens.lens (\FailedCreateStandbyWorkspacesRequest' {standbyWorkspaceRequest} -> standbyWorkspaceRequest) (\s@FailedCreateStandbyWorkspacesRequest' {} a -> s {standbyWorkspaceRequest = a} :: FailedCreateStandbyWorkspacesRequest)

instance
  Data.FromJSON
    FailedCreateStandbyWorkspacesRequest
  where
  parseJSON =
    Data.withObject
      "FailedCreateStandbyWorkspacesRequest"
      ( \x ->
          FailedCreateStandbyWorkspacesRequest'
            Prelude.<$> (x Data..:? "ErrorCode")
            Prelude.<*> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "StandbyWorkspaceRequest")
      )

instance
  Prelude.Hashable
    FailedCreateStandbyWorkspacesRequest
  where
  hashWithSalt
    _salt
    FailedCreateStandbyWorkspacesRequest' {..} =
      _salt
        `Prelude.hashWithSalt` errorCode
        `Prelude.hashWithSalt` errorMessage
        `Prelude.hashWithSalt` standbyWorkspaceRequest

instance
  Prelude.NFData
    FailedCreateStandbyWorkspacesRequest
  where
  rnf FailedCreateStandbyWorkspacesRequest' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf standbyWorkspaceRequest
