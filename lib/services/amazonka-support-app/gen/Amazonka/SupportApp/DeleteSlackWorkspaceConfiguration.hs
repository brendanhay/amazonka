{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SupportApp.DeleteSlackWorkspaceConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Slack workspace configuration from your Amazon Web Services
-- account. This operation doesn\'t delete your Slack workspace.
module Amazonka.SupportApp.DeleteSlackWorkspaceConfiguration
  ( -- * Creating a Request
    DeleteSlackWorkspaceConfiguration (..),
    newDeleteSlackWorkspaceConfiguration,

    -- * Request Lenses
    deleteSlackWorkspaceConfiguration_teamId,

    -- * Destructuring the Response
    DeleteSlackWorkspaceConfigurationResponse (..),
    newDeleteSlackWorkspaceConfigurationResponse,

    -- * Response Lenses
    deleteSlackWorkspaceConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SupportApp.Types

-- | /See:/ 'newDeleteSlackWorkspaceConfiguration' smart constructor.
data DeleteSlackWorkspaceConfiguration = DeleteSlackWorkspaceConfiguration'
  { -- | The team ID in Slack. This ID uniquely identifies a Slack workspace,
    -- such as @T012ABCDEFG@.
    teamId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSlackWorkspaceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'teamId', 'deleteSlackWorkspaceConfiguration_teamId' - The team ID in Slack. This ID uniquely identifies a Slack workspace,
-- such as @T012ABCDEFG@.
newDeleteSlackWorkspaceConfiguration ::
  -- | 'teamId'
  Prelude.Text ->
  DeleteSlackWorkspaceConfiguration
newDeleteSlackWorkspaceConfiguration pTeamId_ =
  DeleteSlackWorkspaceConfiguration'
    { teamId =
        pTeamId_
    }

-- | The team ID in Slack. This ID uniquely identifies a Slack workspace,
-- such as @T012ABCDEFG@.
deleteSlackWorkspaceConfiguration_teamId :: Lens.Lens' DeleteSlackWorkspaceConfiguration Prelude.Text
deleteSlackWorkspaceConfiguration_teamId = Lens.lens (\DeleteSlackWorkspaceConfiguration' {teamId} -> teamId) (\s@DeleteSlackWorkspaceConfiguration' {} a -> s {teamId = a} :: DeleteSlackWorkspaceConfiguration)

instance
  Core.AWSRequest
    DeleteSlackWorkspaceConfiguration
  where
  type
    AWSResponse DeleteSlackWorkspaceConfiguration =
      DeleteSlackWorkspaceConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteSlackWorkspaceConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteSlackWorkspaceConfiguration
  where
  hashWithSalt
    _salt
    DeleteSlackWorkspaceConfiguration' {..} =
      _salt `Prelude.hashWithSalt` teamId

instance
  Prelude.NFData
    DeleteSlackWorkspaceConfiguration
  where
  rnf DeleteSlackWorkspaceConfiguration' {..} =
    Prelude.rnf teamId

instance
  Data.ToHeaders
    DeleteSlackWorkspaceConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DeleteSlackWorkspaceConfiguration
  where
  toJSON DeleteSlackWorkspaceConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("teamId" Data..= teamId)]
      )

instance
  Data.ToPath
    DeleteSlackWorkspaceConfiguration
  where
  toPath =
    Prelude.const
      "/control/delete-slack-workspace-configuration"

instance
  Data.ToQuery
    DeleteSlackWorkspaceConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSlackWorkspaceConfigurationResponse' smart constructor.
data DeleteSlackWorkspaceConfigurationResponse = DeleteSlackWorkspaceConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSlackWorkspaceConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteSlackWorkspaceConfigurationResponse_httpStatus' - The response's http status code.
newDeleteSlackWorkspaceConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteSlackWorkspaceConfigurationResponse
newDeleteSlackWorkspaceConfigurationResponse
  pHttpStatus_ =
    DeleteSlackWorkspaceConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deleteSlackWorkspaceConfigurationResponse_httpStatus :: Lens.Lens' DeleteSlackWorkspaceConfigurationResponse Prelude.Int
deleteSlackWorkspaceConfigurationResponse_httpStatus = Lens.lens (\DeleteSlackWorkspaceConfigurationResponse' {httpStatus} -> httpStatus) (\s@DeleteSlackWorkspaceConfigurationResponse' {} a -> s {httpStatus = a} :: DeleteSlackWorkspaceConfigurationResponse)

instance
  Prelude.NFData
    DeleteSlackWorkspaceConfigurationResponse
  where
  rnf DeleteSlackWorkspaceConfigurationResponse' {..} =
    Prelude.rnf httpStatus
