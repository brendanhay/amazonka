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
-- Module      : Amazonka.SupportApp.DeleteSlackChannelConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Slack channel configuration from your Amazon Web Services
-- account. This operation doesn\'t delete your Slack channel.
module Amazonka.SupportApp.DeleteSlackChannelConfiguration
  ( -- * Creating a Request
    DeleteSlackChannelConfiguration (..),
    newDeleteSlackChannelConfiguration,

    -- * Request Lenses
    deleteSlackChannelConfiguration_channelId,
    deleteSlackChannelConfiguration_teamId,

    -- * Destructuring the Response
    DeleteSlackChannelConfigurationResponse (..),
    newDeleteSlackChannelConfigurationResponse,

    -- * Response Lenses
    deleteSlackChannelConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SupportApp.Types

-- | /See:/ 'newDeleteSlackChannelConfiguration' smart constructor.
data DeleteSlackChannelConfiguration = DeleteSlackChannelConfiguration'
  { -- | The channel ID in Slack. This ID identifies a channel within a Slack
    -- workspace.
    channelId :: Prelude.Text,
    -- | The team ID in Slack. This ID uniquely identifies a Slack workspace,
    -- such as @T012ABCDEFG@.
    teamId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSlackChannelConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelId', 'deleteSlackChannelConfiguration_channelId' - The channel ID in Slack. This ID identifies a channel within a Slack
-- workspace.
--
-- 'teamId', 'deleteSlackChannelConfiguration_teamId' - The team ID in Slack. This ID uniquely identifies a Slack workspace,
-- such as @T012ABCDEFG@.
newDeleteSlackChannelConfiguration ::
  -- | 'channelId'
  Prelude.Text ->
  -- | 'teamId'
  Prelude.Text ->
  DeleteSlackChannelConfiguration
newDeleteSlackChannelConfiguration
  pChannelId_
  pTeamId_ =
    DeleteSlackChannelConfiguration'
      { channelId =
          pChannelId_,
        teamId = pTeamId_
      }

-- | The channel ID in Slack. This ID identifies a channel within a Slack
-- workspace.
deleteSlackChannelConfiguration_channelId :: Lens.Lens' DeleteSlackChannelConfiguration Prelude.Text
deleteSlackChannelConfiguration_channelId = Lens.lens (\DeleteSlackChannelConfiguration' {channelId} -> channelId) (\s@DeleteSlackChannelConfiguration' {} a -> s {channelId = a} :: DeleteSlackChannelConfiguration)

-- | The team ID in Slack. This ID uniquely identifies a Slack workspace,
-- such as @T012ABCDEFG@.
deleteSlackChannelConfiguration_teamId :: Lens.Lens' DeleteSlackChannelConfiguration Prelude.Text
deleteSlackChannelConfiguration_teamId = Lens.lens (\DeleteSlackChannelConfiguration' {teamId} -> teamId) (\s@DeleteSlackChannelConfiguration' {} a -> s {teamId = a} :: DeleteSlackChannelConfiguration)

instance
  Core.AWSRequest
    DeleteSlackChannelConfiguration
  where
  type
    AWSResponse DeleteSlackChannelConfiguration =
      DeleteSlackChannelConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteSlackChannelConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteSlackChannelConfiguration
  where
  hashWithSalt
    _salt
    DeleteSlackChannelConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` channelId
        `Prelude.hashWithSalt` teamId

instance
  Prelude.NFData
    DeleteSlackChannelConfiguration
  where
  rnf DeleteSlackChannelConfiguration' {..} =
    Prelude.rnf channelId
      `Prelude.seq` Prelude.rnf teamId

instance
  Data.ToHeaders
    DeleteSlackChannelConfiguration
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

instance Data.ToJSON DeleteSlackChannelConfiguration where
  toJSON DeleteSlackChannelConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("channelId" Data..= channelId),
            Prelude.Just ("teamId" Data..= teamId)
          ]
      )

instance Data.ToPath DeleteSlackChannelConfiguration where
  toPath =
    Prelude.const
      "/control/delete-slack-channel-configuration"

instance Data.ToQuery DeleteSlackChannelConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSlackChannelConfigurationResponse' smart constructor.
data DeleteSlackChannelConfigurationResponse = DeleteSlackChannelConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSlackChannelConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteSlackChannelConfigurationResponse_httpStatus' - The response's http status code.
newDeleteSlackChannelConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteSlackChannelConfigurationResponse
newDeleteSlackChannelConfigurationResponse
  pHttpStatus_ =
    DeleteSlackChannelConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deleteSlackChannelConfigurationResponse_httpStatus :: Lens.Lens' DeleteSlackChannelConfigurationResponse Prelude.Int
deleteSlackChannelConfigurationResponse_httpStatus = Lens.lens (\DeleteSlackChannelConfigurationResponse' {httpStatus} -> httpStatus) (\s@DeleteSlackChannelConfigurationResponse' {} a -> s {httpStatus = a} :: DeleteSlackChannelConfigurationResponse)

instance
  Prelude.NFData
    DeleteSlackChannelConfigurationResponse
  where
  rnf DeleteSlackChannelConfigurationResponse' {..} =
    Prelude.rnf httpStatus
