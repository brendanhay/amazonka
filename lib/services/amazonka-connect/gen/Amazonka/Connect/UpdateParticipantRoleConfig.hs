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
-- Module      : Amazonka.Connect.UpdateParticipantRoleConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates timeouts for when human chat participants are to be considered
-- idle, and when agents are automatically disconnected from a chat due to
-- idleness. You can set four timers:
--
-- -   Customer idle timeout
--
-- -   Customer auto-disconnect timeout
--
-- -   Agent idle timeout
--
-- -   Agent auto-disconnect timeout
--
-- For more information about how chat timeouts work, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/setup-chat-timeouts.html Set up chat timeouts for human participants>.
module Amazonka.Connect.UpdateParticipantRoleConfig
  ( -- * Creating a Request
    UpdateParticipantRoleConfig (..),
    newUpdateParticipantRoleConfig,

    -- * Request Lenses
    updateParticipantRoleConfig_instanceId,
    updateParticipantRoleConfig_contactId,
    updateParticipantRoleConfig_channelConfiguration,

    -- * Destructuring the Response
    UpdateParticipantRoleConfigResponse (..),
    newUpdateParticipantRoleConfigResponse,

    -- * Response Lenses
    updateParticipantRoleConfigResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateParticipantRoleConfig' smart constructor.
data UpdateParticipantRoleConfig = UpdateParticipantRoleConfig'
  { -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier of the contact in this instance of Amazon Connect.
    contactId :: Prelude.Text,
    -- | The Amazon Connect channel you want to configure.
    channelConfiguration :: UpdateParticipantRoleConfigChannelInfo
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateParticipantRoleConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'updateParticipantRoleConfig_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'contactId', 'updateParticipantRoleConfig_contactId' - The identifier of the contact in this instance of Amazon Connect.
--
-- 'channelConfiguration', 'updateParticipantRoleConfig_channelConfiguration' - The Amazon Connect channel you want to configure.
newUpdateParticipantRoleConfig ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'contactId'
  Prelude.Text ->
  -- | 'channelConfiguration'
  UpdateParticipantRoleConfigChannelInfo ->
  UpdateParticipantRoleConfig
newUpdateParticipantRoleConfig
  pInstanceId_
  pContactId_
  pChannelConfiguration_ =
    UpdateParticipantRoleConfig'
      { instanceId =
          pInstanceId_,
        contactId = pContactId_,
        channelConfiguration = pChannelConfiguration_
      }

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
updateParticipantRoleConfig_instanceId :: Lens.Lens' UpdateParticipantRoleConfig Prelude.Text
updateParticipantRoleConfig_instanceId = Lens.lens (\UpdateParticipantRoleConfig' {instanceId} -> instanceId) (\s@UpdateParticipantRoleConfig' {} a -> s {instanceId = a} :: UpdateParticipantRoleConfig)

-- | The identifier of the contact in this instance of Amazon Connect.
updateParticipantRoleConfig_contactId :: Lens.Lens' UpdateParticipantRoleConfig Prelude.Text
updateParticipantRoleConfig_contactId = Lens.lens (\UpdateParticipantRoleConfig' {contactId} -> contactId) (\s@UpdateParticipantRoleConfig' {} a -> s {contactId = a} :: UpdateParticipantRoleConfig)

-- | The Amazon Connect channel you want to configure.
updateParticipantRoleConfig_channelConfiguration :: Lens.Lens' UpdateParticipantRoleConfig UpdateParticipantRoleConfigChannelInfo
updateParticipantRoleConfig_channelConfiguration = Lens.lens (\UpdateParticipantRoleConfig' {channelConfiguration} -> channelConfiguration) (\s@UpdateParticipantRoleConfig' {} a -> s {channelConfiguration = a} :: UpdateParticipantRoleConfig)

instance Core.AWSRequest UpdateParticipantRoleConfig where
  type
    AWSResponse UpdateParticipantRoleConfig =
      UpdateParticipantRoleConfigResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateParticipantRoleConfigResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateParticipantRoleConfig where
  hashWithSalt _salt UpdateParticipantRoleConfig' {..} =
    _salt
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` contactId
      `Prelude.hashWithSalt` channelConfiguration

instance Prelude.NFData UpdateParticipantRoleConfig where
  rnf UpdateParticipantRoleConfig' {..} =
    Prelude.rnf instanceId `Prelude.seq`
      Prelude.rnf contactId `Prelude.seq`
        Prelude.rnf channelConfiguration

instance Data.ToHeaders UpdateParticipantRoleConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateParticipantRoleConfig where
  toJSON UpdateParticipantRoleConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ChannelConfiguration"
                  Data..= channelConfiguration
              )
          ]
      )

instance Data.ToPath UpdateParticipantRoleConfig where
  toPath UpdateParticipantRoleConfig' {..} =
    Prelude.mconcat
      [ "/contact/participant-role-config/",
        Data.toBS instanceId,
        "/",
        Data.toBS contactId
      ]

instance Data.ToQuery UpdateParticipantRoleConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateParticipantRoleConfigResponse' smart constructor.
data UpdateParticipantRoleConfigResponse = UpdateParticipantRoleConfigResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateParticipantRoleConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateParticipantRoleConfigResponse_httpStatus' - The response's http status code.
newUpdateParticipantRoleConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateParticipantRoleConfigResponse
newUpdateParticipantRoleConfigResponse pHttpStatus_ =
  UpdateParticipantRoleConfigResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateParticipantRoleConfigResponse_httpStatus :: Lens.Lens' UpdateParticipantRoleConfigResponse Prelude.Int
updateParticipantRoleConfigResponse_httpStatus = Lens.lens (\UpdateParticipantRoleConfigResponse' {httpStatus} -> httpStatus) (\s@UpdateParticipantRoleConfigResponse' {} a -> s {httpStatus = a} :: UpdateParticipantRoleConfigResponse)

instance
  Prelude.NFData
    UpdateParticipantRoleConfigResponse
  where
  rnf UpdateParticipantRoleConfigResponse' {..} =
    Prelude.rnf httpStatus
