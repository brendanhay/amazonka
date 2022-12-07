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
-- Module      : Amazonka.PinpointSmsVoiceV2.UpdatePool
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration of an existing pool. You can update the
-- opt-out list, enable or disable two-way messaging, change the
-- @TwoWayChannelArn@, enable or disable self-managed opt-outs, enable or
-- disable deletion protection, and enable or disable shared routes.
module Amazonka.PinpointSmsVoiceV2.UpdatePool
  ( -- * Creating a Request
    UpdatePool (..),
    newUpdatePool,

    -- * Request Lenses
    updatePool_deletionProtectionEnabled,
    updatePool_selfManagedOptOutsEnabled,
    updatePool_twoWayEnabled,
    updatePool_optOutListName,
    updatePool_twoWayChannelArn,
    updatePool_sharedRoutesEnabled,
    updatePool_poolId,

    -- * Destructuring the Response
    UpdatePoolResponse (..),
    newUpdatePoolResponse,

    -- * Response Lenses
    updatePoolResponse_deletionProtectionEnabled,
    updatePoolResponse_poolArn,
    updatePoolResponse_messageType,
    updatePoolResponse_selfManagedOptOutsEnabled,
    updatePoolResponse_createdTimestamp,
    updatePoolResponse_status,
    updatePoolResponse_twoWayEnabled,
    updatePoolResponse_optOutListName,
    updatePoolResponse_poolId,
    updatePoolResponse_twoWayChannelArn,
    updatePoolResponse_sharedRoutesEnabled,
    updatePoolResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSmsVoiceV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdatePool' smart constructor.
data UpdatePool = UpdatePool'
  { -- | When set to true the pool can\'t be deleted.
    deletionProtectionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | By default this is set to false. When an end recipient sends a message
    -- that begins with HELP or STOP to one of your dedicated numbers, Amazon
    -- Pinpoint automatically replies with a customizable message and adds the
    -- end recipient to the OptOutList. When set to true you\'re responsible
    -- for responding to HELP and STOP requests. You\'re also responsible for
    -- tracking and honoring opt-out requests.
    selfManagedOptOutsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | By default this is set to false. When set to true you can receive
    -- incoming text messages from your end recipients.
    twoWayEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The OptOutList to associate with the pool. Valid values are either
    -- OptOutListName or OptOutListArn.
    optOutListName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the two way channel.
    twoWayChannelArn :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether shared routes are enabled for the pool.
    sharedRoutesEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The unique identifier of the pool to update. Valid values are either the
    -- PoolId or PoolArn.
    poolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deletionProtectionEnabled', 'updatePool_deletionProtectionEnabled' - When set to true the pool can\'t be deleted.
--
-- 'selfManagedOptOutsEnabled', 'updatePool_selfManagedOptOutsEnabled' - By default this is set to false. When an end recipient sends a message
-- that begins with HELP or STOP to one of your dedicated numbers, Amazon
-- Pinpoint automatically replies with a customizable message and adds the
-- end recipient to the OptOutList. When set to true you\'re responsible
-- for responding to HELP and STOP requests. You\'re also responsible for
-- tracking and honoring opt-out requests.
--
-- 'twoWayEnabled', 'updatePool_twoWayEnabled' - By default this is set to false. When set to true you can receive
-- incoming text messages from your end recipients.
--
-- 'optOutListName', 'updatePool_optOutListName' - The OptOutList to associate with the pool. Valid values are either
-- OptOutListName or OptOutListArn.
--
-- 'twoWayChannelArn', 'updatePool_twoWayChannelArn' - The Amazon Resource Name (ARN) of the two way channel.
--
-- 'sharedRoutesEnabled', 'updatePool_sharedRoutesEnabled' - Indicates whether shared routes are enabled for the pool.
--
-- 'poolId', 'updatePool_poolId' - The unique identifier of the pool to update. Valid values are either the
-- PoolId or PoolArn.
newUpdatePool ::
  -- | 'poolId'
  Prelude.Text ->
  UpdatePool
newUpdatePool pPoolId_ =
  UpdatePool'
    { deletionProtectionEnabled =
        Prelude.Nothing,
      selfManagedOptOutsEnabled = Prelude.Nothing,
      twoWayEnabled = Prelude.Nothing,
      optOutListName = Prelude.Nothing,
      twoWayChannelArn = Prelude.Nothing,
      sharedRoutesEnabled = Prelude.Nothing,
      poolId = pPoolId_
    }

-- | When set to true the pool can\'t be deleted.
updatePool_deletionProtectionEnabled :: Lens.Lens' UpdatePool (Prelude.Maybe Prelude.Bool)
updatePool_deletionProtectionEnabled = Lens.lens (\UpdatePool' {deletionProtectionEnabled} -> deletionProtectionEnabled) (\s@UpdatePool' {} a -> s {deletionProtectionEnabled = a} :: UpdatePool)

-- | By default this is set to false. When an end recipient sends a message
-- that begins with HELP or STOP to one of your dedicated numbers, Amazon
-- Pinpoint automatically replies with a customizable message and adds the
-- end recipient to the OptOutList. When set to true you\'re responsible
-- for responding to HELP and STOP requests. You\'re also responsible for
-- tracking and honoring opt-out requests.
updatePool_selfManagedOptOutsEnabled :: Lens.Lens' UpdatePool (Prelude.Maybe Prelude.Bool)
updatePool_selfManagedOptOutsEnabled = Lens.lens (\UpdatePool' {selfManagedOptOutsEnabled} -> selfManagedOptOutsEnabled) (\s@UpdatePool' {} a -> s {selfManagedOptOutsEnabled = a} :: UpdatePool)

-- | By default this is set to false. When set to true you can receive
-- incoming text messages from your end recipients.
updatePool_twoWayEnabled :: Lens.Lens' UpdatePool (Prelude.Maybe Prelude.Bool)
updatePool_twoWayEnabled = Lens.lens (\UpdatePool' {twoWayEnabled} -> twoWayEnabled) (\s@UpdatePool' {} a -> s {twoWayEnabled = a} :: UpdatePool)

-- | The OptOutList to associate with the pool. Valid values are either
-- OptOutListName or OptOutListArn.
updatePool_optOutListName :: Lens.Lens' UpdatePool (Prelude.Maybe Prelude.Text)
updatePool_optOutListName = Lens.lens (\UpdatePool' {optOutListName} -> optOutListName) (\s@UpdatePool' {} a -> s {optOutListName = a} :: UpdatePool)

-- | The Amazon Resource Name (ARN) of the two way channel.
updatePool_twoWayChannelArn :: Lens.Lens' UpdatePool (Prelude.Maybe Prelude.Text)
updatePool_twoWayChannelArn = Lens.lens (\UpdatePool' {twoWayChannelArn} -> twoWayChannelArn) (\s@UpdatePool' {} a -> s {twoWayChannelArn = a} :: UpdatePool)

-- | Indicates whether shared routes are enabled for the pool.
updatePool_sharedRoutesEnabled :: Lens.Lens' UpdatePool (Prelude.Maybe Prelude.Bool)
updatePool_sharedRoutesEnabled = Lens.lens (\UpdatePool' {sharedRoutesEnabled} -> sharedRoutesEnabled) (\s@UpdatePool' {} a -> s {sharedRoutesEnabled = a} :: UpdatePool)

-- | The unique identifier of the pool to update. Valid values are either the
-- PoolId or PoolArn.
updatePool_poolId :: Lens.Lens' UpdatePool Prelude.Text
updatePool_poolId = Lens.lens (\UpdatePool' {poolId} -> poolId) (\s@UpdatePool' {} a -> s {poolId = a} :: UpdatePool)

instance Core.AWSRequest UpdatePool where
  type AWSResponse UpdatePool = UpdatePoolResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePoolResponse'
            Prelude.<$> (x Data..?> "DeletionProtectionEnabled")
            Prelude.<*> (x Data..?> "PoolArn")
            Prelude.<*> (x Data..?> "MessageType")
            Prelude.<*> (x Data..?> "SelfManagedOptOutsEnabled")
            Prelude.<*> (x Data..?> "CreatedTimestamp")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "TwoWayEnabled")
            Prelude.<*> (x Data..?> "OptOutListName")
            Prelude.<*> (x Data..?> "PoolId")
            Prelude.<*> (x Data..?> "TwoWayChannelArn")
            Prelude.<*> (x Data..?> "SharedRoutesEnabled")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdatePool where
  hashWithSalt _salt UpdatePool' {..} =
    _salt
      `Prelude.hashWithSalt` deletionProtectionEnabled
      `Prelude.hashWithSalt` selfManagedOptOutsEnabled
      `Prelude.hashWithSalt` twoWayEnabled
      `Prelude.hashWithSalt` optOutListName
      `Prelude.hashWithSalt` twoWayChannelArn
      `Prelude.hashWithSalt` sharedRoutesEnabled
      `Prelude.hashWithSalt` poolId

instance Prelude.NFData UpdatePool where
  rnf UpdatePool' {..} =
    Prelude.rnf deletionProtectionEnabled
      `Prelude.seq` Prelude.rnf selfManagedOptOutsEnabled
      `Prelude.seq` Prelude.rnf twoWayEnabled
      `Prelude.seq` Prelude.rnf optOutListName
      `Prelude.seq` Prelude.rnf twoWayChannelArn
      `Prelude.seq` Prelude.rnf sharedRoutesEnabled
      `Prelude.seq` Prelude.rnf poolId

instance Data.ToHeaders UpdatePool where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PinpointSMSVoiceV2.UpdatePool" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdatePool where
  toJSON UpdatePool' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DeletionProtectionEnabled" Data..=)
              Prelude.<$> deletionProtectionEnabled,
            ("SelfManagedOptOutsEnabled" Data..=)
              Prelude.<$> selfManagedOptOutsEnabled,
            ("TwoWayEnabled" Data..=) Prelude.<$> twoWayEnabled,
            ("OptOutListName" Data..=)
              Prelude.<$> optOutListName,
            ("TwoWayChannelArn" Data..=)
              Prelude.<$> twoWayChannelArn,
            ("SharedRoutesEnabled" Data..=)
              Prelude.<$> sharedRoutesEnabled,
            Prelude.Just ("PoolId" Data..= poolId)
          ]
      )

instance Data.ToPath UpdatePool where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdatePool where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdatePoolResponse' smart constructor.
data UpdatePoolResponse = UpdatePoolResponse'
  { -- | When set to true the pool can\'t be deleted.
    deletionProtectionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the pool.
    poolArn :: Prelude.Maybe Prelude.Text,
    -- | The type of message for the pool to use.
    messageType :: Prelude.Maybe MessageType,
    -- | When an end recipient sends a message that begins with HELP or STOP to
    -- one of your dedicated numbers, Amazon Pinpoint automatically replies
    -- with a customizable message and adds the end recipient to the
    -- OptOutList. When set to true you\'re responsible for responding to HELP
    -- and STOP requests. You\'re also responsible for tracking and honoring
    -- opt-out requests.
    selfManagedOptOutsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The time when the pool was created, in
    -- <https://www.epochconverter.com/ UNIX epoch time> format.
    createdTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The current status of the pool update request.
    status :: Prelude.Maybe PoolStatus,
    -- | By default this is set to false. When set to true you can receive
    -- incoming text messages from your end recipients.
    twoWayEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The name of the OptOutList associated with the pool.
    optOutListName :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the pool.
    poolId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the two way channel.
    twoWayChannelArn :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether shared routes are enabled for the pool.
    sharedRoutesEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePoolResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deletionProtectionEnabled', 'updatePoolResponse_deletionProtectionEnabled' - When set to true the pool can\'t be deleted.
--
-- 'poolArn', 'updatePoolResponse_poolArn' - The ARN of the pool.
--
-- 'messageType', 'updatePoolResponse_messageType' - The type of message for the pool to use.
--
-- 'selfManagedOptOutsEnabled', 'updatePoolResponse_selfManagedOptOutsEnabled' - When an end recipient sends a message that begins with HELP or STOP to
-- one of your dedicated numbers, Amazon Pinpoint automatically replies
-- with a customizable message and adds the end recipient to the
-- OptOutList. When set to true you\'re responsible for responding to HELP
-- and STOP requests. You\'re also responsible for tracking and honoring
-- opt-out requests.
--
-- 'createdTimestamp', 'updatePoolResponse_createdTimestamp' - The time when the pool was created, in
-- <https://www.epochconverter.com/ UNIX epoch time> format.
--
-- 'status', 'updatePoolResponse_status' - The current status of the pool update request.
--
-- 'twoWayEnabled', 'updatePoolResponse_twoWayEnabled' - By default this is set to false. When set to true you can receive
-- incoming text messages from your end recipients.
--
-- 'optOutListName', 'updatePoolResponse_optOutListName' - The name of the OptOutList associated with the pool.
--
-- 'poolId', 'updatePoolResponse_poolId' - The unique identifier of the pool.
--
-- 'twoWayChannelArn', 'updatePoolResponse_twoWayChannelArn' - The Amazon Resource Name (ARN) of the two way channel.
--
-- 'sharedRoutesEnabled', 'updatePoolResponse_sharedRoutesEnabled' - Indicates whether shared routes are enabled for the pool.
--
-- 'httpStatus', 'updatePoolResponse_httpStatus' - The response's http status code.
newUpdatePoolResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdatePoolResponse
newUpdatePoolResponse pHttpStatus_ =
  UpdatePoolResponse'
    { deletionProtectionEnabled =
        Prelude.Nothing,
      poolArn = Prelude.Nothing,
      messageType = Prelude.Nothing,
      selfManagedOptOutsEnabled = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      status = Prelude.Nothing,
      twoWayEnabled = Prelude.Nothing,
      optOutListName = Prelude.Nothing,
      poolId = Prelude.Nothing,
      twoWayChannelArn = Prelude.Nothing,
      sharedRoutesEnabled = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When set to true the pool can\'t be deleted.
updatePoolResponse_deletionProtectionEnabled :: Lens.Lens' UpdatePoolResponse (Prelude.Maybe Prelude.Bool)
updatePoolResponse_deletionProtectionEnabled = Lens.lens (\UpdatePoolResponse' {deletionProtectionEnabled} -> deletionProtectionEnabled) (\s@UpdatePoolResponse' {} a -> s {deletionProtectionEnabled = a} :: UpdatePoolResponse)

-- | The ARN of the pool.
updatePoolResponse_poolArn :: Lens.Lens' UpdatePoolResponse (Prelude.Maybe Prelude.Text)
updatePoolResponse_poolArn = Lens.lens (\UpdatePoolResponse' {poolArn} -> poolArn) (\s@UpdatePoolResponse' {} a -> s {poolArn = a} :: UpdatePoolResponse)

-- | The type of message for the pool to use.
updatePoolResponse_messageType :: Lens.Lens' UpdatePoolResponse (Prelude.Maybe MessageType)
updatePoolResponse_messageType = Lens.lens (\UpdatePoolResponse' {messageType} -> messageType) (\s@UpdatePoolResponse' {} a -> s {messageType = a} :: UpdatePoolResponse)

-- | When an end recipient sends a message that begins with HELP or STOP to
-- one of your dedicated numbers, Amazon Pinpoint automatically replies
-- with a customizable message and adds the end recipient to the
-- OptOutList. When set to true you\'re responsible for responding to HELP
-- and STOP requests. You\'re also responsible for tracking and honoring
-- opt-out requests.
updatePoolResponse_selfManagedOptOutsEnabled :: Lens.Lens' UpdatePoolResponse (Prelude.Maybe Prelude.Bool)
updatePoolResponse_selfManagedOptOutsEnabled = Lens.lens (\UpdatePoolResponse' {selfManagedOptOutsEnabled} -> selfManagedOptOutsEnabled) (\s@UpdatePoolResponse' {} a -> s {selfManagedOptOutsEnabled = a} :: UpdatePoolResponse)

-- | The time when the pool was created, in
-- <https://www.epochconverter.com/ UNIX epoch time> format.
updatePoolResponse_createdTimestamp :: Lens.Lens' UpdatePoolResponse (Prelude.Maybe Prelude.UTCTime)
updatePoolResponse_createdTimestamp = Lens.lens (\UpdatePoolResponse' {createdTimestamp} -> createdTimestamp) (\s@UpdatePoolResponse' {} a -> s {createdTimestamp = a} :: UpdatePoolResponse) Prelude.. Lens.mapping Data._Time

-- | The current status of the pool update request.
updatePoolResponse_status :: Lens.Lens' UpdatePoolResponse (Prelude.Maybe PoolStatus)
updatePoolResponse_status = Lens.lens (\UpdatePoolResponse' {status} -> status) (\s@UpdatePoolResponse' {} a -> s {status = a} :: UpdatePoolResponse)

-- | By default this is set to false. When set to true you can receive
-- incoming text messages from your end recipients.
updatePoolResponse_twoWayEnabled :: Lens.Lens' UpdatePoolResponse (Prelude.Maybe Prelude.Bool)
updatePoolResponse_twoWayEnabled = Lens.lens (\UpdatePoolResponse' {twoWayEnabled} -> twoWayEnabled) (\s@UpdatePoolResponse' {} a -> s {twoWayEnabled = a} :: UpdatePoolResponse)

-- | The name of the OptOutList associated with the pool.
updatePoolResponse_optOutListName :: Lens.Lens' UpdatePoolResponse (Prelude.Maybe Prelude.Text)
updatePoolResponse_optOutListName = Lens.lens (\UpdatePoolResponse' {optOutListName} -> optOutListName) (\s@UpdatePoolResponse' {} a -> s {optOutListName = a} :: UpdatePoolResponse)

-- | The unique identifier of the pool.
updatePoolResponse_poolId :: Lens.Lens' UpdatePoolResponse (Prelude.Maybe Prelude.Text)
updatePoolResponse_poolId = Lens.lens (\UpdatePoolResponse' {poolId} -> poolId) (\s@UpdatePoolResponse' {} a -> s {poolId = a} :: UpdatePoolResponse)

-- | The Amazon Resource Name (ARN) of the two way channel.
updatePoolResponse_twoWayChannelArn :: Lens.Lens' UpdatePoolResponse (Prelude.Maybe Prelude.Text)
updatePoolResponse_twoWayChannelArn = Lens.lens (\UpdatePoolResponse' {twoWayChannelArn} -> twoWayChannelArn) (\s@UpdatePoolResponse' {} a -> s {twoWayChannelArn = a} :: UpdatePoolResponse)

-- | Indicates whether shared routes are enabled for the pool.
updatePoolResponse_sharedRoutesEnabled :: Lens.Lens' UpdatePoolResponse (Prelude.Maybe Prelude.Bool)
updatePoolResponse_sharedRoutesEnabled = Lens.lens (\UpdatePoolResponse' {sharedRoutesEnabled} -> sharedRoutesEnabled) (\s@UpdatePoolResponse' {} a -> s {sharedRoutesEnabled = a} :: UpdatePoolResponse)

-- | The response's http status code.
updatePoolResponse_httpStatus :: Lens.Lens' UpdatePoolResponse Prelude.Int
updatePoolResponse_httpStatus = Lens.lens (\UpdatePoolResponse' {httpStatus} -> httpStatus) (\s@UpdatePoolResponse' {} a -> s {httpStatus = a} :: UpdatePoolResponse)

instance Prelude.NFData UpdatePoolResponse where
  rnf UpdatePoolResponse' {..} =
    Prelude.rnf deletionProtectionEnabled
      `Prelude.seq` Prelude.rnf poolArn
      `Prelude.seq` Prelude.rnf messageType
      `Prelude.seq` Prelude.rnf selfManagedOptOutsEnabled
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf twoWayEnabled
      `Prelude.seq` Prelude.rnf optOutListName
      `Prelude.seq` Prelude.rnf poolId
      `Prelude.seq` Prelude.rnf twoWayChannelArn
      `Prelude.seq` Prelude.rnf sharedRoutesEnabled
      `Prelude.seq` Prelude.rnf httpStatus
