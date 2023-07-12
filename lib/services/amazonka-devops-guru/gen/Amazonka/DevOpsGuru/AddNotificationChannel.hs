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
-- Module      : Amazonka.DevOpsGuru.AddNotificationChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a notification channel to DevOps Guru. A notification channel is
-- used to notify you about important DevOps Guru events, such as when an
-- insight is generated.
--
-- If you use an Amazon SNS topic in another account, you must attach a
-- policy to it that grants DevOps Guru permission to it notifications.
-- DevOps Guru adds the required policy on your behalf to send
-- notifications using Amazon SNS in your account. DevOps Guru only
-- supports standard SNS topics. For more information, see
-- <https://docs.aws.amazon.com/devops-guru/latest/userguide/sns-required-permissions.html Permissions for cross account Amazon SNS topics>.
--
-- If you use an Amazon SNS topic in another account, you must attach a
-- policy to it that grants DevOps Guru permission to it notifications.
-- DevOps Guru adds the required policy on your behalf to send
-- notifications using Amazon SNS in your account. For more information,
-- see Permissions for cross account Amazon SNS topics.
--
-- If you use an Amazon SNS topic that is encrypted by an Amazon Web
-- Services Key Management Service customer-managed key (CMK), then you
-- must add permissions to the CMK. For more information, see
-- <https://docs.aws.amazon.com/devops-guru/latest/userguide/sns-kms-permissions.html Permissions for Amazon Web Services KMS–encrypted Amazon SNS topics>.
module Amazonka.DevOpsGuru.AddNotificationChannel
  ( -- * Creating a Request
    AddNotificationChannel (..),
    newAddNotificationChannel,

    -- * Request Lenses
    addNotificationChannel_config,

    -- * Destructuring the Response
    AddNotificationChannelResponse (..),
    newAddNotificationChannelResponse,

    -- * Response Lenses
    addNotificationChannelResponse_httpStatus,
    addNotificationChannelResponse_id,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAddNotificationChannel' smart constructor.
data AddNotificationChannel = AddNotificationChannel'
  { -- | A @NotificationChannelConfig@ object that specifies what type of
    -- notification channel to add. The one supported notification channel is
    -- Amazon Simple Notification Service (Amazon SNS).
    config :: NotificationChannelConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddNotificationChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'config', 'addNotificationChannel_config' - A @NotificationChannelConfig@ object that specifies what type of
-- notification channel to add. The one supported notification channel is
-- Amazon Simple Notification Service (Amazon SNS).
newAddNotificationChannel ::
  -- | 'config'
  NotificationChannelConfig ->
  AddNotificationChannel
newAddNotificationChannel pConfig_ =
  AddNotificationChannel' {config = pConfig_}

-- | A @NotificationChannelConfig@ object that specifies what type of
-- notification channel to add. The one supported notification channel is
-- Amazon Simple Notification Service (Amazon SNS).
addNotificationChannel_config :: Lens.Lens' AddNotificationChannel NotificationChannelConfig
addNotificationChannel_config = Lens.lens (\AddNotificationChannel' {config} -> config) (\s@AddNotificationChannel' {} a -> s {config = a} :: AddNotificationChannel)

instance Core.AWSRequest AddNotificationChannel where
  type
    AWSResponse AddNotificationChannel =
      AddNotificationChannelResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AddNotificationChannelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Id")
      )

instance Prelude.Hashable AddNotificationChannel where
  hashWithSalt _salt AddNotificationChannel' {..} =
    _salt `Prelude.hashWithSalt` config

instance Prelude.NFData AddNotificationChannel where
  rnf AddNotificationChannel' {..} = Prelude.rnf config

instance Data.ToHeaders AddNotificationChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AddNotificationChannel where
  toJSON AddNotificationChannel' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Config" Data..= config)]
      )

instance Data.ToPath AddNotificationChannel where
  toPath = Prelude.const "/channels"

instance Data.ToQuery AddNotificationChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAddNotificationChannelResponse' smart constructor.
data AddNotificationChannelResponse = AddNotificationChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the added notification channel.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddNotificationChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'addNotificationChannelResponse_httpStatus' - The response's http status code.
--
-- 'id', 'addNotificationChannelResponse_id' - The ID of the added notification channel.
newAddNotificationChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'id'
  Prelude.Text ->
  AddNotificationChannelResponse
newAddNotificationChannelResponse pHttpStatus_ pId_ =
  AddNotificationChannelResponse'
    { httpStatus =
        pHttpStatus_,
      id = pId_
    }

-- | The response's http status code.
addNotificationChannelResponse_httpStatus :: Lens.Lens' AddNotificationChannelResponse Prelude.Int
addNotificationChannelResponse_httpStatus = Lens.lens (\AddNotificationChannelResponse' {httpStatus} -> httpStatus) (\s@AddNotificationChannelResponse' {} a -> s {httpStatus = a} :: AddNotificationChannelResponse)

-- | The ID of the added notification channel.
addNotificationChannelResponse_id :: Lens.Lens' AddNotificationChannelResponse Prelude.Text
addNotificationChannelResponse_id = Lens.lens (\AddNotificationChannelResponse' {id} -> id) (\s@AddNotificationChannelResponse' {} a -> s {id = a} :: AddNotificationChannelResponse)

instance
  Prelude.NFData
    AddNotificationChannelResponse
  where
  rnf AddNotificationChannelResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq` Prelude.rnf id
