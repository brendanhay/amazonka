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
-- Module      : Amazonka.IoTAnalytics.CreateChannel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used to create a channel. A channel collects data from an MQTT topic and
-- archives the raw, unprocessed messages before publishing the data to a
-- pipeline.
module Amazonka.IoTAnalytics.CreateChannel
  ( -- * Creating a Request
    CreateChannel (..),
    newCreateChannel,

    -- * Request Lenses
    createChannel_tags,
    createChannel_channelStorage,
    createChannel_retentionPeriod,
    createChannel_channelName,

    -- * Destructuring the Response
    CreateChannelResponse (..),
    newCreateChannelResponse,

    -- * Response Lenses
    createChannelResponse_channelName,
    createChannelResponse_channelArn,
    createChannelResponse_retentionPeriod,
    createChannelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTAnalytics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateChannel' smart constructor.
data CreateChannel = CreateChannel'
  { -- | Metadata which can be used to manage the channel.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | Where channel data is stored. You can choose one of @serviceManagedS3@
    -- or @customerManagedS3@ storage. If not specified, the default is
    -- @serviceManagedS3@. You can\'t change this storage option after the
    -- channel is created.
    channelStorage :: Prelude.Maybe ChannelStorage,
    -- | How long, in days, message data is kept for the channel. When
    -- @customerManagedS3@ storage is selected, this parameter is ignored.
    retentionPeriod :: Prelude.Maybe RetentionPeriod,
    -- | The name of the channel.
    channelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createChannel_tags' - Metadata which can be used to manage the channel.
--
-- 'channelStorage', 'createChannel_channelStorage' - Where channel data is stored. You can choose one of @serviceManagedS3@
-- or @customerManagedS3@ storage. If not specified, the default is
-- @serviceManagedS3@. You can\'t change this storage option after the
-- channel is created.
--
-- 'retentionPeriod', 'createChannel_retentionPeriod' - How long, in days, message data is kept for the channel. When
-- @customerManagedS3@ storage is selected, this parameter is ignored.
--
-- 'channelName', 'createChannel_channelName' - The name of the channel.
newCreateChannel ::
  -- | 'channelName'
  Prelude.Text ->
  CreateChannel
newCreateChannel pChannelName_ =
  CreateChannel'
    { tags = Prelude.Nothing,
      channelStorage = Prelude.Nothing,
      retentionPeriod = Prelude.Nothing,
      channelName = pChannelName_
    }

-- | Metadata which can be used to manage the channel.
createChannel_tags :: Lens.Lens' CreateChannel (Prelude.Maybe (Prelude.NonEmpty Tag))
createChannel_tags = Lens.lens (\CreateChannel' {tags} -> tags) (\s@CreateChannel' {} a -> s {tags = a} :: CreateChannel) Prelude.. Lens.mapping Lens.coerced

-- | Where channel data is stored. You can choose one of @serviceManagedS3@
-- or @customerManagedS3@ storage. If not specified, the default is
-- @serviceManagedS3@. You can\'t change this storage option after the
-- channel is created.
createChannel_channelStorage :: Lens.Lens' CreateChannel (Prelude.Maybe ChannelStorage)
createChannel_channelStorage = Lens.lens (\CreateChannel' {channelStorage} -> channelStorage) (\s@CreateChannel' {} a -> s {channelStorage = a} :: CreateChannel)

-- | How long, in days, message data is kept for the channel. When
-- @customerManagedS3@ storage is selected, this parameter is ignored.
createChannel_retentionPeriod :: Lens.Lens' CreateChannel (Prelude.Maybe RetentionPeriod)
createChannel_retentionPeriod = Lens.lens (\CreateChannel' {retentionPeriod} -> retentionPeriod) (\s@CreateChannel' {} a -> s {retentionPeriod = a} :: CreateChannel)

-- | The name of the channel.
createChannel_channelName :: Lens.Lens' CreateChannel Prelude.Text
createChannel_channelName = Lens.lens (\CreateChannel' {channelName} -> channelName) (\s@CreateChannel' {} a -> s {channelName = a} :: CreateChannel)

instance Core.AWSRequest CreateChannel where
  type
    AWSResponse CreateChannel =
      CreateChannelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateChannelResponse'
            Prelude.<$> (x Data..?> "channelName")
            Prelude.<*> (x Data..?> "channelArn")
            Prelude.<*> (x Data..?> "retentionPeriod")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateChannel where
  hashWithSalt _salt CreateChannel' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` channelStorage
      `Prelude.hashWithSalt` retentionPeriod
      `Prelude.hashWithSalt` channelName

instance Prelude.NFData CreateChannel where
  rnf CreateChannel' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf channelStorage
      `Prelude.seq` Prelude.rnf retentionPeriod
      `Prelude.seq` Prelude.rnf channelName

instance Data.ToHeaders CreateChannel where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateChannel where
  toJSON CreateChannel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            ("channelStorage" Data..=)
              Prelude.<$> channelStorage,
            ("retentionPeriod" Data..=)
              Prelude.<$> retentionPeriod,
            Prelude.Just ("channelName" Data..= channelName)
          ]
      )

instance Data.ToPath CreateChannel where
  toPath = Prelude.const "/channels"

instance Data.ToQuery CreateChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateChannelResponse' smart constructor.
data CreateChannelResponse = CreateChannelResponse'
  { -- | The name of the channel.
    channelName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the channel.
    channelArn :: Prelude.Maybe Prelude.Text,
    -- | How long, in days, message data is kept for the channel.
    retentionPeriod :: Prelude.Maybe RetentionPeriod,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelName', 'createChannelResponse_channelName' - The name of the channel.
--
-- 'channelArn', 'createChannelResponse_channelArn' - The ARN of the channel.
--
-- 'retentionPeriod', 'createChannelResponse_retentionPeriod' - How long, in days, message data is kept for the channel.
--
-- 'httpStatus', 'createChannelResponse_httpStatus' - The response's http status code.
newCreateChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateChannelResponse
newCreateChannelResponse pHttpStatus_ =
  CreateChannelResponse'
    { channelName =
        Prelude.Nothing,
      channelArn = Prelude.Nothing,
      retentionPeriod = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the channel.
createChannelResponse_channelName :: Lens.Lens' CreateChannelResponse (Prelude.Maybe Prelude.Text)
createChannelResponse_channelName = Lens.lens (\CreateChannelResponse' {channelName} -> channelName) (\s@CreateChannelResponse' {} a -> s {channelName = a} :: CreateChannelResponse)

-- | The ARN of the channel.
createChannelResponse_channelArn :: Lens.Lens' CreateChannelResponse (Prelude.Maybe Prelude.Text)
createChannelResponse_channelArn = Lens.lens (\CreateChannelResponse' {channelArn} -> channelArn) (\s@CreateChannelResponse' {} a -> s {channelArn = a} :: CreateChannelResponse)

-- | How long, in days, message data is kept for the channel.
createChannelResponse_retentionPeriod :: Lens.Lens' CreateChannelResponse (Prelude.Maybe RetentionPeriod)
createChannelResponse_retentionPeriod = Lens.lens (\CreateChannelResponse' {retentionPeriod} -> retentionPeriod) (\s@CreateChannelResponse' {} a -> s {retentionPeriod = a} :: CreateChannelResponse)

-- | The response's http status code.
createChannelResponse_httpStatus :: Lens.Lens' CreateChannelResponse Prelude.Int
createChannelResponse_httpStatus = Lens.lens (\CreateChannelResponse' {httpStatus} -> httpStatus) (\s@CreateChannelResponse' {} a -> s {httpStatus = a} :: CreateChannelResponse)

instance Prelude.NFData CreateChannelResponse where
  rnf CreateChannelResponse' {..} =
    Prelude.rnf channelName
      `Prelude.seq` Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf retentionPeriod
      `Prelude.seq` Prelude.rnf httpStatus
