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
-- Module      : Network.AWS.IoTAnalytics.CreateChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used to create a channel. A channel collects data from an MQTT topic and
-- archives the raw, unprocessed messages before publishing the data to a
-- pipeline.
module Network.AWS.IoTAnalytics.CreateChannel
  ( -- * Creating a Request
    CreateChannel (..),
    newCreateChannel,

    -- * Request Lenses
    createChannel_retentionPeriod,
    createChannel_tags,
    createChannel_channelStorage,
    createChannel_channelName,

    -- * Destructuring the Response
    CreateChannelResponse (..),
    newCreateChannelResponse,

    -- * Response Lenses
    createChannelResponse_channelName,
    createChannelResponse_retentionPeriod,
    createChannelResponse_channelArn,
    createChannelResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateChannel' smart constructor.
data CreateChannel = CreateChannel'
  { -- | How long, in days, message data is kept for the channel. When
    -- @customerManagedS3@ storage is selected, this parameter is ignored.
    retentionPeriod :: Prelude.Maybe RetentionPeriod,
    -- | Metadata which can be used to manage the channel.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | Where channel data is stored. You can choose one of @serviceManagedS3@
    -- or @customerManagedS3@ storage. If not specified, the default is
    -- @serviceManagedS3@. You can\'t change this storage option after the
    -- channel is created.
    channelStorage :: Prelude.Maybe ChannelStorage,
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
-- 'retentionPeriod', 'createChannel_retentionPeriod' - How long, in days, message data is kept for the channel. When
-- @customerManagedS3@ storage is selected, this parameter is ignored.
--
-- 'tags', 'createChannel_tags' - Metadata which can be used to manage the channel.
--
-- 'channelStorage', 'createChannel_channelStorage' - Where channel data is stored. You can choose one of @serviceManagedS3@
-- or @customerManagedS3@ storage. If not specified, the default is
-- @serviceManagedS3@. You can\'t change this storage option after the
-- channel is created.
--
-- 'channelName', 'createChannel_channelName' - The name of the channel.
newCreateChannel ::
  -- | 'channelName'
  Prelude.Text ->
  CreateChannel
newCreateChannel pChannelName_ =
  CreateChannel'
    { retentionPeriod = Prelude.Nothing,
      tags = Prelude.Nothing,
      channelStorage = Prelude.Nothing,
      channelName = pChannelName_
    }

-- | How long, in days, message data is kept for the channel. When
-- @customerManagedS3@ storage is selected, this parameter is ignored.
createChannel_retentionPeriod :: Lens.Lens' CreateChannel (Prelude.Maybe RetentionPeriod)
createChannel_retentionPeriod = Lens.lens (\CreateChannel' {retentionPeriod} -> retentionPeriod) (\s@CreateChannel' {} a -> s {retentionPeriod = a} :: CreateChannel)

-- | Metadata which can be used to manage the channel.
createChannel_tags :: Lens.Lens' CreateChannel (Prelude.Maybe (Prelude.NonEmpty Tag))
createChannel_tags = Lens.lens (\CreateChannel' {tags} -> tags) (\s@CreateChannel' {} a -> s {tags = a} :: CreateChannel) Prelude.. Lens.mapping Lens._Coerce

-- | Where channel data is stored. You can choose one of @serviceManagedS3@
-- or @customerManagedS3@ storage. If not specified, the default is
-- @serviceManagedS3@. You can\'t change this storage option after the
-- channel is created.
createChannel_channelStorage :: Lens.Lens' CreateChannel (Prelude.Maybe ChannelStorage)
createChannel_channelStorage = Lens.lens (\CreateChannel' {channelStorage} -> channelStorage) (\s@CreateChannel' {} a -> s {channelStorage = a} :: CreateChannel)

-- | The name of the channel.
createChannel_channelName :: Lens.Lens' CreateChannel Prelude.Text
createChannel_channelName = Lens.lens (\CreateChannel' {channelName} -> channelName) (\s@CreateChannel' {} a -> s {channelName = a} :: CreateChannel)

instance Core.AWSRequest CreateChannel where
  type
    AWSResponse CreateChannel =
      CreateChannelResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateChannelResponse'
            Prelude.<$> (x Core..?> "channelName")
            Prelude.<*> (x Core..?> "retentionPeriod")
            Prelude.<*> (x Core..?> "channelArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateChannel

instance Prelude.NFData CreateChannel

instance Core.ToHeaders CreateChannel where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON CreateChannel where
  toJSON CreateChannel' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("retentionPeriod" Core..=)
              Prelude.<$> retentionPeriod,
            ("tags" Core..=) Prelude.<$> tags,
            ("channelStorage" Core..=)
              Prelude.<$> channelStorage,
            Prelude.Just ("channelName" Core..= channelName)
          ]
      )

instance Core.ToPath CreateChannel where
  toPath = Prelude.const "/channels"

instance Core.ToQuery CreateChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateChannelResponse' smart constructor.
data CreateChannelResponse = CreateChannelResponse'
  { -- | The name of the channel.
    channelName :: Prelude.Maybe Prelude.Text,
    -- | How long, in days, message data is kept for the channel.
    retentionPeriod :: Prelude.Maybe RetentionPeriod,
    -- | The ARN of the channel.
    channelArn :: Prelude.Maybe Prelude.Text,
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
-- 'retentionPeriod', 'createChannelResponse_retentionPeriod' - How long, in days, message data is kept for the channel.
--
-- 'channelArn', 'createChannelResponse_channelArn' - The ARN of the channel.
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
      retentionPeriod = Prelude.Nothing,
      channelArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the channel.
createChannelResponse_channelName :: Lens.Lens' CreateChannelResponse (Prelude.Maybe Prelude.Text)
createChannelResponse_channelName = Lens.lens (\CreateChannelResponse' {channelName} -> channelName) (\s@CreateChannelResponse' {} a -> s {channelName = a} :: CreateChannelResponse)

-- | How long, in days, message data is kept for the channel.
createChannelResponse_retentionPeriod :: Lens.Lens' CreateChannelResponse (Prelude.Maybe RetentionPeriod)
createChannelResponse_retentionPeriod = Lens.lens (\CreateChannelResponse' {retentionPeriod} -> retentionPeriod) (\s@CreateChannelResponse' {} a -> s {retentionPeriod = a} :: CreateChannelResponse)

-- | The ARN of the channel.
createChannelResponse_channelArn :: Lens.Lens' CreateChannelResponse (Prelude.Maybe Prelude.Text)
createChannelResponse_channelArn = Lens.lens (\CreateChannelResponse' {channelArn} -> channelArn) (\s@CreateChannelResponse' {} a -> s {channelArn = a} :: CreateChannelResponse)

-- | The response's http status code.
createChannelResponse_httpStatus :: Lens.Lens' CreateChannelResponse Prelude.Int
createChannelResponse_httpStatus = Lens.lens (\CreateChannelResponse' {httpStatus} -> httpStatus) (\s@CreateChannelResponse' {} a -> s {httpStatus = a} :: CreateChannelResponse)

instance Prelude.NFData CreateChannelResponse
