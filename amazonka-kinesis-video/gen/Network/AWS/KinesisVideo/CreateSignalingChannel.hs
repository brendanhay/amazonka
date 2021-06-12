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
-- Module      : Network.AWS.KinesisVideo.CreateSignalingChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a signaling channel.
--
-- @CreateSignalingChannel@ is an asynchronous operation.
module Network.AWS.KinesisVideo.CreateSignalingChannel
  ( -- * Creating a Request
    CreateSignalingChannel (..),
    newCreateSignalingChannel,

    -- * Request Lenses
    createSignalingChannel_singleMasterConfiguration,
    createSignalingChannel_channelType,
    createSignalingChannel_tags,
    createSignalingChannel_channelName,

    -- * Destructuring the Response
    CreateSignalingChannelResponse (..),
    newCreateSignalingChannelResponse,

    -- * Response Lenses
    createSignalingChannelResponse_channelARN,
    createSignalingChannelResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.KinesisVideo.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateSignalingChannel' smart constructor.
data CreateSignalingChannel = CreateSignalingChannel'
  { -- | A structure containing the configuration for the @SINGLE_MASTER@ channel
    -- type.
    singleMasterConfiguration :: Core.Maybe SingleMasterConfiguration,
    -- | A type of the signaling channel that you are creating. Currently,
    -- @SINGLE_MASTER@ is the only supported channel type.
    channelType :: Core.Maybe ChannelType,
    -- | A set of tags (key-value pairs) that you want to associate with this
    -- channel.
    tags :: Core.Maybe [Tag],
    -- | A name for the signaling channel that you are creating. It must be
    -- unique for each AWS account and AWS Region.
    channelName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateSignalingChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'singleMasterConfiguration', 'createSignalingChannel_singleMasterConfiguration' - A structure containing the configuration for the @SINGLE_MASTER@ channel
-- type.
--
-- 'channelType', 'createSignalingChannel_channelType' - A type of the signaling channel that you are creating. Currently,
-- @SINGLE_MASTER@ is the only supported channel type.
--
-- 'tags', 'createSignalingChannel_tags' - A set of tags (key-value pairs) that you want to associate with this
-- channel.
--
-- 'channelName', 'createSignalingChannel_channelName' - A name for the signaling channel that you are creating. It must be
-- unique for each AWS account and AWS Region.
newCreateSignalingChannel ::
  -- | 'channelName'
  Core.Text ->
  CreateSignalingChannel
newCreateSignalingChannel pChannelName_ =
  CreateSignalingChannel'
    { singleMasterConfiguration =
        Core.Nothing,
      channelType = Core.Nothing,
      tags = Core.Nothing,
      channelName = pChannelName_
    }

-- | A structure containing the configuration for the @SINGLE_MASTER@ channel
-- type.
createSignalingChannel_singleMasterConfiguration :: Lens.Lens' CreateSignalingChannel (Core.Maybe SingleMasterConfiguration)
createSignalingChannel_singleMasterConfiguration = Lens.lens (\CreateSignalingChannel' {singleMasterConfiguration} -> singleMasterConfiguration) (\s@CreateSignalingChannel' {} a -> s {singleMasterConfiguration = a} :: CreateSignalingChannel)

-- | A type of the signaling channel that you are creating. Currently,
-- @SINGLE_MASTER@ is the only supported channel type.
createSignalingChannel_channelType :: Lens.Lens' CreateSignalingChannel (Core.Maybe ChannelType)
createSignalingChannel_channelType = Lens.lens (\CreateSignalingChannel' {channelType} -> channelType) (\s@CreateSignalingChannel' {} a -> s {channelType = a} :: CreateSignalingChannel)

-- | A set of tags (key-value pairs) that you want to associate with this
-- channel.
createSignalingChannel_tags :: Lens.Lens' CreateSignalingChannel (Core.Maybe [Tag])
createSignalingChannel_tags = Lens.lens (\CreateSignalingChannel' {tags} -> tags) (\s@CreateSignalingChannel' {} a -> s {tags = a} :: CreateSignalingChannel) Core.. Lens.mapping Lens._Coerce

-- | A name for the signaling channel that you are creating. It must be
-- unique for each AWS account and AWS Region.
createSignalingChannel_channelName :: Lens.Lens' CreateSignalingChannel Core.Text
createSignalingChannel_channelName = Lens.lens (\CreateSignalingChannel' {channelName} -> channelName) (\s@CreateSignalingChannel' {} a -> s {channelName = a} :: CreateSignalingChannel)

instance Core.AWSRequest CreateSignalingChannel where
  type
    AWSResponse CreateSignalingChannel =
      CreateSignalingChannelResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSignalingChannelResponse'
            Core.<$> (x Core..?> "ChannelARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateSignalingChannel

instance Core.NFData CreateSignalingChannel

instance Core.ToHeaders CreateSignalingChannel where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON CreateSignalingChannel where
  toJSON CreateSignalingChannel' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SingleMasterConfiguration" Core..=)
              Core.<$> singleMasterConfiguration,
            ("ChannelType" Core..=) Core.<$> channelType,
            ("Tags" Core..=) Core.<$> tags,
            Core.Just ("ChannelName" Core..= channelName)
          ]
      )

instance Core.ToPath CreateSignalingChannel where
  toPath = Core.const "/createSignalingChannel"

instance Core.ToQuery CreateSignalingChannel where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateSignalingChannelResponse' smart constructor.
data CreateSignalingChannelResponse = CreateSignalingChannelResponse'
  { -- | The Amazon Resource Name (ARN) of the created channel.
    channelARN :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateSignalingChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelARN', 'createSignalingChannelResponse_channelARN' - The Amazon Resource Name (ARN) of the created channel.
--
-- 'httpStatus', 'createSignalingChannelResponse_httpStatus' - The response's http status code.
newCreateSignalingChannelResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateSignalingChannelResponse
newCreateSignalingChannelResponse pHttpStatus_ =
  CreateSignalingChannelResponse'
    { channelARN =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the created channel.
createSignalingChannelResponse_channelARN :: Lens.Lens' CreateSignalingChannelResponse (Core.Maybe Core.Text)
createSignalingChannelResponse_channelARN = Lens.lens (\CreateSignalingChannelResponse' {channelARN} -> channelARN) (\s@CreateSignalingChannelResponse' {} a -> s {channelARN = a} :: CreateSignalingChannelResponse)

-- | The response's http status code.
createSignalingChannelResponse_httpStatus :: Lens.Lens' CreateSignalingChannelResponse Core.Int
createSignalingChannelResponse_httpStatus = Lens.lens (\CreateSignalingChannelResponse' {httpStatus} -> httpStatus) (\s@CreateSignalingChannelResponse' {} a -> s {httpStatus = a} :: CreateSignalingChannelResponse)

instance Core.NFData CreateSignalingChannelResponse
