{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.KinesisVideo.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateSignalingChannel' smart constructor.
data CreateSignalingChannel = CreateSignalingChannel'
  { -- | A structure containing the configuration for the @SINGLE_MASTER@ channel
    -- type.
    singleMasterConfiguration :: Prelude.Maybe SingleMasterConfiguration,
    -- | A type of the signaling channel that you are creating. Currently,
    -- @SINGLE_MASTER@ is the only supported channel type.
    channelType :: Prelude.Maybe ChannelType,
    -- | A set of tags (key-value pairs) that you want to associate with this
    -- channel.
    tags :: Prelude.Maybe [Tag],
    -- | A name for the signaling channel that you are creating. It must be
    -- unique for each AWS account and AWS Region.
    channelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  CreateSignalingChannel
newCreateSignalingChannel pChannelName_ =
  CreateSignalingChannel'
    { singleMasterConfiguration =
        Prelude.Nothing,
      channelType = Prelude.Nothing,
      tags = Prelude.Nothing,
      channelName = pChannelName_
    }

-- | A structure containing the configuration for the @SINGLE_MASTER@ channel
-- type.
createSignalingChannel_singleMasterConfiguration :: Lens.Lens' CreateSignalingChannel (Prelude.Maybe SingleMasterConfiguration)
createSignalingChannel_singleMasterConfiguration = Lens.lens (\CreateSignalingChannel' {singleMasterConfiguration} -> singleMasterConfiguration) (\s@CreateSignalingChannel' {} a -> s {singleMasterConfiguration = a} :: CreateSignalingChannel)

-- | A type of the signaling channel that you are creating. Currently,
-- @SINGLE_MASTER@ is the only supported channel type.
createSignalingChannel_channelType :: Lens.Lens' CreateSignalingChannel (Prelude.Maybe ChannelType)
createSignalingChannel_channelType = Lens.lens (\CreateSignalingChannel' {channelType} -> channelType) (\s@CreateSignalingChannel' {} a -> s {channelType = a} :: CreateSignalingChannel)

-- | A set of tags (key-value pairs) that you want to associate with this
-- channel.
createSignalingChannel_tags :: Lens.Lens' CreateSignalingChannel (Prelude.Maybe [Tag])
createSignalingChannel_tags = Lens.lens (\CreateSignalingChannel' {tags} -> tags) (\s@CreateSignalingChannel' {} a -> s {tags = a} :: CreateSignalingChannel) Prelude.. Lens.mapping Prelude._Coerce

-- | A name for the signaling channel that you are creating. It must be
-- unique for each AWS account and AWS Region.
createSignalingChannel_channelName :: Lens.Lens' CreateSignalingChannel Prelude.Text
createSignalingChannel_channelName = Lens.lens (\CreateSignalingChannel' {channelName} -> channelName) (\s@CreateSignalingChannel' {} a -> s {channelName = a} :: CreateSignalingChannel)

instance Prelude.AWSRequest CreateSignalingChannel where
  type
    Rs CreateSignalingChannel =
      CreateSignalingChannelResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSignalingChannelResponse'
            Prelude.<$> (x Prelude..?> "ChannelARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSignalingChannel

instance Prelude.NFData CreateSignalingChannel

instance Prelude.ToHeaders CreateSignalingChannel where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON CreateSignalingChannel where
  toJSON CreateSignalingChannel' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SingleMasterConfiguration" Prelude..=)
              Prelude.<$> singleMasterConfiguration,
            ("ChannelType" Prelude..=) Prelude.<$> channelType,
            ("Tags" Prelude..=) Prelude.<$> tags,
            Prelude.Just ("ChannelName" Prelude..= channelName)
          ]
      )

instance Prelude.ToPath CreateSignalingChannel where
  toPath = Prelude.const "/createSignalingChannel"

instance Prelude.ToQuery CreateSignalingChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSignalingChannelResponse' smart constructor.
data CreateSignalingChannelResponse = CreateSignalingChannelResponse'
  { -- | The Amazon Resource Name (ARN) of the created channel.
    channelARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  CreateSignalingChannelResponse
newCreateSignalingChannelResponse pHttpStatus_ =
  CreateSignalingChannelResponse'
    { channelARN =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the created channel.
createSignalingChannelResponse_channelARN :: Lens.Lens' CreateSignalingChannelResponse (Prelude.Maybe Prelude.Text)
createSignalingChannelResponse_channelARN = Lens.lens (\CreateSignalingChannelResponse' {channelARN} -> channelARN) (\s@CreateSignalingChannelResponse' {} a -> s {channelARN = a} :: CreateSignalingChannelResponse)

-- | The response's http status code.
createSignalingChannelResponse_httpStatus :: Lens.Lens' CreateSignalingChannelResponse Prelude.Int
createSignalingChannelResponse_httpStatus = Lens.lens (\CreateSignalingChannelResponse' {httpStatus} -> httpStatus) (\s@CreateSignalingChannelResponse' {} a -> s {httpStatus = a} :: CreateSignalingChannelResponse)

instance
  Prelude.NFData
    CreateSignalingChannelResponse
