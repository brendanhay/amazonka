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
-- Module      : Network.AWS.MediaPackage.CreateChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Channel.
module Network.AWS.MediaPackage.CreateChannel
  ( -- * Creating a Request
    CreateChannel (..),
    newCreateChannel,

    -- * Request Lenses
    createChannel_tags,
    createChannel_description,
    createChannel_id,

    -- * Destructuring the Response
    CreateChannelResponse (..),
    newCreateChannelResponse,

    -- * Response Lenses
    createChannelResponse_egressAccessLogs,
    createChannelResponse_hlsIngest,
    createChannelResponse_arn,
    createChannelResponse_id,
    createChannelResponse_ingressAccessLogs,
    createChannelResponse_tags,
    createChannelResponse_description,
    createChannelResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A new Channel configuration.
--
-- /See:/ 'newCreateChannel' smart constructor.
data CreateChannel = CreateChannel'
  { tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | A short text description of the Channel.
    description :: Core.Maybe Core.Text,
    -- | The ID of the Channel. The ID must be unique within the region and it
    -- cannot be changed after a Channel is created.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createChannel_tags' - Undocumented member.
--
-- 'description', 'createChannel_description' - A short text description of the Channel.
--
-- 'id', 'createChannel_id' - The ID of the Channel. The ID must be unique within the region and it
-- cannot be changed after a Channel is created.
newCreateChannel ::
  -- | 'id'
  Core.Text ->
  CreateChannel
newCreateChannel pId_ =
  CreateChannel'
    { tags = Core.Nothing,
      description = Core.Nothing,
      id = pId_
    }

-- | Undocumented member.
createChannel_tags :: Lens.Lens' CreateChannel (Core.Maybe (Core.HashMap Core.Text Core.Text))
createChannel_tags = Lens.lens (\CreateChannel' {tags} -> tags) (\s@CreateChannel' {} a -> s {tags = a} :: CreateChannel) Core.. Lens.mapping Lens._Coerce

-- | A short text description of the Channel.
createChannel_description :: Lens.Lens' CreateChannel (Core.Maybe Core.Text)
createChannel_description = Lens.lens (\CreateChannel' {description} -> description) (\s@CreateChannel' {} a -> s {description = a} :: CreateChannel)

-- | The ID of the Channel. The ID must be unique within the region and it
-- cannot be changed after a Channel is created.
createChannel_id :: Lens.Lens' CreateChannel Core.Text
createChannel_id = Lens.lens (\CreateChannel' {id} -> id) (\s@CreateChannel' {} a -> s {id = a} :: CreateChannel)

instance Core.AWSRequest CreateChannel where
  type
    AWSResponse CreateChannel =
      CreateChannelResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateChannelResponse'
            Core.<$> (x Core..?> "egressAccessLogs")
            Core.<*> (x Core..?> "hlsIngest")
            Core.<*> (x Core..?> "arn")
            Core.<*> (x Core..?> "id")
            Core.<*> (x Core..?> "ingressAccessLogs")
            Core.<*> (x Core..?> "tags" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "description")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateChannel

instance Core.NFData CreateChannel

instance Core.ToHeaders CreateChannel where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateChannel where
  toJSON CreateChannel' {..} =
    Core.object
      ( Core.catMaybes
          [ ("tags" Core..=) Core.<$> tags,
            ("description" Core..=) Core.<$> description,
            Core.Just ("id" Core..= id)
          ]
      )

instance Core.ToPath CreateChannel where
  toPath = Core.const "/channels"

instance Core.ToQuery CreateChannel where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateChannelResponse' smart constructor.
data CreateChannelResponse = CreateChannelResponse'
  { egressAccessLogs :: Core.Maybe EgressAccessLogs,
    hlsIngest :: Core.Maybe HlsIngest,
    -- | The Amazon Resource Name (ARN) assigned to the Channel.
    arn :: Core.Maybe Core.Text,
    -- | The ID of the Channel.
    id :: Core.Maybe Core.Text,
    ingressAccessLogs :: Core.Maybe IngressAccessLogs,
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | A short text description of the Channel.
    description :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'egressAccessLogs', 'createChannelResponse_egressAccessLogs' - Undocumented member.
--
-- 'hlsIngest', 'createChannelResponse_hlsIngest' - Undocumented member.
--
-- 'arn', 'createChannelResponse_arn' - The Amazon Resource Name (ARN) assigned to the Channel.
--
-- 'id', 'createChannelResponse_id' - The ID of the Channel.
--
-- 'ingressAccessLogs', 'createChannelResponse_ingressAccessLogs' - Undocumented member.
--
-- 'tags', 'createChannelResponse_tags' - Undocumented member.
--
-- 'description', 'createChannelResponse_description' - A short text description of the Channel.
--
-- 'httpStatus', 'createChannelResponse_httpStatus' - The response's http status code.
newCreateChannelResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateChannelResponse
newCreateChannelResponse pHttpStatus_ =
  CreateChannelResponse'
    { egressAccessLogs =
        Core.Nothing,
      hlsIngest = Core.Nothing,
      arn = Core.Nothing,
      id = Core.Nothing,
      ingressAccessLogs = Core.Nothing,
      tags = Core.Nothing,
      description = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createChannelResponse_egressAccessLogs :: Lens.Lens' CreateChannelResponse (Core.Maybe EgressAccessLogs)
createChannelResponse_egressAccessLogs = Lens.lens (\CreateChannelResponse' {egressAccessLogs} -> egressAccessLogs) (\s@CreateChannelResponse' {} a -> s {egressAccessLogs = a} :: CreateChannelResponse)

-- | Undocumented member.
createChannelResponse_hlsIngest :: Lens.Lens' CreateChannelResponse (Core.Maybe HlsIngest)
createChannelResponse_hlsIngest = Lens.lens (\CreateChannelResponse' {hlsIngest} -> hlsIngest) (\s@CreateChannelResponse' {} a -> s {hlsIngest = a} :: CreateChannelResponse)

-- | The Amazon Resource Name (ARN) assigned to the Channel.
createChannelResponse_arn :: Lens.Lens' CreateChannelResponse (Core.Maybe Core.Text)
createChannelResponse_arn = Lens.lens (\CreateChannelResponse' {arn} -> arn) (\s@CreateChannelResponse' {} a -> s {arn = a} :: CreateChannelResponse)

-- | The ID of the Channel.
createChannelResponse_id :: Lens.Lens' CreateChannelResponse (Core.Maybe Core.Text)
createChannelResponse_id = Lens.lens (\CreateChannelResponse' {id} -> id) (\s@CreateChannelResponse' {} a -> s {id = a} :: CreateChannelResponse)

-- | Undocumented member.
createChannelResponse_ingressAccessLogs :: Lens.Lens' CreateChannelResponse (Core.Maybe IngressAccessLogs)
createChannelResponse_ingressAccessLogs = Lens.lens (\CreateChannelResponse' {ingressAccessLogs} -> ingressAccessLogs) (\s@CreateChannelResponse' {} a -> s {ingressAccessLogs = a} :: CreateChannelResponse)

-- | Undocumented member.
createChannelResponse_tags :: Lens.Lens' CreateChannelResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
createChannelResponse_tags = Lens.lens (\CreateChannelResponse' {tags} -> tags) (\s@CreateChannelResponse' {} a -> s {tags = a} :: CreateChannelResponse) Core.. Lens.mapping Lens._Coerce

-- | A short text description of the Channel.
createChannelResponse_description :: Lens.Lens' CreateChannelResponse (Core.Maybe Core.Text)
createChannelResponse_description = Lens.lens (\CreateChannelResponse' {description} -> description) (\s@CreateChannelResponse' {} a -> s {description = a} :: CreateChannelResponse)

-- | The response's http status code.
createChannelResponse_httpStatus :: Lens.Lens' CreateChannelResponse Core.Int
createChannelResponse_httpStatus = Lens.lens (\CreateChannelResponse' {httpStatus} -> httpStatus) (\s@CreateChannelResponse' {} a -> s {httpStatus = a} :: CreateChannelResponse)

instance Core.NFData CreateChannelResponse
