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
-- Module      : Network.AWS.MediaPackage.UpdateChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing Channel.
module Network.AWS.MediaPackage.UpdateChannel
  ( -- * Creating a Request
    UpdateChannel (..),
    newUpdateChannel,

    -- * Request Lenses
    updateChannel_description,
    updateChannel_id,

    -- * Destructuring the Response
    UpdateChannelResponse (..),
    newUpdateChannelResponse,

    -- * Response Lenses
    updateChannelResponse_egressAccessLogs,
    updateChannelResponse_hlsIngest,
    updateChannelResponse_arn,
    updateChannelResponse_id,
    updateChannelResponse_ingressAccessLogs,
    updateChannelResponse_tags,
    updateChannelResponse_description,
    updateChannelResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Configuration parameters used to update the Channel.
--
-- /See:/ 'newUpdateChannel' smart constructor.
data UpdateChannel = UpdateChannel'
  { -- | A short text description of the Channel.
    description :: Core.Maybe Core.Text,
    -- | The ID of the Channel to update.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateChannel_description' - A short text description of the Channel.
--
-- 'id', 'updateChannel_id' - The ID of the Channel to update.
newUpdateChannel ::
  -- | 'id'
  Core.Text ->
  UpdateChannel
newUpdateChannel pId_ =
  UpdateChannel'
    { description = Core.Nothing,
      id = pId_
    }

-- | A short text description of the Channel.
updateChannel_description :: Lens.Lens' UpdateChannel (Core.Maybe Core.Text)
updateChannel_description = Lens.lens (\UpdateChannel' {description} -> description) (\s@UpdateChannel' {} a -> s {description = a} :: UpdateChannel)

-- | The ID of the Channel to update.
updateChannel_id :: Lens.Lens' UpdateChannel Core.Text
updateChannel_id = Lens.lens (\UpdateChannel' {id} -> id) (\s@UpdateChannel' {} a -> s {id = a} :: UpdateChannel)

instance Core.AWSRequest UpdateChannel where
  type
    AWSResponse UpdateChannel =
      UpdateChannelResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateChannelResponse'
            Core.<$> (x Core..?> "egressAccessLogs")
            Core.<*> (x Core..?> "hlsIngest")
            Core.<*> (x Core..?> "arn")
            Core.<*> (x Core..?> "id")
            Core.<*> (x Core..?> "ingressAccessLogs")
            Core.<*> (x Core..?> "tags" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "description")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateChannel

instance Core.NFData UpdateChannel

instance Core.ToHeaders UpdateChannel where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateChannel where
  toJSON UpdateChannel' {..} =
    Core.object
      ( Core.catMaybes
          [("description" Core..=) Core.<$> description]
      )

instance Core.ToPath UpdateChannel where
  toPath UpdateChannel' {..} =
    Core.mconcat ["/channels/", Core.toBS id]

instance Core.ToQuery UpdateChannel where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateChannelResponse' smart constructor.
data UpdateChannelResponse = UpdateChannelResponse'
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
-- Create a value of 'UpdateChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'egressAccessLogs', 'updateChannelResponse_egressAccessLogs' - Undocumented member.
--
-- 'hlsIngest', 'updateChannelResponse_hlsIngest' - Undocumented member.
--
-- 'arn', 'updateChannelResponse_arn' - The Amazon Resource Name (ARN) assigned to the Channel.
--
-- 'id', 'updateChannelResponse_id' - The ID of the Channel.
--
-- 'ingressAccessLogs', 'updateChannelResponse_ingressAccessLogs' - Undocumented member.
--
-- 'tags', 'updateChannelResponse_tags' - Undocumented member.
--
-- 'description', 'updateChannelResponse_description' - A short text description of the Channel.
--
-- 'httpStatus', 'updateChannelResponse_httpStatus' - The response's http status code.
newUpdateChannelResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateChannelResponse
newUpdateChannelResponse pHttpStatus_ =
  UpdateChannelResponse'
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
updateChannelResponse_egressAccessLogs :: Lens.Lens' UpdateChannelResponse (Core.Maybe EgressAccessLogs)
updateChannelResponse_egressAccessLogs = Lens.lens (\UpdateChannelResponse' {egressAccessLogs} -> egressAccessLogs) (\s@UpdateChannelResponse' {} a -> s {egressAccessLogs = a} :: UpdateChannelResponse)

-- | Undocumented member.
updateChannelResponse_hlsIngest :: Lens.Lens' UpdateChannelResponse (Core.Maybe HlsIngest)
updateChannelResponse_hlsIngest = Lens.lens (\UpdateChannelResponse' {hlsIngest} -> hlsIngest) (\s@UpdateChannelResponse' {} a -> s {hlsIngest = a} :: UpdateChannelResponse)

-- | The Amazon Resource Name (ARN) assigned to the Channel.
updateChannelResponse_arn :: Lens.Lens' UpdateChannelResponse (Core.Maybe Core.Text)
updateChannelResponse_arn = Lens.lens (\UpdateChannelResponse' {arn} -> arn) (\s@UpdateChannelResponse' {} a -> s {arn = a} :: UpdateChannelResponse)

-- | The ID of the Channel.
updateChannelResponse_id :: Lens.Lens' UpdateChannelResponse (Core.Maybe Core.Text)
updateChannelResponse_id = Lens.lens (\UpdateChannelResponse' {id} -> id) (\s@UpdateChannelResponse' {} a -> s {id = a} :: UpdateChannelResponse)

-- | Undocumented member.
updateChannelResponse_ingressAccessLogs :: Lens.Lens' UpdateChannelResponse (Core.Maybe IngressAccessLogs)
updateChannelResponse_ingressAccessLogs = Lens.lens (\UpdateChannelResponse' {ingressAccessLogs} -> ingressAccessLogs) (\s@UpdateChannelResponse' {} a -> s {ingressAccessLogs = a} :: UpdateChannelResponse)

-- | Undocumented member.
updateChannelResponse_tags :: Lens.Lens' UpdateChannelResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
updateChannelResponse_tags = Lens.lens (\UpdateChannelResponse' {tags} -> tags) (\s@UpdateChannelResponse' {} a -> s {tags = a} :: UpdateChannelResponse) Core.. Lens.mapping Lens._Coerce

-- | A short text description of the Channel.
updateChannelResponse_description :: Lens.Lens' UpdateChannelResponse (Core.Maybe Core.Text)
updateChannelResponse_description = Lens.lens (\UpdateChannelResponse' {description} -> description) (\s@UpdateChannelResponse' {} a -> s {description = a} :: UpdateChannelResponse)

-- | The response's http status code.
updateChannelResponse_httpStatus :: Lens.Lens' UpdateChannelResponse Core.Int
updateChannelResponse_httpStatus = Lens.lens (\UpdateChannelResponse' {httpStatus} -> httpStatus) (\s@UpdateChannelResponse' {} a -> s {httpStatus = a} :: UpdateChannelResponse)

instance Core.NFData UpdateChannelResponse
