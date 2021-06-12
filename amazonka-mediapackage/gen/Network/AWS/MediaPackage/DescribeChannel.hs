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
-- Module      : Network.AWS.MediaPackage.DescribeChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about a Channel.
module Network.AWS.MediaPackage.DescribeChannel
  ( -- * Creating a Request
    DescribeChannel (..),
    newDescribeChannel,

    -- * Request Lenses
    describeChannel_id,

    -- * Destructuring the Response
    DescribeChannelResponse (..),
    newDescribeChannelResponse,

    -- * Response Lenses
    describeChannelResponse_egressAccessLogs,
    describeChannelResponse_hlsIngest,
    describeChannelResponse_arn,
    describeChannelResponse_id,
    describeChannelResponse_ingressAccessLogs,
    describeChannelResponse_tags,
    describeChannelResponse_description,
    describeChannelResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeChannel' smart constructor.
data DescribeChannel = DescribeChannel'
  { -- | The ID of a Channel.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'describeChannel_id' - The ID of a Channel.
newDescribeChannel ::
  -- | 'id'
  Core.Text ->
  DescribeChannel
newDescribeChannel pId_ = DescribeChannel' {id = pId_}

-- | The ID of a Channel.
describeChannel_id :: Lens.Lens' DescribeChannel Core.Text
describeChannel_id = Lens.lens (\DescribeChannel' {id} -> id) (\s@DescribeChannel' {} a -> s {id = a} :: DescribeChannel)

instance Core.AWSRequest DescribeChannel where
  type
    AWSResponse DescribeChannel =
      DescribeChannelResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeChannelResponse'
            Core.<$> (x Core..?> "egressAccessLogs")
            Core.<*> (x Core..?> "hlsIngest")
            Core.<*> (x Core..?> "arn")
            Core.<*> (x Core..?> "id")
            Core.<*> (x Core..?> "ingressAccessLogs")
            Core.<*> (x Core..?> "tags" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "description")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeChannel

instance Core.NFData DescribeChannel

instance Core.ToHeaders DescribeChannel where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DescribeChannel where
  toPath DescribeChannel' {..} =
    Core.mconcat ["/channels/", Core.toBS id]

instance Core.ToQuery DescribeChannel where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeChannelResponse' smart constructor.
data DescribeChannelResponse = DescribeChannelResponse'
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
-- Create a value of 'DescribeChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'egressAccessLogs', 'describeChannelResponse_egressAccessLogs' - Undocumented member.
--
-- 'hlsIngest', 'describeChannelResponse_hlsIngest' - Undocumented member.
--
-- 'arn', 'describeChannelResponse_arn' - The Amazon Resource Name (ARN) assigned to the Channel.
--
-- 'id', 'describeChannelResponse_id' - The ID of the Channel.
--
-- 'ingressAccessLogs', 'describeChannelResponse_ingressAccessLogs' - Undocumented member.
--
-- 'tags', 'describeChannelResponse_tags' - Undocumented member.
--
-- 'description', 'describeChannelResponse_description' - A short text description of the Channel.
--
-- 'httpStatus', 'describeChannelResponse_httpStatus' - The response's http status code.
newDescribeChannelResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeChannelResponse
newDescribeChannelResponse pHttpStatus_ =
  DescribeChannelResponse'
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
describeChannelResponse_egressAccessLogs :: Lens.Lens' DescribeChannelResponse (Core.Maybe EgressAccessLogs)
describeChannelResponse_egressAccessLogs = Lens.lens (\DescribeChannelResponse' {egressAccessLogs} -> egressAccessLogs) (\s@DescribeChannelResponse' {} a -> s {egressAccessLogs = a} :: DescribeChannelResponse)

-- | Undocumented member.
describeChannelResponse_hlsIngest :: Lens.Lens' DescribeChannelResponse (Core.Maybe HlsIngest)
describeChannelResponse_hlsIngest = Lens.lens (\DescribeChannelResponse' {hlsIngest} -> hlsIngest) (\s@DescribeChannelResponse' {} a -> s {hlsIngest = a} :: DescribeChannelResponse)

-- | The Amazon Resource Name (ARN) assigned to the Channel.
describeChannelResponse_arn :: Lens.Lens' DescribeChannelResponse (Core.Maybe Core.Text)
describeChannelResponse_arn = Lens.lens (\DescribeChannelResponse' {arn} -> arn) (\s@DescribeChannelResponse' {} a -> s {arn = a} :: DescribeChannelResponse)

-- | The ID of the Channel.
describeChannelResponse_id :: Lens.Lens' DescribeChannelResponse (Core.Maybe Core.Text)
describeChannelResponse_id = Lens.lens (\DescribeChannelResponse' {id} -> id) (\s@DescribeChannelResponse' {} a -> s {id = a} :: DescribeChannelResponse)

-- | Undocumented member.
describeChannelResponse_ingressAccessLogs :: Lens.Lens' DescribeChannelResponse (Core.Maybe IngressAccessLogs)
describeChannelResponse_ingressAccessLogs = Lens.lens (\DescribeChannelResponse' {ingressAccessLogs} -> ingressAccessLogs) (\s@DescribeChannelResponse' {} a -> s {ingressAccessLogs = a} :: DescribeChannelResponse)

-- | Undocumented member.
describeChannelResponse_tags :: Lens.Lens' DescribeChannelResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
describeChannelResponse_tags = Lens.lens (\DescribeChannelResponse' {tags} -> tags) (\s@DescribeChannelResponse' {} a -> s {tags = a} :: DescribeChannelResponse) Core.. Lens.mapping Lens._Coerce

-- | A short text description of the Channel.
describeChannelResponse_description :: Lens.Lens' DescribeChannelResponse (Core.Maybe Core.Text)
describeChannelResponse_description = Lens.lens (\DescribeChannelResponse' {description} -> description) (\s@DescribeChannelResponse' {} a -> s {description = a} :: DescribeChannelResponse)

-- | The response's http status code.
describeChannelResponse_httpStatus :: Lens.Lens' DescribeChannelResponse Core.Int
describeChannelResponse_httpStatus = Lens.lens (\DescribeChannelResponse' {httpStatus} -> httpStatus) (\s@DescribeChannelResponse' {} a -> s {httpStatus = a} :: DescribeChannelResponse)

instance Core.NFData DescribeChannelResponse
