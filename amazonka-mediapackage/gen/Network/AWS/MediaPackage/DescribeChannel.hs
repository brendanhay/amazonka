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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeChannel' smart constructor.
data DescribeChannel = DescribeChannel'
  { -- | The ID of a Channel.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DescribeChannel
newDescribeChannel pId_ = DescribeChannel' {id = pId_}

-- | The ID of a Channel.
describeChannel_id :: Lens.Lens' DescribeChannel Prelude.Text
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
            Prelude.<$> (x Core..?> "egressAccessLogs")
            Prelude.<*> (x Core..?> "hlsIngest")
            Prelude.<*> (x Core..?> "arn")
            Prelude.<*> (x Core..?> "id")
            Prelude.<*> (x Core..?> "ingressAccessLogs")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "description")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeChannel

instance Prelude.NFData DescribeChannel

instance Core.ToHeaders DescribeChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeChannel where
  toPath DescribeChannel' {..} =
    Prelude.mconcat ["/channels/", Core.toBS id]

instance Core.ToQuery DescribeChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeChannelResponse' smart constructor.
data DescribeChannelResponse = DescribeChannelResponse'
  { egressAccessLogs :: Prelude.Maybe EgressAccessLogs,
    hlsIngest :: Prelude.Maybe HlsIngest,
    -- | The Amazon Resource Name (ARN) assigned to the Channel.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Channel.
    id :: Prelude.Maybe Prelude.Text,
    ingressAccessLogs :: Prelude.Maybe IngressAccessLogs,
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A short text description of the Channel.
    description :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeChannelResponse
newDescribeChannelResponse pHttpStatus_ =
  DescribeChannelResponse'
    { egressAccessLogs =
        Prelude.Nothing,
      hlsIngest = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      ingressAccessLogs = Prelude.Nothing,
      tags = Prelude.Nothing,
      description = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeChannelResponse_egressAccessLogs :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe EgressAccessLogs)
describeChannelResponse_egressAccessLogs = Lens.lens (\DescribeChannelResponse' {egressAccessLogs} -> egressAccessLogs) (\s@DescribeChannelResponse' {} a -> s {egressAccessLogs = a} :: DescribeChannelResponse)

-- | Undocumented member.
describeChannelResponse_hlsIngest :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe HlsIngest)
describeChannelResponse_hlsIngest = Lens.lens (\DescribeChannelResponse' {hlsIngest} -> hlsIngest) (\s@DescribeChannelResponse' {} a -> s {hlsIngest = a} :: DescribeChannelResponse)

-- | The Amazon Resource Name (ARN) assigned to the Channel.
describeChannelResponse_arn :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe Prelude.Text)
describeChannelResponse_arn = Lens.lens (\DescribeChannelResponse' {arn} -> arn) (\s@DescribeChannelResponse' {} a -> s {arn = a} :: DescribeChannelResponse)

-- | The ID of the Channel.
describeChannelResponse_id :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe Prelude.Text)
describeChannelResponse_id = Lens.lens (\DescribeChannelResponse' {id} -> id) (\s@DescribeChannelResponse' {} a -> s {id = a} :: DescribeChannelResponse)

-- | Undocumented member.
describeChannelResponse_ingressAccessLogs :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe IngressAccessLogs)
describeChannelResponse_ingressAccessLogs = Lens.lens (\DescribeChannelResponse' {ingressAccessLogs} -> ingressAccessLogs) (\s@DescribeChannelResponse' {} a -> s {ingressAccessLogs = a} :: DescribeChannelResponse)

-- | Undocumented member.
describeChannelResponse_tags :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeChannelResponse_tags = Lens.lens (\DescribeChannelResponse' {tags} -> tags) (\s@DescribeChannelResponse' {} a -> s {tags = a} :: DescribeChannelResponse) Prelude.. Lens.mapping Lens._Coerce

-- | A short text description of the Channel.
describeChannelResponse_description :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe Prelude.Text)
describeChannelResponse_description = Lens.lens (\DescribeChannelResponse' {description} -> description) (\s@DescribeChannelResponse' {} a -> s {description = a} :: DescribeChannelResponse)

-- | The response's http status code.
describeChannelResponse_httpStatus :: Lens.Lens' DescribeChannelResponse Prelude.Int
describeChannelResponse_httpStatus = Lens.lens (\DescribeChannelResponse' {httpStatus} -> httpStatus) (\s@DescribeChannelResponse' {} a -> s {httpStatus = a} :: DescribeChannelResponse)

instance Prelude.NFData DescribeChannelResponse
