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
-- Module      : Amazonka.MediaPackage.DescribeChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about a Channel.
module Amazonka.MediaPackage.DescribeChannel
  ( -- * Creating a Request
    DescribeChannel (..),
    newDescribeChannel,

    -- * Request Lenses
    describeChannel_id,

    -- * Destructuring the Response
    DescribeChannelResponse (..),
    newDescribeChannelResponse,

    -- * Response Lenses
    describeChannelResponse_arn,
    describeChannelResponse_createdAt,
    describeChannelResponse_description,
    describeChannelResponse_egressAccessLogs,
    describeChannelResponse_hlsIngest,
    describeChannelResponse_id,
    describeChannelResponse_ingressAccessLogs,
    describeChannelResponse_tags,
    describeChannelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackage.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeChannelResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "createdAt")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "egressAccessLogs")
            Prelude.<*> (x Data..?> "hlsIngest")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "ingressAccessLogs")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeChannel where
  hashWithSalt _salt DescribeChannel' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData DescribeChannel where
  rnf DescribeChannel' {..} = Prelude.rnf id

instance Data.ToHeaders DescribeChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeChannel where
  toPath DescribeChannel' {..} =
    Prelude.mconcat ["/channels/", Data.toBS id]

instance Data.ToQuery DescribeChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeChannelResponse' smart constructor.
data DescribeChannelResponse = DescribeChannelResponse'
  { -- | The Amazon Resource Name (ARN) assigned to the Channel.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time the Channel was created.
    createdAt :: Prelude.Maybe Prelude.Text,
    -- | A short text description of the Channel.
    description :: Prelude.Maybe Prelude.Text,
    egressAccessLogs :: Prelude.Maybe EgressAccessLogs,
    hlsIngest :: Prelude.Maybe HlsIngest,
    -- | The ID of the Channel.
    id :: Prelude.Maybe Prelude.Text,
    ingressAccessLogs :: Prelude.Maybe IngressAccessLogs,
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
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
-- 'arn', 'describeChannelResponse_arn' - The Amazon Resource Name (ARN) assigned to the Channel.
--
-- 'createdAt', 'describeChannelResponse_createdAt' - The date and time the Channel was created.
--
-- 'description', 'describeChannelResponse_description' - A short text description of the Channel.
--
-- 'egressAccessLogs', 'describeChannelResponse_egressAccessLogs' - Undocumented member.
--
-- 'hlsIngest', 'describeChannelResponse_hlsIngest' - Undocumented member.
--
-- 'id', 'describeChannelResponse_id' - The ID of the Channel.
--
-- 'ingressAccessLogs', 'describeChannelResponse_ingressAccessLogs' - Undocumented member.
--
-- 'tags', 'describeChannelResponse_tags' - Undocumented member.
--
-- 'httpStatus', 'describeChannelResponse_httpStatus' - The response's http status code.
newDescribeChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeChannelResponse
newDescribeChannelResponse pHttpStatus_ =
  DescribeChannelResponse'
    { arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      description = Prelude.Nothing,
      egressAccessLogs = Prelude.Nothing,
      hlsIngest = Prelude.Nothing,
      id = Prelude.Nothing,
      ingressAccessLogs = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) assigned to the Channel.
describeChannelResponse_arn :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe Prelude.Text)
describeChannelResponse_arn = Lens.lens (\DescribeChannelResponse' {arn} -> arn) (\s@DescribeChannelResponse' {} a -> s {arn = a} :: DescribeChannelResponse)

-- | The date and time the Channel was created.
describeChannelResponse_createdAt :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe Prelude.Text)
describeChannelResponse_createdAt = Lens.lens (\DescribeChannelResponse' {createdAt} -> createdAt) (\s@DescribeChannelResponse' {} a -> s {createdAt = a} :: DescribeChannelResponse)

-- | A short text description of the Channel.
describeChannelResponse_description :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe Prelude.Text)
describeChannelResponse_description = Lens.lens (\DescribeChannelResponse' {description} -> description) (\s@DescribeChannelResponse' {} a -> s {description = a} :: DescribeChannelResponse)

-- | Undocumented member.
describeChannelResponse_egressAccessLogs :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe EgressAccessLogs)
describeChannelResponse_egressAccessLogs = Lens.lens (\DescribeChannelResponse' {egressAccessLogs} -> egressAccessLogs) (\s@DescribeChannelResponse' {} a -> s {egressAccessLogs = a} :: DescribeChannelResponse)

-- | Undocumented member.
describeChannelResponse_hlsIngest :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe HlsIngest)
describeChannelResponse_hlsIngest = Lens.lens (\DescribeChannelResponse' {hlsIngest} -> hlsIngest) (\s@DescribeChannelResponse' {} a -> s {hlsIngest = a} :: DescribeChannelResponse)

-- | The ID of the Channel.
describeChannelResponse_id :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe Prelude.Text)
describeChannelResponse_id = Lens.lens (\DescribeChannelResponse' {id} -> id) (\s@DescribeChannelResponse' {} a -> s {id = a} :: DescribeChannelResponse)

-- | Undocumented member.
describeChannelResponse_ingressAccessLogs :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe IngressAccessLogs)
describeChannelResponse_ingressAccessLogs = Lens.lens (\DescribeChannelResponse' {ingressAccessLogs} -> ingressAccessLogs) (\s@DescribeChannelResponse' {} a -> s {ingressAccessLogs = a} :: DescribeChannelResponse)

-- | Undocumented member.
describeChannelResponse_tags :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeChannelResponse_tags = Lens.lens (\DescribeChannelResponse' {tags} -> tags) (\s@DescribeChannelResponse' {} a -> s {tags = a} :: DescribeChannelResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeChannelResponse_httpStatus :: Lens.Lens' DescribeChannelResponse Prelude.Int
describeChannelResponse_httpStatus = Lens.lens (\DescribeChannelResponse' {httpStatus} -> httpStatus) (\s@DescribeChannelResponse' {} a -> s {httpStatus = a} :: DescribeChannelResponse)

instance Prelude.NFData DescribeChannelResponse where
  rnf DescribeChannelResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf egressAccessLogs
      `Prelude.seq` Prelude.rnf hlsIngest
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf ingressAccessLogs
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
