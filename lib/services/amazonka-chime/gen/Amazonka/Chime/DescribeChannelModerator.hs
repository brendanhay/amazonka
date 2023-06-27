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
-- Module      : Amazonka.Chime.DescribeChannelModerator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the full details of a single ChannelModerator.
--
-- The @x-amz-chime-bearer@ request header is mandatory. Use the
-- @AppInstanceUserArn@ of the user that makes the API call as the value in
-- the header.
module Amazonka.Chime.DescribeChannelModerator
  ( -- * Creating a Request
    DescribeChannelModerator (..),
    newDescribeChannelModerator,

    -- * Request Lenses
    describeChannelModerator_chimeBearer,
    describeChannelModerator_channelArn,
    describeChannelModerator_channelModeratorArn,

    -- * Destructuring the Response
    DescribeChannelModeratorResponse (..),
    newDescribeChannelModeratorResponse,

    -- * Response Lenses
    describeChannelModeratorResponse_channelModerator,
    describeChannelModeratorResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeChannelModerator' smart constructor.
data DescribeChannelModerator = DescribeChannelModerator'
  { -- | The @AppInstanceUserArn@ of the user that makes the API call.
    chimeBearer :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the channel.
    channelArn :: Prelude.Text,
    -- | The ARN of the channel moderator.
    channelModeratorArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeChannelModerator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'chimeBearer', 'describeChannelModerator_chimeBearer' - The @AppInstanceUserArn@ of the user that makes the API call.
--
-- 'channelArn', 'describeChannelModerator_channelArn' - The ARN of the channel.
--
-- 'channelModeratorArn', 'describeChannelModerator_channelModeratorArn' - The ARN of the channel moderator.
newDescribeChannelModerator ::
  -- | 'channelArn'
  Prelude.Text ->
  -- | 'channelModeratorArn'
  Prelude.Text ->
  DescribeChannelModerator
newDescribeChannelModerator
  pChannelArn_
  pChannelModeratorArn_ =
    DescribeChannelModerator'
      { chimeBearer =
          Prelude.Nothing,
        channelArn = pChannelArn_,
        channelModeratorArn = pChannelModeratorArn_
      }

-- | The @AppInstanceUserArn@ of the user that makes the API call.
describeChannelModerator_chimeBearer :: Lens.Lens' DescribeChannelModerator (Prelude.Maybe Prelude.Text)
describeChannelModerator_chimeBearer = Lens.lens (\DescribeChannelModerator' {chimeBearer} -> chimeBearer) (\s@DescribeChannelModerator' {} a -> s {chimeBearer = a} :: DescribeChannelModerator)

-- | The ARN of the channel.
describeChannelModerator_channelArn :: Lens.Lens' DescribeChannelModerator Prelude.Text
describeChannelModerator_channelArn = Lens.lens (\DescribeChannelModerator' {channelArn} -> channelArn) (\s@DescribeChannelModerator' {} a -> s {channelArn = a} :: DescribeChannelModerator)

-- | The ARN of the channel moderator.
describeChannelModerator_channelModeratorArn :: Lens.Lens' DescribeChannelModerator Prelude.Text
describeChannelModerator_channelModeratorArn = Lens.lens (\DescribeChannelModerator' {channelModeratorArn} -> channelModeratorArn) (\s@DescribeChannelModerator' {} a -> s {channelModeratorArn = a} :: DescribeChannelModerator)

instance Core.AWSRequest DescribeChannelModerator where
  type
    AWSResponse DescribeChannelModerator =
      DescribeChannelModeratorResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeChannelModeratorResponse'
            Prelude.<$> (x Data..?> "ChannelModerator")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeChannelModerator where
  hashWithSalt _salt DescribeChannelModerator' {..} =
    _salt
      `Prelude.hashWithSalt` chimeBearer
      `Prelude.hashWithSalt` channelArn
      `Prelude.hashWithSalt` channelModeratorArn

instance Prelude.NFData DescribeChannelModerator where
  rnf DescribeChannelModerator' {..} =
    Prelude.rnf chimeBearer
      `Prelude.seq` Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf channelModeratorArn

instance Data.ToHeaders DescribeChannelModerator where
  toHeaders DescribeChannelModerator' {..} =
    Prelude.mconcat
      ["x-amz-chime-bearer" Data.=# chimeBearer]

instance Data.ToPath DescribeChannelModerator where
  toPath DescribeChannelModerator' {..} =
    Prelude.mconcat
      [ "/channels/",
        Data.toBS channelArn,
        "/moderators/",
        Data.toBS channelModeratorArn
      ]

instance Data.ToQuery DescribeChannelModerator where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeChannelModeratorResponse' smart constructor.
data DescribeChannelModeratorResponse = DescribeChannelModeratorResponse'
  { -- | The details of the channel moderator.
    channelModerator :: Prelude.Maybe ChannelModerator,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeChannelModeratorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelModerator', 'describeChannelModeratorResponse_channelModerator' - The details of the channel moderator.
--
-- 'httpStatus', 'describeChannelModeratorResponse_httpStatus' - The response's http status code.
newDescribeChannelModeratorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeChannelModeratorResponse
newDescribeChannelModeratorResponse pHttpStatus_ =
  DescribeChannelModeratorResponse'
    { channelModerator =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of the channel moderator.
describeChannelModeratorResponse_channelModerator :: Lens.Lens' DescribeChannelModeratorResponse (Prelude.Maybe ChannelModerator)
describeChannelModeratorResponse_channelModerator = Lens.lens (\DescribeChannelModeratorResponse' {channelModerator} -> channelModerator) (\s@DescribeChannelModeratorResponse' {} a -> s {channelModerator = a} :: DescribeChannelModeratorResponse)

-- | The response's http status code.
describeChannelModeratorResponse_httpStatus :: Lens.Lens' DescribeChannelModeratorResponse Prelude.Int
describeChannelModeratorResponse_httpStatus = Lens.lens (\DescribeChannelModeratorResponse' {httpStatus} -> httpStatus) (\s@DescribeChannelModeratorResponse' {} a -> s {httpStatus = a} :: DescribeChannelModeratorResponse)

instance
  Prelude.NFData
    DescribeChannelModeratorResponse
  where
  rnf DescribeChannelModeratorResponse' {..} =
    Prelude.rnf channelModerator
      `Prelude.seq` Prelude.rnf httpStatus
