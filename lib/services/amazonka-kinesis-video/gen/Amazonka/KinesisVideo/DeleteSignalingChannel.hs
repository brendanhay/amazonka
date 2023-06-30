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
-- Module      : Amazonka.KinesisVideo.DeleteSignalingChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified signaling channel. @DeleteSignalingChannel@ is an
-- asynchronous operation. If you don\'t specify the channel\'s current
-- version, the most recent version is deleted.
module Amazonka.KinesisVideo.DeleteSignalingChannel
  ( -- * Creating a Request
    DeleteSignalingChannel (..),
    newDeleteSignalingChannel,

    -- * Request Lenses
    deleteSignalingChannel_currentVersion,
    deleteSignalingChannel_channelARN,

    -- * Destructuring the Response
    DeleteSignalingChannelResponse (..),
    newDeleteSignalingChannelResponse,

    -- * Response Lenses
    deleteSignalingChannelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisVideo.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteSignalingChannel' smart constructor.
data DeleteSignalingChannel = DeleteSignalingChannel'
  { -- | The current version of the signaling channel that you want to delete.
    -- You can obtain the current version by invoking the
    -- @DescribeSignalingChannel@ or @ListSignalingChannels@ API operations.
    currentVersion :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the signaling channel that you want to
    -- delete.
    channelARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSignalingChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currentVersion', 'deleteSignalingChannel_currentVersion' - The current version of the signaling channel that you want to delete.
-- You can obtain the current version by invoking the
-- @DescribeSignalingChannel@ or @ListSignalingChannels@ API operations.
--
-- 'channelARN', 'deleteSignalingChannel_channelARN' - The Amazon Resource Name (ARN) of the signaling channel that you want to
-- delete.
newDeleteSignalingChannel ::
  -- | 'channelARN'
  Prelude.Text ->
  DeleteSignalingChannel
newDeleteSignalingChannel pChannelARN_ =
  DeleteSignalingChannel'
    { currentVersion =
        Prelude.Nothing,
      channelARN = pChannelARN_
    }

-- | The current version of the signaling channel that you want to delete.
-- You can obtain the current version by invoking the
-- @DescribeSignalingChannel@ or @ListSignalingChannels@ API operations.
deleteSignalingChannel_currentVersion :: Lens.Lens' DeleteSignalingChannel (Prelude.Maybe Prelude.Text)
deleteSignalingChannel_currentVersion = Lens.lens (\DeleteSignalingChannel' {currentVersion} -> currentVersion) (\s@DeleteSignalingChannel' {} a -> s {currentVersion = a} :: DeleteSignalingChannel)

-- | The Amazon Resource Name (ARN) of the signaling channel that you want to
-- delete.
deleteSignalingChannel_channelARN :: Lens.Lens' DeleteSignalingChannel Prelude.Text
deleteSignalingChannel_channelARN = Lens.lens (\DeleteSignalingChannel' {channelARN} -> channelARN) (\s@DeleteSignalingChannel' {} a -> s {channelARN = a} :: DeleteSignalingChannel)

instance Core.AWSRequest DeleteSignalingChannel where
  type
    AWSResponse DeleteSignalingChannel =
      DeleteSignalingChannelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteSignalingChannelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteSignalingChannel where
  hashWithSalt _salt DeleteSignalingChannel' {..} =
    _salt
      `Prelude.hashWithSalt` currentVersion
      `Prelude.hashWithSalt` channelARN

instance Prelude.NFData DeleteSignalingChannel where
  rnf DeleteSignalingChannel' {..} =
    Prelude.rnf currentVersion
      `Prelude.seq` Prelude.rnf channelARN

instance Data.ToHeaders DeleteSignalingChannel where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON DeleteSignalingChannel where
  toJSON DeleteSignalingChannel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CurrentVersion" Data..=)
              Prelude.<$> currentVersion,
            Prelude.Just ("ChannelARN" Data..= channelARN)
          ]
      )

instance Data.ToPath DeleteSignalingChannel where
  toPath = Prelude.const "/deleteSignalingChannel"

instance Data.ToQuery DeleteSignalingChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSignalingChannelResponse' smart constructor.
data DeleteSignalingChannelResponse = DeleteSignalingChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSignalingChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteSignalingChannelResponse_httpStatus' - The response's http status code.
newDeleteSignalingChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteSignalingChannelResponse
newDeleteSignalingChannelResponse pHttpStatus_ =
  DeleteSignalingChannelResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteSignalingChannelResponse_httpStatus :: Lens.Lens' DeleteSignalingChannelResponse Prelude.Int
deleteSignalingChannelResponse_httpStatus = Lens.lens (\DeleteSignalingChannelResponse' {httpStatus} -> httpStatus) (\s@DeleteSignalingChannelResponse' {} a -> s {httpStatus = a} :: DeleteSignalingChannelResponse)

instance
  Prelude.NFData
    DeleteSignalingChannelResponse
  where
  rnf DeleteSignalingChannelResponse' {..} =
    Prelude.rnf httpStatus
