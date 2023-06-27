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
-- Module      : Amazonka.MediaPackageV2.DeleteChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a channel to stop AWS Elemental MediaPackage from receiving
-- further content. You must delete the channel\'s origin endpoints before
-- you can delete the channel.
module Amazonka.MediaPackageV2.DeleteChannel
  ( -- * Creating a Request
    DeleteChannel (..),
    newDeleteChannel,

    -- * Request Lenses
    deleteChannel_channelGroupName,
    deleteChannel_channelName,

    -- * Destructuring the Response
    DeleteChannelResponse (..),
    newDeleteChannelResponse,

    -- * Response Lenses
    deleteChannelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackageV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteChannel' smart constructor.
data DeleteChannel = DeleteChannel'
  { -- | The name that describes the channel group. The name is the primary
    -- identifier for the channel group, and must be unique for your account in
    -- the AWS Region.
    channelGroupName :: Prelude.Text,
    -- | The name that describes the channel. The name is the primary identifier
    -- for the channel, and must be unique for your account in the AWS Region
    -- and channel group.
    channelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelGroupName', 'deleteChannel_channelGroupName' - The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
--
-- 'channelName', 'deleteChannel_channelName' - The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
newDeleteChannel ::
  -- | 'channelGroupName'
  Prelude.Text ->
  -- | 'channelName'
  Prelude.Text ->
  DeleteChannel
newDeleteChannel pChannelGroupName_ pChannelName_ =
  DeleteChannel'
    { channelGroupName =
        pChannelGroupName_,
      channelName = pChannelName_
    }

-- | The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
deleteChannel_channelGroupName :: Lens.Lens' DeleteChannel Prelude.Text
deleteChannel_channelGroupName = Lens.lens (\DeleteChannel' {channelGroupName} -> channelGroupName) (\s@DeleteChannel' {} a -> s {channelGroupName = a} :: DeleteChannel)

-- | The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
deleteChannel_channelName :: Lens.Lens' DeleteChannel Prelude.Text
deleteChannel_channelName = Lens.lens (\DeleteChannel' {channelName} -> channelName) (\s@DeleteChannel' {} a -> s {channelName = a} :: DeleteChannel)

instance Core.AWSRequest DeleteChannel where
  type
    AWSResponse DeleteChannel =
      DeleteChannelResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteChannelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteChannel where
  hashWithSalt _salt DeleteChannel' {..} =
    _salt
      `Prelude.hashWithSalt` channelGroupName
      `Prelude.hashWithSalt` channelName

instance Prelude.NFData DeleteChannel where
  rnf DeleteChannel' {..} =
    Prelude.rnf channelGroupName
      `Prelude.seq` Prelude.rnf channelName

instance Data.ToHeaders DeleteChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteChannel where
  toPath DeleteChannel' {..} =
    Prelude.mconcat
      [ "/channelGroup/",
        Data.toBS channelGroupName,
        "/channel/",
        Data.toBS channelName,
        "/"
      ]

instance Data.ToQuery DeleteChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteChannelResponse' smart constructor.
data DeleteChannelResponse = DeleteChannelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteChannelResponse_httpStatus' - The response's http status code.
newDeleteChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteChannelResponse
newDeleteChannelResponse pHttpStatus_ =
  DeleteChannelResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteChannelResponse_httpStatus :: Lens.Lens' DeleteChannelResponse Prelude.Int
deleteChannelResponse_httpStatus = Lens.lens (\DeleteChannelResponse' {httpStatus} -> httpStatus) (\s@DeleteChannelResponse' {} a -> s {httpStatus = a} :: DeleteChannelResponse)

instance Prelude.NFData DeleteChannelResponse where
  rnf DeleteChannelResponse' {..} =
    Prelude.rnf httpStatus
