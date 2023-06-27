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
-- Module      : Amazonka.MediaPackageV2.DeleteOriginEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Origin endpoints can serve content until they\'re deleted. Delete the
-- endpoint if it should no longer respond to playback requests. You must
-- delete all endpoints from a channel before you can delete the channel.
module Amazonka.MediaPackageV2.DeleteOriginEndpoint
  ( -- * Creating a Request
    DeleteOriginEndpoint (..),
    newDeleteOriginEndpoint,

    -- * Request Lenses
    deleteOriginEndpoint_channelGroupName,
    deleteOriginEndpoint_channelName,
    deleteOriginEndpoint_originEndpointName,

    -- * Destructuring the Response
    DeleteOriginEndpointResponse (..),
    newDeleteOriginEndpointResponse,

    -- * Response Lenses
    deleteOriginEndpointResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackageV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteOriginEndpoint' smart constructor.
data DeleteOriginEndpoint = DeleteOriginEndpoint'
  { -- | The name that describes the channel group. The name is the primary
    -- identifier for the channel group, and must be unique for your account in
    -- the AWS Region.
    channelGroupName :: Prelude.Text,
    -- | The name that describes the channel. The name is the primary identifier
    -- for the channel, and must be unique for your account in the AWS Region
    -- and channel group.
    channelName :: Prelude.Text,
    -- | The name that describes the origin endpoint. The name is the primary
    -- identifier for the origin endpoint, and and must be unique for your
    -- account in the AWS Region and channel.
    originEndpointName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteOriginEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelGroupName', 'deleteOriginEndpoint_channelGroupName' - The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
--
-- 'channelName', 'deleteOriginEndpoint_channelName' - The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
--
-- 'originEndpointName', 'deleteOriginEndpoint_originEndpointName' - The name that describes the origin endpoint. The name is the primary
-- identifier for the origin endpoint, and and must be unique for your
-- account in the AWS Region and channel.
newDeleteOriginEndpoint ::
  -- | 'channelGroupName'
  Prelude.Text ->
  -- | 'channelName'
  Prelude.Text ->
  -- | 'originEndpointName'
  Prelude.Text ->
  DeleteOriginEndpoint
newDeleteOriginEndpoint
  pChannelGroupName_
  pChannelName_
  pOriginEndpointName_ =
    DeleteOriginEndpoint'
      { channelGroupName =
          pChannelGroupName_,
        channelName = pChannelName_,
        originEndpointName = pOriginEndpointName_
      }

-- | The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
deleteOriginEndpoint_channelGroupName :: Lens.Lens' DeleteOriginEndpoint Prelude.Text
deleteOriginEndpoint_channelGroupName = Lens.lens (\DeleteOriginEndpoint' {channelGroupName} -> channelGroupName) (\s@DeleteOriginEndpoint' {} a -> s {channelGroupName = a} :: DeleteOriginEndpoint)

-- | The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
deleteOriginEndpoint_channelName :: Lens.Lens' DeleteOriginEndpoint Prelude.Text
deleteOriginEndpoint_channelName = Lens.lens (\DeleteOriginEndpoint' {channelName} -> channelName) (\s@DeleteOriginEndpoint' {} a -> s {channelName = a} :: DeleteOriginEndpoint)

-- | The name that describes the origin endpoint. The name is the primary
-- identifier for the origin endpoint, and and must be unique for your
-- account in the AWS Region and channel.
deleteOriginEndpoint_originEndpointName :: Lens.Lens' DeleteOriginEndpoint Prelude.Text
deleteOriginEndpoint_originEndpointName = Lens.lens (\DeleteOriginEndpoint' {originEndpointName} -> originEndpointName) (\s@DeleteOriginEndpoint' {} a -> s {originEndpointName = a} :: DeleteOriginEndpoint)

instance Core.AWSRequest DeleteOriginEndpoint where
  type
    AWSResponse DeleteOriginEndpoint =
      DeleteOriginEndpointResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteOriginEndpointResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteOriginEndpoint where
  hashWithSalt _salt DeleteOriginEndpoint' {..} =
    _salt
      `Prelude.hashWithSalt` channelGroupName
      `Prelude.hashWithSalt` channelName
      `Prelude.hashWithSalt` originEndpointName

instance Prelude.NFData DeleteOriginEndpoint where
  rnf DeleteOriginEndpoint' {..} =
    Prelude.rnf channelGroupName
      `Prelude.seq` Prelude.rnf channelName
      `Prelude.seq` Prelude.rnf originEndpointName

instance Data.ToHeaders DeleteOriginEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteOriginEndpoint where
  toPath DeleteOriginEndpoint' {..} =
    Prelude.mconcat
      [ "/channelGroup/",
        Data.toBS channelGroupName,
        "/channel/",
        Data.toBS channelName,
        "/originEndpoint/",
        Data.toBS originEndpointName
      ]

instance Data.ToQuery DeleteOriginEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteOriginEndpointResponse' smart constructor.
data DeleteOriginEndpointResponse = DeleteOriginEndpointResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteOriginEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteOriginEndpointResponse_httpStatus' - The response's http status code.
newDeleteOriginEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteOriginEndpointResponse
newDeleteOriginEndpointResponse pHttpStatus_ =
  DeleteOriginEndpointResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteOriginEndpointResponse_httpStatus :: Lens.Lens' DeleteOriginEndpointResponse Prelude.Int
deleteOriginEndpointResponse_httpStatus = Lens.lens (\DeleteOriginEndpointResponse' {httpStatus} -> httpStatus) (\s@DeleteOriginEndpointResponse' {} a -> s {httpStatus = a} :: DeleteOriginEndpointResponse)

instance Prelude.NFData DeleteOriginEndpointResponse where
  rnf DeleteOriginEndpointResponse' {..} =
    Prelude.rnf httpStatus
