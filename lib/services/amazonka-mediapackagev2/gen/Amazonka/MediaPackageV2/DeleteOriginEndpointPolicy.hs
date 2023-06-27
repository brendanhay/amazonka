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
-- Module      : Amazonka.MediaPackageV2.DeleteOriginEndpointPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an origin endpoint policy.
module Amazonka.MediaPackageV2.DeleteOriginEndpointPolicy
  ( -- * Creating a Request
    DeleteOriginEndpointPolicy (..),
    newDeleteOriginEndpointPolicy,

    -- * Request Lenses
    deleteOriginEndpointPolicy_channelGroupName,
    deleteOriginEndpointPolicy_channelName,
    deleteOriginEndpointPolicy_originEndpointName,

    -- * Destructuring the Response
    DeleteOriginEndpointPolicyResponse (..),
    newDeleteOriginEndpointPolicyResponse,

    -- * Response Lenses
    deleteOriginEndpointPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackageV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteOriginEndpointPolicy' smart constructor.
data DeleteOriginEndpointPolicy = DeleteOriginEndpointPolicy'
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
-- Create a value of 'DeleteOriginEndpointPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelGroupName', 'deleteOriginEndpointPolicy_channelGroupName' - The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
--
-- 'channelName', 'deleteOriginEndpointPolicy_channelName' - The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
--
-- 'originEndpointName', 'deleteOriginEndpointPolicy_originEndpointName' - The name that describes the origin endpoint. The name is the primary
-- identifier for the origin endpoint, and and must be unique for your
-- account in the AWS Region and channel.
newDeleteOriginEndpointPolicy ::
  -- | 'channelGroupName'
  Prelude.Text ->
  -- | 'channelName'
  Prelude.Text ->
  -- | 'originEndpointName'
  Prelude.Text ->
  DeleteOriginEndpointPolicy
newDeleteOriginEndpointPolicy
  pChannelGroupName_
  pChannelName_
  pOriginEndpointName_ =
    DeleteOriginEndpointPolicy'
      { channelGroupName =
          pChannelGroupName_,
        channelName = pChannelName_,
        originEndpointName = pOriginEndpointName_
      }

-- | The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
deleteOriginEndpointPolicy_channelGroupName :: Lens.Lens' DeleteOriginEndpointPolicy Prelude.Text
deleteOriginEndpointPolicy_channelGroupName = Lens.lens (\DeleteOriginEndpointPolicy' {channelGroupName} -> channelGroupName) (\s@DeleteOriginEndpointPolicy' {} a -> s {channelGroupName = a} :: DeleteOriginEndpointPolicy)

-- | The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
deleteOriginEndpointPolicy_channelName :: Lens.Lens' DeleteOriginEndpointPolicy Prelude.Text
deleteOriginEndpointPolicy_channelName = Lens.lens (\DeleteOriginEndpointPolicy' {channelName} -> channelName) (\s@DeleteOriginEndpointPolicy' {} a -> s {channelName = a} :: DeleteOriginEndpointPolicy)

-- | The name that describes the origin endpoint. The name is the primary
-- identifier for the origin endpoint, and and must be unique for your
-- account in the AWS Region and channel.
deleteOriginEndpointPolicy_originEndpointName :: Lens.Lens' DeleteOriginEndpointPolicy Prelude.Text
deleteOriginEndpointPolicy_originEndpointName = Lens.lens (\DeleteOriginEndpointPolicy' {originEndpointName} -> originEndpointName) (\s@DeleteOriginEndpointPolicy' {} a -> s {originEndpointName = a} :: DeleteOriginEndpointPolicy)

instance Core.AWSRequest DeleteOriginEndpointPolicy where
  type
    AWSResponse DeleteOriginEndpointPolicy =
      DeleteOriginEndpointPolicyResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteOriginEndpointPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteOriginEndpointPolicy where
  hashWithSalt _salt DeleteOriginEndpointPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` channelGroupName
      `Prelude.hashWithSalt` channelName
      `Prelude.hashWithSalt` originEndpointName

instance Prelude.NFData DeleteOriginEndpointPolicy where
  rnf DeleteOriginEndpointPolicy' {..} =
    Prelude.rnf channelGroupName
      `Prelude.seq` Prelude.rnf channelName
      `Prelude.seq` Prelude.rnf originEndpointName

instance Data.ToHeaders DeleteOriginEndpointPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteOriginEndpointPolicy where
  toPath DeleteOriginEndpointPolicy' {..} =
    Prelude.mconcat
      [ "/channelGroup/",
        Data.toBS channelGroupName,
        "/channel/",
        Data.toBS channelName,
        "/originEndpoint/",
        Data.toBS originEndpointName,
        "/policy"
      ]

instance Data.ToQuery DeleteOriginEndpointPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteOriginEndpointPolicyResponse' smart constructor.
data DeleteOriginEndpointPolicyResponse = DeleteOriginEndpointPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteOriginEndpointPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteOriginEndpointPolicyResponse_httpStatus' - The response's http status code.
newDeleteOriginEndpointPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteOriginEndpointPolicyResponse
newDeleteOriginEndpointPolicyResponse pHttpStatus_ =
  DeleteOriginEndpointPolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteOriginEndpointPolicyResponse_httpStatus :: Lens.Lens' DeleteOriginEndpointPolicyResponse Prelude.Int
deleteOriginEndpointPolicyResponse_httpStatus = Lens.lens (\DeleteOriginEndpointPolicyResponse' {httpStatus} -> httpStatus) (\s@DeleteOriginEndpointPolicyResponse' {} a -> s {httpStatus = a} :: DeleteOriginEndpointPolicyResponse)

instance
  Prelude.NFData
    DeleteOriginEndpointPolicyResponse
  where
  rnf DeleteOriginEndpointPolicyResponse' {..} =
    Prelude.rnf httpStatus
