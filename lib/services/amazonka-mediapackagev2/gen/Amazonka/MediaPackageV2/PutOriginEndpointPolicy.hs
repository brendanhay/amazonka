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
-- Module      : Amazonka.MediaPackageV2.PutOriginEndpointPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches an IAM policy to the specified origin endpoint. You can attach
-- only one policy with each request.
module Amazonka.MediaPackageV2.PutOriginEndpointPolicy
  ( -- * Creating a Request
    PutOriginEndpointPolicy (..),
    newPutOriginEndpointPolicy,

    -- * Request Lenses
    putOriginEndpointPolicy_channelGroupName,
    putOriginEndpointPolicy_channelName,
    putOriginEndpointPolicy_originEndpointName,
    putOriginEndpointPolicy_policy,

    -- * Destructuring the Response
    PutOriginEndpointPolicyResponse (..),
    newPutOriginEndpointPolicyResponse,

    -- * Response Lenses
    putOriginEndpointPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackageV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutOriginEndpointPolicy' smart constructor.
data PutOriginEndpointPolicy = PutOriginEndpointPolicy'
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
    originEndpointName :: Prelude.Text,
    -- | The policy to attach to the specified origin endpoint.
    policy :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutOriginEndpointPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelGroupName', 'putOriginEndpointPolicy_channelGroupName' - The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
--
-- 'channelName', 'putOriginEndpointPolicy_channelName' - The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
--
-- 'originEndpointName', 'putOriginEndpointPolicy_originEndpointName' - The name that describes the origin endpoint. The name is the primary
-- identifier for the origin endpoint, and and must be unique for your
-- account in the AWS Region and channel.
--
-- 'policy', 'putOriginEndpointPolicy_policy' - The policy to attach to the specified origin endpoint.
newPutOriginEndpointPolicy ::
  -- | 'channelGroupName'
  Prelude.Text ->
  -- | 'channelName'
  Prelude.Text ->
  -- | 'originEndpointName'
  Prelude.Text ->
  -- | 'policy'
  Prelude.Text ->
  PutOriginEndpointPolicy
newPutOriginEndpointPolicy
  pChannelGroupName_
  pChannelName_
  pOriginEndpointName_
  pPolicy_ =
    PutOriginEndpointPolicy'
      { channelGroupName =
          pChannelGroupName_,
        channelName = pChannelName_,
        originEndpointName = pOriginEndpointName_,
        policy = pPolicy_
      }

-- | The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
putOriginEndpointPolicy_channelGroupName :: Lens.Lens' PutOriginEndpointPolicy Prelude.Text
putOriginEndpointPolicy_channelGroupName = Lens.lens (\PutOriginEndpointPolicy' {channelGroupName} -> channelGroupName) (\s@PutOriginEndpointPolicy' {} a -> s {channelGroupName = a} :: PutOriginEndpointPolicy)

-- | The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
putOriginEndpointPolicy_channelName :: Lens.Lens' PutOriginEndpointPolicy Prelude.Text
putOriginEndpointPolicy_channelName = Lens.lens (\PutOriginEndpointPolicy' {channelName} -> channelName) (\s@PutOriginEndpointPolicy' {} a -> s {channelName = a} :: PutOriginEndpointPolicy)

-- | The name that describes the origin endpoint. The name is the primary
-- identifier for the origin endpoint, and and must be unique for your
-- account in the AWS Region and channel.
putOriginEndpointPolicy_originEndpointName :: Lens.Lens' PutOriginEndpointPolicy Prelude.Text
putOriginEndpointPolicy_originEndpointName = Lens.lens (\PutOriginEndpointPolicy' {originEndpointName} -> originEndpointName) (\s@PutOriginEndpointPolicy' {} a -> s {originEndpointName = a} :: PutOriginEndpointPolicy)

-- | The policy to attach to the specified origin endpoint.
putOriginEndpointPolicy_policy :: Lens.Lens' PutOriginEndpointPolicy Prelude.Text
putOriginEndpointPolicy_policy = Lens.lens (\PutOriginEndpointPolicy' {policy} -> policy) (\s@PutOriginEndpointPolicy' {} a -> s {policy = a} :: PutOriginEndpointPolicy)

instance Core.AWSRequest PutOriginEndpointPolicy where
  type
    AWSResponse PutOriginEndpointPolicy =
      PutOriginEndpointPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutOriginEndpointPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutOriginEndpointPolicy where
  hashWithSalt _salt PutOriginEndpointPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` channelGroupName
      `Prelude.hashWithSalt` channelName
      `Prelude.hashWithSalt` originEndpointName
      `Prelude.hashWithSalt` policy

instance Prelude.NFData PutOriginEndpointPolicy where
  rnf PutOriginEndpointPolicy' {..} =
    Prelude.rnf channelGroupName
      `Prelude.seq` Prelude.rnf channelName
      `Prelude.seq` Prelude.rnf originEndpointName
      `Prelude.seq` Prelude.rnf policy

instance Data.ToHeaders PutOriginEndpointPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutOriginEndpointPolicy where
  toJSON PutOriginEndpointPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Policy" Data..= policy)]
      )

instance Data.ToPath PutOriginEndpointPolicy where
  toPath PutOriginEndpointPolicy' {..} =
    Prelude.mconcat
      [ "/channelGroup/",
        Data.toBS channelGroupName,
        "/channel/",
        Data.toBS channelName,
        "/originEndpoint/",
        Data.toBS originEndpointName,
        "/policy"
      ]

instance Data.ToQuery PutOriginEndpointPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutOriginEndpointPolicyResponse' smart constructor.
data PutOriginEndpointPolicyResponse = PutOriginEndpointPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutOriginEndpointPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putOriginEndpointPolicyResponse_httpStatus' - The response's http status code.
newPutOriginEndpointPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutOriginEndpointPolicyResponse
newPutOriginEndpointPolicyResponse pHttpStatus_ =
  PutOriginEndpointPolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putOriginEndpointPolicyResponse_httpStatus :: Lens.Lens' PutOriginEndpointPolicyResponse Prelude.Int
putOriginEndpointPolicyResponse_httpStatus = Lens.lens (\PutOriginEndpointPolicyResponse' {httpStatus} -> httpStatus) (\s@PutOriginEndpointPolicyResponse' {} a -> s {httpStatus = a} :: PutOriginEndpointPolicyResponse)

instance
  Prelude.NFData
    PutOriginEndpointPolicyResponse
  where
  rnf PutOriginEndpointPolicyResponse' {..} =
    Prelude.rnf httpStatus
