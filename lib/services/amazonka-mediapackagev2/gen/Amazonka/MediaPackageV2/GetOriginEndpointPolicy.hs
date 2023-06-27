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
-- Module      : Amazonka.MediaPackageV2.GetOriginEndpointPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the specified origin endpoint policy that\'s configured in AWS
-- Elemental MediaPackage.
module Amazonka.MediaPackageV2.GetOriginEndpointPolicy
  ( -- * Creating a Request
    GetOriginEndpointPolicy (..),
    newGetOriginEndpointPolicy,

    -- * Request Lenses
    getOriginEndpointPolicy_channelGroupName,
    getOriginEndpointPolicy_channelName,
    getOriginEndpointPolicy_originEndpointName,

    -- * Destructuring the Response
    GetOriginEndpointPolicyResponse (..),
    newGetOriginEndpointPolicyResponse,

    -- * Response Lenses
    getOriginEndpointPolicyResponse_httpStatus,
    getOriginEndpointPolicyResponse_channelGroupName,
    getOriginEndpointPolicyResponse_channelName,
    getOriginEndpointPolicyResponse_originEndpointName,
    getOriginEndpointPolicyResponse_policy,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackageV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetOriginEndpointPolicy' smart constructor.
data GetOriginEndpointPolicy = GetOriginEndpointPolicy'
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
-- Create a value of 'GetOriginEndpointPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelGroupName', 'getOriginEndpointPolicy_channelGroupName' - The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
--
-- 'channelName', 'getOriginEndpointPolicy_channelName' - The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
--
-- 'originEndpointName', 'getOriginEndpointPolicy_originEndpointName' - The name that describes the origin endpoint. The name is the primary
-- identifier for the origin endpoint, and and must be unique for your
-- account in the AWS Region and channel.
newGetOriginEndpointPolicy ::
  -- | 'channelGroupName'
  Prelude.Text ->
  -- | 'channelName'
  Prelude.Text ->
  -- | 'originEndpointName'
  Prelude.Text ->
  GetOriginEndpointPolicy
newGetOriginEndpointPolicy
  pChannelGroupName_
  pChannelName_
  pOriginEndpointName_ =
    GetOriginEndpointPolicy'
      { channelGroupName =
          pChannelGroupName_,
        channelName = pChannelName_,
        originEndpointName = pOriginEndpointName_
      }

-- | The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
getOriginEndpointPolicy_channelGroupName :: Lens.Lens' GetOriginEndpointPolicy Prelude.Text
getOriginEndpointPolicy_channelGroupName = Lens.lens (\GetOriginEndpointPolicy' {channelGroupName} -> channelGroupName) (\s@GetOriginEndpointPolicy' {} a -> s {channelGroupName = a} :: GetOriginEndpointPolicy)

-- | The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
getOriginEndpointPolicy_channelName :: Lens.Lens' GetOriginEndpointPolicy Prelude.Text
getOriginEndpointPolicy_channelName = Lens.lens (\GetOriginEndpointPolicy' {channelName} -> channelName) (\s@GetOriginEndpointPolicy' {} a -> s {channelName = a} :: GetOriginEndpointPolicy)

-- | The name that describes the origin endpoint. The name is the primary
-- identifier for the origin endpoint, and and must be unique for your
-- account in the AWS Region and channel.
getOriginEndpointPolicy_originEndpointName :: Lens.Lens' GetOriginEndpointPolicy Prelude.Text
getOriginEndpointPolicy_originEndpointName = Lens.lens (\GetOriginEndpointPolicy' {originEndpointName} -> originEndpointName) (\s@GetOriginEndpointPolicy' {} a -> s {originEndpointName = a} :: GetOriginEndpointPolicy)

instance Core.AWSRequest GetOriginEndpointPolicy where
  type
    AWSResponse GetOriginEndpointPolicy =
      GetOriginEndpointPolicyResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetOriginEndpointPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ChannelGroupName")
            Prelude.<*> (x Data..:> "ChannelName")
            Prelude.<*> (x Data..:> "OriginEndpointName")
            Prelude.<*> (x Data..:> "Policy")
      )

instance Prelude.Hashable GetOriginEndpointPolicy where
  hashWithSalt _salt GetOriginEndpointPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` channelGroupName
      `Prelude.hashWithSalt` channelName
      `Prelude.hashWithSalt` originEndpointName

instance Prelude.NFData GetOriginEndpointPolicy where
  rnf GetOriginEndpointPolicy' {..} =
    Prelude.rnf channelGroupName
      `Prelude.seq` Prelude.rnf channelName
      `Prelude.seq` Prelude.rnf originEndpointName

instance Data.ToHeaders GetOriginEndpointPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetOriginEndpointPolicy where
  toPath GetOriginEndpointPolicy' {..} =
    Prelude.mconcat
      [ "/channelGroup/",
        Data.toBS channelGroupName,
        "/channel/",
        Data.toBS channelName,
        "/originEndpoint/",
        Data.toBS originEndpointName,
        "/policy"
      ]

instance Data.ToQuery GetOriginEndpointPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetOriginEndpointPolicyResponse' smart constructor.
data GetOriginEndpointPolicyResponse = GetOriginEndpointPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name that describes the channel group. The name is the primary
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
    -- | The policy assigned to the origin endpoint.
    policy :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetOriginEndpointPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getOriginEndpointPolicyResponse_httpStatus' - The response's http status code.
--
-- 'channelGroupName', 'getOriginEndpointPolicyResponse_channelGroupName' - The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
--
-- 'channelName', 'getOriginEndpointPolicyResponse_channelName' - The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
--
-- 'originEndpointName', 'getOriginEndpointPolicyResponse_originEndpointName' - The name that describes the origin endpoint. The name is the primary
-- identifier for the origin endpoint, and and must be unique for your
-- account in the AWS Region and channel.
--
-- 'policy', 'getOriginEndpointPolicyResponse_policy' - The policy assigned to the origin endpoint.
newGetOriginEndpointPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'channelGroupName'
  Prelude.Text ->
  -- | 'channelName'
  Prelude.Text ->
  -- | 'originEndpointName'
  Prelude.Text ->
  -- | 'policy'
  Prelude.Text ->
  GetOriginEndpointPolicyResponse
newGetOriginEndpointPolicyResponse
  pHttpStatus_
  pChannelGroupName_
  pChannelName_
  pOriginEndpointName_
  pPolicy_ =
    GetOriginEndpointPolicyResponse'
      { httpStatus =
          pHttpStatus_,
        channelGroupName = pChannelGroupName_,
        channelName = pChannelName_,
        originEndpointName = pOriginEndpointName_,
        policy = pPolicy_
      }

-- | The response's http status code.
getOriginEndpointPolicyResponse_httpStatus :: Lens.Lens' GetOriginEndpointPolicyResponse Prelude.Int
getOriginEndpointPolicyResponse_httpStatus = Lens.lens (\GetOriginEndpointPolicyResponse' {httpStatus} -> httpStatus) (\s@GetOriginEndpointPolicyResponse' {} a -> s {httpStatus = a} :: GetOriginEndpointPolicyResponse)

-- | The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
getOriginEndpointPolicyResponse_channelGroupName :: Lens.Lens' GetOriginEndpointPolicyResponse Prelude.Text
getOriginEndpointPolicyResponse_channelGroupName = Lens.lens (\GetOriginEndpointPolicyResponse' {channelGroupName} -> channelGroupName) (\s@GetOriginEndpointPolicyResponse' {} a -> s {channelGroupName = a} :: GetOriginEndpointPolicyResponse)

-- | The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
getOriginEndpointPolicyResponse_channelName :: Lens.Lens' GetOriginEndpointPolicyResponse Prelude.Text
getOriginEndpointPolicyResponse_channelName = Lens.lens (\GetOriginEndpointPolicyResponse' {channelName} -> channelName) (\s@GetOriginEndpointPolicyResponse' {} a -> s {channelName = a} :: GetOriginEndpointPolicyResponse)

-- | The name that describes the origin endpoint. The name is the primary
-- identifier for the origin endpoint, and and must be unique for your
-- account in the AWS Region and channel.
getOriginEndpointPolicyResponse_originEndpointName :: Lens.Lens' GetOriginEndpointPolicyResponse Prelude.Text
getOriginEndpointPolicyResponse_originEndpointName = Lens.lens (\GetOriginEndpointPolicyResponse' {originEndpointName} -> originEndpointName) (\s@GetOriginEndpointPolicyResponse' {} a -> s {originEndpointName = a} :: GetOriginEndpointPolicyResponse)

-- | The policy assigned to the origin endpoint.
getOriginEndpointPolicyResponse_policy :: Lens.Lens' GetOriginEndpointPolicyResponse Prelude.Text
getOriginEndpointPolicyResponse_policy = Lens.lens (\GetOriginEndpointPolicyResponse' {policy} -> policy) (\s@GetOriginEndpointPolicyResponse' {} a -> s {policy = a} :: GetOriginEndpointPolicyResponse)

instance
  Prelude.NFData
    GetOriginEndpointPolicyResponse
  where
  rnf GetOriginEndpointPolicyResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf channelGroupName
      `Prelude.seq` Prelude.rnf channelName
      `Prelude.seq` Prelude.rnf originEndpointName
      `Prelude.seq` Prelude.rnf policy
