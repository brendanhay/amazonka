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
-- Module      : Amazonka.MediaPackageV2.GetChannelPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the specified channel policy that\'s configured in AWS
-- Elemental MediaPackage. With policies, you can specify who has access to
-- AWS resources and what actions they can perform on those resources.
module Amazonka.MediaPackageV2.GetChannelPolicy
  ( -- * Creating a Request
    GetChannelPolicy (..),
    newGetChannelPolicy,

    -- * Request Lenses
    getChannelPolicy_channelGroupName,
    getChannelPolicy_channelName,

    -- * Destructuring the Response
    GetChannelPolicyResponse (..),
    newGetChannelPolicyResponse,

    -- * Response Lenses
    getChannelPolicyResponse_httpStatus,
    getChannelPolicyResponse_channelGroupName,
    getChannelPolicyResponse_channelName,
    getChannelPolicyResponse_policy,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackageV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetChannelPolicy' smart constructor.
data GetChannelPolicy = GetChannelPolicy'
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
-- Create a value of 'GetChannelPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelGroupName', 'getChannelPolicy_channelGroupName' - The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
--
-- 'channelName', 'getChannelPolicy_channelName' - The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
newGetChannelPolicy ::
  -- | 'channelGroupName'
  Prelude.Text ->
  -- | 'channelName'
  Prelude.Text ->
  GetChannelPolicy
newGetChannelPolicy pChannelGroupName_ pChannelName_ =
  GetChannelPolicy'
    { channelGroupName =
        pChannelGroupName_,
      channelName = pChannelName_
    }

-- | The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
getChannelPolicy_channelGroupName :: Lens.Lens' GetChannelPolicy Prelude.Text
getChannelPolicy_channelGroupName = Lens.lens (\GetChannelPolicy' {channelGroupName} -> channelGroupName) (\s@GetChannelPolicy' {} a -> s {channelGroupName = a} :: GetChannelPolicy)

-- | The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
getChannelPolicy_channelName :: Lens.Lens' GetChannelPolicy Prelude.Text
getChannelPolicy_channelName = Lens.lens (\GetChannelPolicy' {channelName} -> channelName) (\s@GetChannelPolicy' {} a -> s {channelName = a} :: GetChannelPolicy)

instance Core.AWSRequest GetChannelPolicy where
  type
    AWSResponse GetChannelPolicy =
      GetChannelPolicyResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetChannelPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ChannelGroupName")
            Prelude.<*> (x Data..:> "ChannelName")
            Prelude.<*> (x Data..:> "Policy")
      )

instance Prelude.Hashable GetChannelPolicy where
  hashWithSalt _salt GetChannelPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` channelGroupName
      `Prelude.hashWithSalt` channelName

instance Prelude.NFData GetChannelPolicy where
  rnf GetChannelPolicy' {..} =
    Prelude.rnf channelGroupName
      `Prelude.seq` Prelude.rnf channelName

instance Data.ToHeaders GetChannelPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetChannelPolicy where
  toPath GetChannelPolicy' {..} =
    Prelude.mconcat
      [ "/channelGroup/",
        Data.toBS channelGroupName,
        "/channel/",
        Data.toBS channelName,
        "/policy"
      ]

instance Data.ToQuery GetChannelPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetChannelPolicyResponse' smart constructor.
data GetChannelPolicyResponse = GetChannelPolicyResponse'
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
    -- | The policy assigned to the channel.
    policy :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetChannelPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getChannelPolicyResponse_httpStatus' - The response's http status code.
--
-- 'channelGroupName', 'getChannelPolicyResponse_channelGroupName' - The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
--
-- 'channelName', 'getChannelPolicyResponse_channelName' - The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
--
-- 'policy', 'getChannelPolicyResponse_policy' - The policy assigned to the channel.
newGetChannelPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'channelGroupName'
  Prelude.Text ->
  -- | 'channelName'
  Prelude.Text ->
  -- | 'policy'
  Prelude.Text ->
  GetChannelPolicyResponse
newGetChannelPolicyResponse
  pHttpStatus_
  pChannelGroupName_
  pChannelName_
  pPolicy_ =
    GetChannelPolicyResponse'
      { httpStatus =
          pHttpStatus_,
        channelGroupName = pChannelGroupName_,
        channelName = pChannelName_,
        policy = pPolicy_
      }

-- | The response's http status code.
getChannelPolicyResponse_httpStatus :: Lens.Lens' GetChannelPolicyResponse Prelude.Int
getChannelPolicyResponse_httpStatus = Lens.lens (\GetChannelPolicyResponse' {httpStatus} -> httpStatus) (\s@GetChannelPolicyResponse' {} a -> s {httpStatus = a} :: GetChannelPolicyResponse)

-- | The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
getChannelPolicyResponse_channelGroupName :: Lens.Lens' GetChannelPolicyResponse Prelude.Text
getChannelPolicyResponse_channelGroupName = Lens.lens (\GetChannelPolicyResponse' {channelGroupName} -> channelGroupName) (\s@GetChannelPolicyResponse' {} a -> s {channelGroupName = a} :: GetChannelPolicyResponse)

-- | The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
getChannelPolicyResponse_channelName :: Lens.Lens' GetChannelPolicyResponse Prelude.Text
getChannelPolicyResponse_channelName = Lens.lens (\GetChannelPolicyResponse' {channelName} -> channelName) (\s@GetChannelPolicyResponse' {} a -> s {channelName = a} :: GetChannelPolicyResponse)

-- | The policy assigned to the channel.
getChannelPolicyResponse_policy :: Lens.Lens' GetChannelPolicyResponse Prelude.Text
getChannelPolicyResponse_policy = Lens.lens (\GetChannelPolicyResponse' {policy} -> policy) (\s@GetChannelPolicyResponse' {} a -> s {policy = a} :: GetChannelPolicyResponse)

instance Prelude.NFData GetChannelPolicyResponse where
  rnf GetChannelPolicyResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf channelGroupName
      `Prelude.seq` Prelude.rnf channelName
      `Prelude.seq` Prelude.rnf policy
