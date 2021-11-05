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
-- Module      : Network.AWS.MediaTailor.GetChannelPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a channel\'s IAM policy.
module Network.AWS.MediaTailor.GetChannelPolicy
  ( -- * Creating a Request
    GetChannelPolicy (..),
    newGetChannelPolicy,

    -- * Request Lenses
    getChannelPolicy_channelName,

    -- * Destructuring the Response
    GetChannelPolicyResponse (..),
    newGetChannelPolicyResponse,

    -- * Response Lenses
    getChannelPolicyResponse_policy,
    getChannelPolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaTailor.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetChannelPolicy' smart constructor.
data GetChannelPolicy = GetChannelPolicy'
  { -- | The identifier for the channel you are working on.
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
-- 'channelName', 'getChannelPolicy_channelName' - The identifier for the channel you are working on.
newGetChannelPolicy ::
  -- | 'channelName'
  Prelude.Text ->
  GetChannelPolicy
newGetChannelPolicy pChannelName_ =
  GetChannelPolicy' {channelName = pChannelName_}

-- | The identifier for the channel you are working on.
getChannelPolicy_channelName :: Lens.Lens' GetChannelPolicy Prelude.Text
getChannelPolicy_channelName = Lens.lens (\GetChannelPolicy' {channelName} -> channelName) (\s@GetChannelPolicy' {} a -> s {channelName = a} :: GetChannelPolicy)

instance Core.AWSRequest GetChannelPolicy where
  type
    AWSResponse GetChannelPolicy =
      GetChannelPolicyResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetChannelPolicyResponse'
            Prelude.<$> (x Core..?> "Policy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetChannelPolicy

instance Prelude.NFData GetChannelPolicy

instance Core.ToHeaders GetChannelPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetChannelPolicy where
  toPath GetChannelPolicy' {..} =
    Prelude.mconcat
      ["/channel/", Core.toBS channelName, "/policy"]

instance Core.ToQuery GetChannelPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetChannelPolicyResponse' smart constructor.
data GetChannelPolicyResponse = GetChannelPolicyResponse'
  { -- | The IAM policy for the channel.
    policy :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
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
-- 'policy', 'getChannelPolicyResponse_policy' - The IAM policy for the channel.
--
-- 'httpStatus', 'getChannelPolicyResponse_httpStatus' - The response's http status code.
newGetChannelPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetChannelPolicyResponse
newGetChannelPolicyResponse pHttpStatus_ =
  GetChannelPolicyResponse'
    { policy = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The IAM policy for the channel.
getChannelPolicyResponse_policy :: Lens.Lens' GetChannelPolicyResponse (Prelude.Maybe Prelude.Text)
getChannelPolicyResponse_policy = Lens.lens (\GetChannelPolicyResponse' {policy} -> policy) (\s@GetChannelPolicyResponse' {} a -> s {policy = a} :: GetChannelPolicyResponse)

-- | The response's http status code.
getChannelPolicyResponse_httpStatus :: Lens.Lens' GetChannelPolicyResponse Prelude.Int
getChannelPolicyResponse_httpStatus = Lens.lens (\GetChannelPolicyResponse' {httpStatus} -> httpStatus) (\s@GetChannelPolicyResponse' {} a -> s {httpStatus = a} :: GetChannelPolicyResponse)

instance Prelude.NFData GetChannelPolicyResponse
