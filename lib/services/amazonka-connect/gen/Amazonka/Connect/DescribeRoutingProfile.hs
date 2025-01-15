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
-- Module      : Amazonka.Connect.DescribeRoutingProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified routing profile.
module Amazonka.Connect.DescribeRoutingProfile
  ( -- * Creating a Request
    DescribeRoutingProfile (..),
    newDescribeRoutingProfile,

    -- * Request Lenses
    describeRoutingProfile_instanceId,
    describeRoutingProfile_routingProfileId,

    -- * Destructuring the Response
    DescribeRoutingProfileResponse (..),
    newDescribeRoutingProfileResponse,

    -- * Response Lenses
    describeRoutingProfileResponse_routingProfile,
    describeRoutingProfileResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeRoutingProfile' smart constructor.
data DescribeRoutingProfile = DescribeRoutingProfile'
  { -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier of the routing profile.
    routingProfileId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRoutingProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'describeRoutingProfile_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'routingProfileId', 'describeRoutingProfile_routingProfileId' - The identifier of the routing profile.
newDescribeRoutingProfile ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'routingProfileId'
  Prelude.Text ->
  DescribeRoutingProfile
newDescribeRoutingProfile
  pInstanceId_
  pRoutingProfileId_ =
    DescribeRoutingProfile'
      { instanceId = pInstanceId_,
        routingProfileId = pRoutingProfileId_
      }

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
describeRoutingProfile_instanceId :: Lens.Lens' DescribeRoutingProfile Prelude.Text
describeRoutingProfile_instanceId = Lens.lens (\DescribeRoutingProfile' {instanceId} -> instanceId) (\s@DescribeRoutingProfile' {} a -> s {instanceId = a} :: DescribeRoutingProfile)

-- | The identifier of the routing profile.
describeRoutingProfile_routingProfileId :: Lens.Lens' DescribeRoutingProfile Prelude.Text
describeRoutingProfile_routingProfileId = Lens.lens (\DescribeRoutingProfile' {routingProfileId} -> routingProfileId) (\s@DescribeRoutingProfile' {} a -> s {routingProfileId = a} :: DescribeRoutingProfile)

instance Core.AWSRequest DescribeRoutingProfile where
  type
    AWSResponse DescribeRoutingProfile =
      DescribeRoutingProfileResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRoutingProfileResponse'
            Prelude.<$> (x Data..?> "RoutingProfile")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeRoutingProfile where
  hashWithSalt _salt DescribeRoutingProfile' {..} =
    _salt
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` routingProfileId

instance Prelude.NFData DescribeRoutingProfile where
  rnf DescribeRoutingProfile' {..} =
    Prelude.rnf instanceId `Prelude.seq`
      Prelude.rnf routingProfileId

instance Data.ToHeaders DescribeRoutingProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeRoutingProfile where
  toPath DescribeRoutingProfile' {..} =
    Prelude.mconcat
      [ "/routing-profiles/",
        Data.toBS instanceId,
        "/",
        Data.toBS routingProfileId
      ]

instance Data.ToQuery DescribeRoutingProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeRoutingProfileResponse' smart constructor.
data DescribeRoutingProfileResponse = DescribeRoutingProfileResponse'
  { -- | The routing profile.
    routingProfile :: Prelude.Maybe RoutingProfile,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRoutingProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'routingProfile', 'describeRoutingProfileResponse_routingProfile' - The routing profile.
--
-- 'httpStatus', 'describeRoutingProfileResponse_httpStatus' - The response's http status code.
newDescribeRoutingProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeRoutingProfileResponse
newDescribeRoutingProfileResponse pHttpStatus_ =
  DescribeRoutingProfileResponse'
    { routingProfile =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The routing profile.
describeRoutingProfileResponse_routingProfile :: Lens.Lens' DescribeRoutingProfileResponse (Prelude.Maybe RoutingProfile)
describeRoutingProfileResponse_routingProfile = Lens.lens (\DescribeRoutingProfileResponse' {routingProfile} -> routingProfile) (\s@DescribeRoutingProfileResponse' {} a -> s {routingProfile = a} :: DescribeRoutingProfileResponse)

-- | The response's http status code.
describeRoutingProfileResponse_httpStatus :: Lens.Lens' DescribeRoutingProfileResponse Prelude.Int
describeRoutingProfileResponse_httpStatus = Lens.lens (\DescribeRoutingProfileResponse' {httpStatus} -> httpStatus) (\s@DescribeRoutingProfileResponse' {} a -> s {httpStatus = a} :: DescribeRoutingProfileResponse)

instance
  Prelude.NFData
    DescribeRoutingProfileResponse
  where
  rnf DescribeRoutingProfileResponse' {..} =
    Prelude.rnf routingProfile `Prelude.seq`
      Prelude.rnf httpStatus
