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
-- Module      : Amazonka.Connect.DescribeSecurityProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Gets basic information about the security profle.
module Amazonka.Connect.DescribeSecurityProfile
  ( -- * Creating a Request
    DescribeSecurityProfile (..),
    newDescribeSecurityProfile,

    -- * Request Lenses
    describeSecurityProfile_securityProfileId,
    describeSecurityProfile_instanceId,

    -- * Destructuring the Response
    DescribeSecurityProfileResponse (..),
    newDescribeSecurityProfileResponse,

    -- * Response Lenses
    describeSecurityProfileResponse_securityProfile,
    describeSecurityProfileResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeSecurityProfile' smart constructor.
data DescribeSecurityProfile = DescribeSecurityProfile'
  { -- | The identifier for the security profle.
    securityProfileId :: Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSecurityProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityProfileId', 'describeSecurityProfile_securityProfileId' - The identifier for the security profle.
--
-- 'instanceId', 'describeSecurityProfile_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
newDescribeSecurityProfile ::
  -- | 'securityProfileId'
  Prelude.Text ->
  -- | 'instanceId'
  Prelude.Text ->
  DescribeSecurityProfile
newDescribeSecurityProfile
  pSecurityProfileId_
  pInstanceId_ =
    DescribeSecurityProfile'
      { securityProfileId =
          pSecurityProfileId_,
        instanceId = pInstanceId_
      }

-- | The identifier for the security profle.
describeSecurityProfile_securityProfileId :: Lens.Lens' DescribeSecurityProfile Prelude.Text
describeSecurityProfile_securityProfileId = Lens.lens (\DescribeSecurityProfile' {securityProfileId} -> securityProfileId) (\s@DescribeSecurityProfile' {} a -> s {securityProfileId = a} :: DescribeSecurityProfile)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
describeSecurityProfile_instanceId :: Lens.Lens' DescribeSecurityProfile Prelude.Text
describeSecurityProfile_instanceId = Lens.lens (\DescribeSecurityProfile' {instanceId} -> instanceId) (\s@DescribeSecurityProfile' {} a -> s {instanceId = a} :: DescribeSecurityProfile)

instance Core.AWSRequest DescribeSecurityProfile where
  type
    AWSResponse DescribeSecurityProfile =
      DescribeSecurityProfileResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSecurityProfileResponse'
            Prelude.<$> (x Data..?> "SecurityProfile")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSecurityProfile where
  hashWithSalt _salt DescribeSecurityProfile' {..} =
    _salt `Prelude.hashWithSalt` securityProfileId
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData DescribeSecurityProfile where
  rnf DescribeSecurityProfile' {..} =
    Prelude.rnf securityProfileId
      `Prelude.seq` Prelude.rnf instanceId

instance Data.ToHeaders DescribeSecurityProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeSecurityProfile where
  toPath DescribeSecurityProfile' {..} =
    Prelude.mconcat
      [ "/security-profiles/",
        Data.toBS instanceId,
        "/",
        Data.toBS securityProfileId
      ]

instance Data.ToQuery DescribeSecurityProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeSecurityProfileResponse' smart constructor.
data DescribeSecurityProfileResponse = DescribeSecurityProfileResponse'
  { -- | The security profile.
    securityProfile :: Prelude.Maybe SecurityProfile,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSecurityProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityProfile', 'describeSecurityProfileResponse_securityProfile' - The security profile.
--
-- 'httpStatus', 'describeSecurityProfileResponse_httpStatus' - The response's http status code.
newDescribeSecurityProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSecurityProfileResponse
newDescribeSecurityProfileResponse pHttpStatus_ =
  DescribeSecurityProfileResponse'
    { securityProfile =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The security profile.
describeSecurityProfileResponse_securityProfile :: Lens.Lens' DescribeSecurityProfileResponse (Prelude.Maybe SecurityProfile)
describeSecurityProfileResponse_securityProfile = Lens.lens (\DescribeSecurityProfileResponse' {securityProfile} -> securityProfile) (\s@DescribeSecurityProfileResponse' {} a -> s {securityProfile = a} :: DescribeSecurityProfileResponse)

-- | The response's http status code.
describeSecurityProfileResponse_httpStatus :: Lens.Lens' DescribeSecurityProfileResponse Prelude.Int
describeSecurityProfileResponse_httpStatus = Lens.lens (\DescribeSecurityProfileResponse' {httpStatus} -> httpStatus) (\s@DescribeSecurityProfileResponse' {} a -> s {httpStatus = a} :: DescribeSecurityProfileResponse)

instance
  Prelude.NFData
    DescribeSecurityProfileResponse
  where
  rnf DescribeSecurityProfileResponse' {..} =
    Prelude.rnf securityProfile
      `Prelude.seq` Prelude.rnf httpStatus
