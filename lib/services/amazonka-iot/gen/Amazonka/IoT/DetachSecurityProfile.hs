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
-- Module      : Amazonka.IoT.DetachSecurityProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a Device Defender security profile from a thing group or
-- from this account.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DetachSecurityProfile>
-- action.
module Amazonka.IoT.DetachSecurityProfile
  ( -- * Creating a Request
    DetachSecurityProfile (..),
    newDetachSecurityProfile,

    -- * Request Lenses
    detachSecurityProfile_securityProfileName,
    detachSecurityProfile_securityProfileTargetArn,

    -- * Destructuring the Response
    DetachSecurityProfileResponse (..),
    newDetachSecurityProfileResponse,

    -- * Response Lenses
    detachSecurityProfileResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDetachSecurityProfile' smart constructor.
data DetachSecurityProfile = DetachSecurityProfile'
  { -- | The security profile that is detached.
    securityProfileName :: Prelude.Text,
    -- | The ARN of the thing group from which the security profile is detached.
    securityProfileTargetArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetachSecurityProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityProfileName', 'detachSecurityProfile_securityProfileName' - The security profile that is detached.
--
-- 'securityProfileTargetArn', 'detachSecurityProfile_securityProfileTargetArn' - The ARN of the thing group from which the security profile is detached.
newDetachSecurityProfile ::
  -- | 'securityProfileName'
  Prelude.Text ->
  -- | 'securityProfileTargetArn'
  Prelude.Text ->
  DetachSecurityProfile
newDetachSecurityProfile
  pSecurityProfileName_
  pSecurityProfileTargetArn_ =
    DetachSecurityProfile'
      { securityProfileName =
          pSecurityProfileName_,
        securityProfileTargetArn =
          pSecurityProfileTargetArn_
      }

-- | The security profile that is detached.
detachSecurityProfile_securityProfileName :: Lens.Lens' DetachSecurityProfile Prelude.Text
detachSecurityProfile_securityProfileName = Lens.lens (\DetachSecurityProfile' {securityProfileName} -> securityProfileName) (\s@DetachSecurityProfile' {} a -> s {securityProfileName = a} :: DetachSecurityProfile)

-- | The ARN of the thing group from which the security profile is detached.
detachSecurityProfile_securityProfileTargetArn :: Lens.Lens' DetachSecurityProfile Prelude.Text
detachSecurityProfile_securityProfileTargetArn = Lens.lens (\DetachSecurityProfile' {securityProfileTargetArn} -> securityProfileTargetArn) (\s@DetachSecurityProfile' {} a -> s {securityProfileTargetArn = a} :: DetachSecurityProfile)

instance Core.AWSRequest DetachSecurityProfile where
  type
    AWSResponse DetachSecurityProfile =
      DetachSecurityProfileResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DetachSecurityProfileResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DetachSecurityProfile where
  hashWithSalt _salt DetachSecurityProfile' {..} =
    _salt `Prelude.hashWithSalt` securityProfileName
      `Prelude.hashWithSalt` securityProfileTargetArn

instance Prelude.NFData DetachSecurityProfile where
  rnf DetachSecurityProfile' {..} =
    Prelude.rnf securityProfileName
      `Prelude.seq` Prelude.rnf securityProfileTargetArn

instance Data.ToHeaders DetachSecurityProfile where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DetachSecurityProfile where
  toPath DetachSecurityProfile' {..} =
    Prelude.mconcat
      [ "/security-profiles/",
        Data.toBS securityProfileName,
        "/targets"
      ]

instance Data.ToQuery DetachSecurityProfile where
  toQuery DetachSecurityProfile' {..} =
    Prelude.mconcat
      [ "securityProfileTargetArn"
          Data.=: securityProfileTargetArn
      ]

-- | /See:/ 'newDetachSecurityProfileResponse' smart constructor.
data DetachSecurityProfileResponse = DetachSecurityProfileResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetachSecurityProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'detachSecurityProfileResponse_httpStatus' - The response's http status code.
newDetachSecurityProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DetachSecurityProfileResponse
newDetachSecurityProfileResponse pHttpStatus_ =
  DetachSecurityProfileResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
detachSecurityProfileResponse_httpStatus :: Lens.Lens' DetachSecurityProfileResponse Prelude.Int
detachSecurityProfileResponse_httpStatus = Lens.lens (\DetachSecurityProfileResponse' {httpStatus} -> httpStatus) (\s@DetachSecurityProfileResponse' {} a -> s {httpStatus = a} :: DetachSecurityProfileResponse)

instance Prelude.NFData DetachSecurityProfileResponse where
  rnf DetachSecurityProfileResponse' {..} =
    Prelude.rnf httpStatus
