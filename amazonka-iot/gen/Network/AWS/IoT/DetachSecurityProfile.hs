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
-- Module      : Network.AWS.IoT.DetachSecurityProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a Device Defender security profile from a thing group or
-- from this account.
module Network.AWS.IoT.DetachSecurityProfile
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

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDetachSecurityProfile' smart constructor.
data DetachSecurityProfile = DetachSecurityProfile'
  { -- | The security profile that is detached.
    securityProfileName :: Core.Text,
    -- | The ARN of the thing group from which the security profile is detached.
    securityProfileTargetArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'securityProfileTargetArn'
  Core.Text ->
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
detachSecurityProfile_securityProfileName :: Lens.Lens' DetachSecurityProfile Core.Text
detachSecurityProfile_securityProfileName = Lens.lens (\DetachSecurityProfile' {securityProfileName} -> securityProfileName) (\s@DetachSecurityProfile' {} a -> s {securityProfileName = a} :: DetachSecurityProfile)

-- | The ARN of the thing group from which the security profile is detached.
detachSecurityProfile_securityProfileTargetArn :: Lens.Lens' DetachSecurityProfile Core.Text
detachSecurityProfile_securityProfileTargetArn = Lens.lens (\DetachSecurityProfile' {securityProfileTargetArn} -> securityProfileTargetArn) (\s@DetachSecurityProfile' {} a -> s {securityProfileTargetArn = a} :: DetachSecurityProfile)

instance Core.AWSRequest DetachSecurityProfile where
  type
    AWSResponse DetachSecurityProfile =
      DetachSecurityProfileResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DetachSecurityProfileResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DetachSecurityProfile

instance Core.NFData DetachSecurityProfile

instance Core.ToHeaders DetachSecurityProfile where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DetachSecurityProfile where
  toPath DetachSecurityProfile' {..} =
    Core.mconcat
      [ "/security-profiles/",
        Core.toBS securityProfileName,
        "/targets"
      ]

instance Core.ToQuery DetachSecurityProfile where
  toQuery DetachSecurityProfile' {..} =
    Core.mconcat
      [ "securityProfileTargetArn"
          Core.=: securityProfileTargetArn
      ]

-- | /See:/ 'newDetachSecurityProfileResponse' smart constructor.
data DetachSecurityProfileResponse = DetachSecurityProfileResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DetachSecurityProfileResponse
newDetachSecurityProfileResponse pHttpStatus_ =
  DetachSecurityProfileResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
detachSecurityProfileResponse_httpStatus :: Lens.Lens' DetachSecurityProfileResponse Core.Int
detachSecurityProfileResponse_httpStatus = Lens.lens (\DetachSecurityProfileResponse' {httpStatus} -> httpStatus) (\s@DetachSecurityProfileResponse' {} a -> s {httpStatus = a} :: DetachSecurityProfileResponse)

instance Core.NFData DetachSecurityProfileResponse
