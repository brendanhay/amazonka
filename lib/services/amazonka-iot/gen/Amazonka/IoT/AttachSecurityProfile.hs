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
-- Module      : Amazonka.IoT.AttachSecurityProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a Device Defender security profile with a thing group or this
-- account. Each thing group or account can have up to five security
-- profiles associated with it.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions AttachSecurityProfile>
-- action.
module Amazonka.IoT.AttachSecurityProfile
  ( -- * Creating a Request
    AttachSecurityProfile (..),
    newAttachSecurityProfile,

    -- * Request Lenses
    attachSecurityProfile_securityProfileName,
    attachSecurityProfile_securityProfileTargetArn,

    -- * Destructuring the Response
    AttachSecurityProfileResponse (..),
    newAttachSecurityProfileResponse,

    -- * Response Lenses
    attachSecurityProfileResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAttachSecurityProfile' smart constructor.
data AttachSecurityProfile = AttachSecurityProfile'
  { -- | The security profile that is attached.
    securityProfileName :: Prelude.Text,
    -- | The ARN of the target (thing group) to which the security profile is
    -- attached.
    securityProfileTargetArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachSecurityProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityProfileName', 'attachSecurityProfile_securityProfileName' - The security profile that is attached.
--
-- 'securityProfileTargetArn', 'attachSecurityProfile_securityProfileTargetArn' - The ARN of the target (thing group) to which the security profile is
-- attached.
newAttachSecurityProfile ::
  -- | 'securityProfileName'
  Prelude.Text ->
  -- | 'securityProfileTargetArn'
  Prelude.Text ->
  AttachSecurityProfile
newAttachSecurityProfile
  pSecurityProfileName_
  pSecurityProfileTargetArn_ =
    AttachSecurityProfile'
      { securityProfileName =
          pSecurityProfileName_,
        securityProfileTargetArn =
          pSecurityProfileTargetArn_
      }

-- | The security profile that is attached.
attachSecurityProfile_securityProfileName :: Lens.Lens' AttachSecurityProfile Prelude.Text
attachSecurityProfile_securityProfileName = Lens.lens (\AttachSecurityProfile' {securityProfileName} -> securityProfileName) (\s@AttachSecurityProfile' {} a -> s {securityProfileName = a} :: AttachSecurityProfile)

-- | The ARN of the target (thing group) to which the security profile is
-- attached.
attachSecurityProfile_securityProfileTargetArn :: Lens.Lens' AttachSecurityProfile Prelude.Text
attachSecurityProfile_securityProfileTargetArn = Lens.lens (\AttachSecurityProfile' {securityProfileTargetArn} -> securityProfileTargetArn) (\s@AttachSecurityProfile' {} a -> s {securityProfileTargetArn = a} :: AttachSecurityProfile)

instance Core.AWSRequest AttachSecurityProfile where
  type
    AWSResponse AttachSecurityProfile =
      AttachSecurityProfileResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AttachSecurityProfileResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AttachSecurityProfile where
  hashWithSalt _salt AttachSecurityProfile' {..} =
    _salt
      `Prelude.hashWithSalt` securityProfileName
      `Prelude.hashWithSalt` securityProfileTargetArn

instance Prelude.NFData AttachSecurityProfile where
  rnf AttachSecurityProfile' {..} =
    Prelude.rnf securityProfileName
      `Prelude.seq` Prelude.rnf securityProfileTargetArn

instance Data.ToHeaders AttachSecurityProfile where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON AttachSecurityProfile where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath AttachSecurityProfile where
  toPath AttachSecurityProfile' {..} =
    Prelude.mconcat
      [ "/security-profiles/",
        Data.toBS securityProfileName,
        "/targets"
      ]

instance Data.ToQuery AttachSecurityProfile where
  toQuery AttachSecurityProfile' {..} =
    Prelude.mconcat
      [ "securityProfileTargetArn"
          Data.=: securityProfileTargetArn
      ]

-- | /See:/ 'newAttachSecurityProfileResponse' smart constructor.
data AttachSecurityProfileResponse = AttachSecurityProfileResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachSecurityProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'attachSecurityProfileResponse_httpStatus' - The response's http status code.
newAttachSecurityProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AttachSecurityProfileResponse
newAttachSecurityProfileResponse pHttpStatus_ =
  AttachSecurityProfileResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
attachSecurityProfileResponse_httpStatus :: Lens.Lens' AttachSecurityProfileResponse Prelude.Int
attachSecurityProfileResponse_httpStatus = Lens.lens (\AttachSecurityProfileResponse' {httpStatus} -> httpStatus) (\s@AttachSecurityProfileResponse' {} a -> s {httpStatus = a} :: AttachSecurityProfileResponse)

instance Prelude.NFData AttachSecurityProfileResponse where
  rnf AttachSecurityProfileResponse' {..} =
    Prelude.rnf httpStatus
