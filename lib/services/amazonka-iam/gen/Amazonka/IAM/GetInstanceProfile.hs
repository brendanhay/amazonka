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
-- Module      : Amazonka.IAM.GetInstanceProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the specified instance profile, including
-- the instance profile\'s path, GUID, ARN, and role. For more information
-- about instance profiles, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html About instance profiles>
-- in the /IAM User Guide/.
module Amazonka.IAM.GetInstanceProfile
  ( -- * Creating a Request
    GetInstanceProfile (..),
    newGetInstanceProfile,

    -- * Request Lenses
    getInstanceProfile_instanceProfileName,

    -- * Destructuring the Response
    GetInstanceProfileResponse (..),
    newGetInstanceProfileResponse,

    -- * Response Lenses
    getInstanceProfileResponse_httpStatus,
    getInstanceProfileResponse_instanceProfile,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetInstanceProfile' smart constructor.
data GetInstanceProfile = GetInstanceProfile'
  { -- | The name of the instance profile to get information about.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    instanceProfileName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInstanceProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceProfileName', 'getInstanceProfile_instanceProfileName' - The name of the instance profile to get information about.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newGetInstanceProfile ::
  -- | 'instanceProfileName'
  Prelude.Text ->
  GetInstanceProfile
newGetInstanceProfile pInstanceProfileName_ =
  GetInstanceProfile'
    { instanceProfileName =
        pInstanceProfileName_
    }

-- | The name of the instance profile to get information about.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
getInstanceProfile_instanceProfileName :: Lens.Lens' GetInstanceProfile Prelude.Text
getInstanceProfile_instanceProfileName = Lens.lens (\GetInstanceProfile' {instanceProfileName} -> instanceProfileName) (\s@GetInstanceProfile' {} a -> s {instanceProfileName = a} :: GetInstanceProfile)

instance Core.AWSRequest GetInstanceProfile where
  type
    AWSResponse GetInstanceProfile =
      GetInstanceProfileResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "GetInstanceProfileResult"
      ( \s h x ->
          GetInstanceProfileResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..@ "InstanceProfile")
      )

instance Prelude.Hashable GetInstanceProfile where
  hashWithSalt _salt GetInstanceProfile' {..} =
    _salt `Prelude.hashWithSalt` instanceProfileName

instance Prelude.NFData GetInstanceProfile where
  rnf GetInstanceProfile' {..} =
    Prelude.rnf instanceProfileName

instance Core.ToHeaders GetInstanceProfile where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetInstanceProfile where
  toPath = Prelude.const "/"

instance Core.ToQuery GetInstanceProfile where
  toQuery GetInstanceProfile' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("GetInstanceProfile" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-05-08" :: Prelude.ByteString),
        "InstanceProfileName" Core.=: instanceProfileName
      ]

-- | Contains the response to a successful GetInstanceProfile request.
--
-- /See:/ 'newGetInstanceProfileResponse' smart constructor.
data GetInstanceProfileResponse = GetInstanceProfileResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A structure containing details about the instance profile.
    instanceProfile :: InstanceProfile
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInstanceProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getInstanceProfileResponse_httpStatus' - The response's http status code.
--
-- 'instanceProfile', 'getInstanceProfileResponse_instanceProfile' - A structure containing details about the instance profile.
newGetInstanceProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'instanceProfile'
  InstanceProfile ->
  GetInstanceProfileResponse
newGetInstanceProfileResponse
  pHttpStatus_
  pInstanceProfile_ =
    GetInstanceProfileResponse'
      { httpStatus =
          pHttpStatus_,
        instanceProfile = pInstanceProfile_
      }

-- | The response's http status code.
getInstanceProfileResponse_httpStatus :: Lens.Lens' GetInstanceProfileResponse Prelude.Int
getInstanceProfileResponse_httpStatus = Lens.lens (\GetInstanceProfileResponse' {httpStatus} -> httpStatus) (\s@GetInstanceProfileResponse' {} a -> s {httpStatus = a} :: GetInstanceProfileResponse)

-- | A structure containing details about the instance profile.
getInstanceProfileResponse_instanceProfile :: Lens.Lens' GetInstanceProfileResponse InstanceProfile
getInstanceProfileResponse_instanceProfile = Lens.lens (\GetInstanceProfileResponse' {instanceProfile} -> instanceProfile) (\s@GetInstanceProfileResponse' {} a -> s {instanceProfile = a} :: GetInstanceProfileResponse)

instance Prelude.NFData GetInstanceProfileResponse where
  rnf GetInstanceProfileResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf instanceProfile
