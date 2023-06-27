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
-- Module      : Amazonka.IoT.DeleteSecurityProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Device Defender security profile.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DeleteSecurityProfile>
-- action.
module Amazonka.IoT.DeleteSecurityProfile
  ( -- * Creating a Request
    DeleteSecurityProfile (..),
    newDeleteSecurityProfile,

    -- * Request Lenses
    deleteSecurityProfile_expectedVersion,
    deleteSecurityProfile_securityProfileName,

    -- * Destructuring the Response
    DeleteSecurityProfileResponse (..),
    newDeleteSecurityProfileResponse,

    -- * Response Lenses
    deleteSecurityProfileResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteSecurityProfile' smart constructor.
data DeleteSecurityProfile = DeleteSecurityProfile'
  { -- | The expected version of the security profile. A new version is generated
    -- whenever the security profile is updated. If you specify a value that is
    -- different from the actual version, a @VersionConflictException@ is
    -- thrown.
    expectedVersion :: Prelude.Maybe Prelude.Integer,
    -- | The name of the security profile to be deleted.
    securityProfileName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSecurityProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedVersion', 'deleteSecurityProfile_expectedVersion' - The expected version of the security profile. A new version is generated
-- whenever the security profile is updated. If you specify a value that is
-- different from the actual version, a @VersionConflictException@ is
-- thrown.
--
-- 'securityProfileName', 'deleteSecurityProfile_securityProfileName' - The name of the security profile to be deleted.
newDeleteSecurityProfile ::
  -- | 'securityProfileName'
  Prelude.Text ->
  DeleteSecurityProfile
newDeleteSecurityProfile pSecurityProfileName_ =
  DeleteSecurityProfile'
    { expectedVersion =
        Prelude.Nothing,
      securityProfileName = pSecurityProfileName_
    }

-- | The expected version of the security profile. A new version is generated
-- whenever the security profile is updated. If you specify a value that is
-- different from the actual version, a @VersionConflictException@ is
-- thrown.
deleteSecurityProfile_expectedVersion :: Lens.Lens' DeleteSecurityProfile (Prelude.Maybe Prelude.Integer)
deleteSecurityProfile_expectedVersion = Lens.lens (\DeleteSecurityProfile' {expectedVersion} -> expectedVersion) (\s@DeleteSecurityProfile' {} a -> s {expectedVersion = a} :: DeleteSecurityProfile)

-- | The name of the security profile to be deleted.
deleteSecurityProfile_securityProfileName :: Lens.Lens' DeleteSecurityProfile Prelude.Text
deleteSecurityProfile_securityProfileName = Lens.lens (\DeleteSecurityProfile' {securityProfileName} -> securityProfileName) (\s@DeleteSecurityProfile' {} a -> s {securityProfileName = a} :: DeleteSecurityProfile)

instance Core.AWSRequest DeleteSecurityProfile where
  type
    AWSResponse DeleteSecurityProfile =
      DeleteSecurityProfileResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteSecurityProfileResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteSecurityProfile where
  hashWithSalt _salt DeleteSecurityProfile' {..} =
    _salt
      `Prelude.hashWithSalt` expectedVersion
      `Prelude.hashWithSalt` securityProfileName

instance Prelude.NFData DeleteSecurityProfile where
  rnf DeleteSecurityProfile' {..} =
    Prelude.rnf expectedVersion
      `Prelude.seq` Prelude.rnf securityProfileName

instance Data.ToHeaders DeleteSecurityProfile where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteSecurityProfile where
  toPath DeleteSecurityProfile' {..} =
    Prelude.mconcat
      [ "/security-profiles/",
        Data.toBS securityProfileName
      ]

instance Data.ToQuery DeleteSecurityProfile where
  toQuery DeleteSecurityProfile' {..} =
    Prelude.mconcat
      ["expectedVersion" Data.=: expectedVersion]

-- | /See:/ 'newDeleteSecurityProfileResponse' smart constructor.
data DeleteSecurityProfileResponse = DeleteSecurityProfileResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSecurityProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteSecurityProfileResponse_httpStatus' - The response's http status code.
newDeleteSecurityProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteSecurityProfileResponse
newDeleteSecurityProfileResponse pHttpStatus_ =
  DeleteSecurityProfileResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteSecurityProfileResponse_httpStatus :: Lens.Lens' DeleteSecurityProfileResponse Prelude.Int
deleteSecurityProfileResponse_httpStatus = Lens.lens (\DeleteSecurityProfileResponse' {httpStatus} -> httpStatus) (\s@DeleteSecurityProfileResponse' {} a -> s {httpStatus = a} :: DeleteSecurityProfileResponse)

instance Prelude.NFData DeleteSecurityProfileResponse where
  rnf DeleteSecurityProfileResponse' {..} =
    Prelude.rnf httpStatus
