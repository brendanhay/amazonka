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
-- Module      : Amazonka.Connect.DeleteSecurityProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Deletes a security profile.
module Amazonka.Connect.DeleteSecurityProfile
  ( -- * Creating a Request
    DeleteSecurityProfile (..),
    newDeleteSecurityProfile,

    -- * Request Lenses
    deleteSecurityProfile_instanceId,
    deleteSecurityProfile_securityProfileId,

    -- * Destructuring the Response
    DeleteSecurityProfileResponse (..),
    newDeleteSecurityProfileResponse,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteSecurityProfile' smart constructor.
data DeleteSecurityProfile = DeleteSecurityProfile'
  { -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier for the security profle.
    securityProfileId :: Prelude.Text
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
-- 'instanceId', 'deleteSecurityProfile_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'securityProfileId', 'deleteSecurityProfile_securityProfileId' - The identifier for the security profle.
newDeleteSecurityProfile ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'securityProfileId'
  Prelude.Text ->
  DeleteSecurityProfile
newDeleteSecurityProfile
  pInstanceId_
  pSecurityProfileId_ =
    DeleteSecurityProfile'
      { instanceId = pInstanceId_,
        securityProfileId = pSecurityProfileId_
      }

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
deleteSecurityProfile_instanceId :: Lens.Lens' DeleteSecurityProfile Prelude.Text
deleteSecurityProfile_instanceId = Lens.lens (\DeleteSecurityProfile' {instanceId} -> instanceId) (\s@DeleteSecurityProfile' {} a -> s {instanceId = a} :: DeleteSecurityProfile)

-- | The identifier for the security profle.
deleteSecurityProfile_securityProfileId :: Lens.Lens' DeleteSecurityProfile Prelude.Text
deleteSecurityProfile_securityProfileId = Lens.lens (\DeleteSecurityProfile' {securityProfileId} -> securityProfileId) (\s@DeleteSecurityProfile' {} a -> s {securityProfileId = a} :: DeleteSecurityProfile)

instance Core.AWSRequest DeleteSecurityProfile where
  type
    AWSResponse DeleteSecurityProfile =
      DeleteSecurityProfileResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteSecurityProfileResponse'

instance Prelude.Hashable DeleteSecurityProfile where
  hashWithSalt _salt DeleteSecurityProfile' {..} =
    _salt `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` securityProfileId

instance Prelude.NFData DeleteSecurityProfile where
  rnf DeleteSecurityProfile' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf securityProfileId

instance Data.ToHeaders DeleteSecurityProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteSecurityProfile where
  toPath DeleteSecurityProfile' {..} =
    Prelude.mconcat
      [ "/security-profiles/",
        Data.toBS instanceId,
        "/",
        Data.toBS securityProfileId
      ]

instance Data.ToQuery DeleteSecurityProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSecurityProfileResponse' smart constructor.
data DeleteSecurityProfileResponse = DeleteSecurityProfileResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSecurityProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteSecurityProfileResponse ::
  DeleteSecurityProfileResponse
newDeleteSecurityProfileResponse =
  DeleteSecurityProfileResponse'

instance Prelude.NFData DeleteSecurityProfileResponse where
  rnf _ = ()
