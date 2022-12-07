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
-- Module      : Amazonka.EKS.DeleteFargateProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Fargate profile.
--
-- When you delete a Fargate profile, any pods running on Fargate that were
-- created with the profile are deleted. If those pods match another
-- Fargate profile, then they are scheduled on Fargate with that profile.
-- If they no longer match any Fargate profiles, then they are not
-- scheduled on Fargate and they may remain in a pending state.
--
-- Only one Fargate profile in a cluster can be in the @DELETING@ status at
-- a time. You must wait for a Fargate profile to finish deleting before
-- you can delete any other profiles in that cluster.
module Amazonka.EKS.DeleteFargateProfile
  ( -- * Creating a Request
    DeleteFargateProfile (..),
    newDeleteFargateProfile,

    -- * Request Lenses
    deleteFargateProfile_clusterName,
    deleteFargateProfile_fargateProfileName,

    -- * Destructuring the Response
    DeleteFargateProfileResponse (..),
    newDeleteFargateProfileResponse,

    -- * Response Lenses
    deleteFargateProfileResponse_fargateProfile,
    deleteFargateProfileResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EKS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteFargateProfile' smart constructor.
data DeleteFargateProfile = DeleteFargateProfile'
  { -- | The name of the Amazon EKS cluster associated with the Fargate profile
    -- to delete.
    clusterName :: Prelude.Text,
    -- | The name of the Fargate profile to delete.
    fargateProfileName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFargateProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterName', 'deleteFargateProfile_clusterName' - The name of the Amazon EKS cluster associated with the Fargate profile
-- to delete.
--
-- 'fargateProfileName', 'deleteFargateProfile_fargateProfileName' - The name of the Fargate profile to delete.
newDeleteFargateProfile ::
  -- | 'clusterName'
  Prelude.Text ->
  -- | 'fargateProfileName'
  Prelude.Text ->
  DeleteFargateProfile
newDeleteFargateProfile
  pClusterName_
  pFargateProfileName_ =
    DeleteFargateProfile'
      { clusterName = pClusterName_,
        fargateProfileName = pFargateProfileName_
      }

-- | The name of the Amazon EKS cluster associated with the Fargate profile
-- to delete.
deleteFargateProfile_clusterName :: Lens.Lens' DeleteFargateProfile Prelude.Text
deleteFargateProfile_clusterName = Lens.lens (\DeleteFargateProfile' {clusterName} -> clusterName) (\s@DeleteFargateProfile' {} a -> s {clusterName = a} :: DeleteFargateProfile)

-- | The name of the Fargate profile to delete.
deleteFargateProfile_fargateProfileName :: Lens.Lens' DeleteFargateProfile Prelude.Text
deleteFargateProfile_fargateProfileName = Lens.lens (\DeleteFargateProfile' {fargateProfileName} -> fargateProfileName) (\s@DeleteFargateProfile' {} a -> s {fargateProfileName = a} :: DeleteFargateProfile)

instance Core.AWSRequest DeleteFargateProfile where
  type
    AWSResponse DeleteFargateProfile =
      DeleteFargateProfileResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteFargateProfileResponse'
            Prelude.<$> (x Data..?> "fargateProfile")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteFargateProfile where
  hashWithSalt _salt DeleteFargateProfile' {..} =
    _salt `Prelude.hashWithSalt` clusterName
      `Prelude.hashWithSalt` fargateProfileName

instance Prelude.NFData DeleteFargateProfile where
  rnf DeleteFargateProfile' {..} =
    Prelude.rnf clusterName
      `Prelude.seq` Prelude.rnf fargateProfileName

instance Data.ToHeaders DeleteFargateProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteFargateProfile where
  toPath DeleteFargateProfile' {..} =
    Prelude.mconcat
      [ "/clusters/",
        Data.toBS clusterName,
        "/fargate-profiles/",
        Data.toBS fargateProfileName
      ]

instance Data.ToQuery DeleteFargateProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFargateProfileResponse' smart constructor.
data DeleteFargateProfileResponse = DeleteFargateProfileResponse'
  { -- | The deleted Fargate profile.
    fargateProfile :: Prelude.Maybe FargateProfile,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFargateProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fargateProfile', 'deleteFargateProfileResponse_fargateProfile' - The deleted Fargate profile.
--
-- 'httpStatus', 'deleteFargateProfileResponse_httpStatus' - The response's http status code.
newDeleteFargateProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteFargateProfileResponse
newDeleteFargateProfileResponse pHttpStatus_ =
  DeleteFargateProfileResponse'
    { fargateProfile =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The deleted Fargate profile.
deleteFargateProfileResponse_fargateProfile :: Lens.Lens' DeleteFargateProfileResponse (Prelude.Maybe FargateProfile)
deleteFargateProfileResponse_fargateProfile = Lens.lens (\DeleteFargateProfileResponse' {fargateProfile} -> fargateProfile) (\s@DeleteFargateProfileResponse' {} a -> s {fargateProfile = a} :: DeleteFargateProfileResponse)

-- | The response's http status code.
deleteFargateProfileResponse_httpStatus :: Lens.Lens' DeleteFargateProfileResponse Prelude.Int
deleteFargateProfileResponse_httpStatus = Lens.lens (\DeleteFargateProfileResponse' {httpStatus} -> httpStatus) (\s@DeleteFargateProfileResponse' {} a -> s {httpStatus = a} :: DeleteFargateProfileResponse)

instance Prelude.NFData DeleteFargateProfileResponse where
  rnf DeleteFargateProfileResponse' {..} =
    Prelude.rnf fargateProfile
      `Prelude.seq` Prelude.rnf httpStatus
