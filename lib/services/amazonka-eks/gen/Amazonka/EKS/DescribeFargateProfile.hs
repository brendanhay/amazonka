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
-- Module      : Amazonka.EKS.DescribeFargateProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns descriptive information about an Fargate profile.
module Amazonka.EKS.DescribeFargateProfile
  ( -- * Creating a Request
    DescribeFargateProfile (..),
    newDescribeFargateProfile,

    -- * Request Lenses
    describeFargateProfile_clusterName,
    describeFargateProfile_fargateProfileName,

    -- * Destructuring the Response
    DescribeFargateProfileResponse (..),
    newDescribeFargateProfileResponse,

    -- * Response Lenses
    describeFargateProfileResponse_fargateProfile,
    describeFargateProfileResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EKS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeFargateProfile' smart constructor.
data DescribeFargateProfile = DescribeFargateProfile'
  { -- | The name of the Amazon EKS cluster associated with the Fargate profile.
    clusterName :: Prelude.Text,
    -- | The name of the Fargate profile to describe.
    fargateProfileName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFargateProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterName', 'describeFargateProfile_clusterName' - The name of the Amazon EKS cluster associated with the Fargate profile.
--
-- 'fargateProfileName', 'describeFargateProfile_fargateProfileName' - The name of the Fargate profile to describe.
newDescribeFargateProfile ::
  -- | 'clusterName'
  Prelude.Text ->
  -- | 'fargateProfileName'
  Prelude.Text ->
  DescribeFargateProfile
newDescribeFargateProfile
  pClusterName_
  pFargateProfileName_ =
    DescribeFargateProfile'
      { clusterName =
          pClusterName_,
        fargateProfileName = pFargateProfileName_
      }

-- | The name of the Amazon EKS cluster associated with the Fargate profile.
describeFargateProfile_clusterName :: Lens.Lens' DescribeFargateProfile Prelude.Text
describeFargateProfile_clusterName = Lens.lens (\DescribeFargateProfile' {clusterName} -> clusterName) (\s@DescribeFargateProfile' {} a -> s {clusterName = a} :: DescribeFargateProfile)

-- | The name of the Fargate profile to describe.
describeFargateProfile_fargateProfileName :: Lens.Lens' DescribeFargateProfile Prelude.Text
describeFargateProfile_fargateProfileName = Lens.lens (\DescribeFargateProfile' {fargateProfileName} -> fargateProfileName) (\s@DescribeFargateProfile' {} a -> s {fargateProfileName = a} :: DescribeFargateProfile)

instance Core.AWSRequest DescribeFargateProfile where
  type
    AWSResponse DescribeFargateProfile =
      DescribeFargateProfileResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFargateProfileResponse'
            Prelude.<$> (x Data..?> "fargateProfile")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeFargateProfile where
  hashWithSalt _salt DescribeFargateProfile' {..} =
    _salt
      `Prelude.hashWithSalt` clusterName
      `Prelude.hashWithSalt` fargateProfileName

instance Prelude.NFData DescribeFargateProfile where
  rnf DescribeFargateProfile' {..} =
    Prelude.rnf clusterName
      `Prelude.seq` Prelude.rnf fargateProfileName

instance Data.ToHeaders DescribeFargateProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeFargateProfile where
  toPath DescribeFargateProfile' {..} =
    Prelude.mconcat
      [ "/clusters/",
        Data.toBS clusterName,
        "/fargate-profiles/",
        Data.toBS fargateProfileName
      ]

instance Data.ToQuery DescribeFargateProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeFargateProfileResponse' smart constructor.
data DescribeFargateProfileResponse = DescribeFargateProfileResponse'
  { -- | The full description of your Fargate profile.
    fargateProfile :: Prelude.Maybe FargateProfile,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFargateProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fargateProfile', 'describeFargateProfileResponse_fargateProfile' - The full description of your Fargate profile.
--
-- 'httpStatus', 'describeFargateProfileResponse_httpStatus' - The response's http status code.
newDescribeFargateProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeFargateProfileResponse
newDescribeFargateProfileResponse pHttpStatus_ =
  DescribeFargateProfileResponse'
    { fargateProfile =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The full description of your Fargate profile.
describeFargateProfileResponse_fargateProfile :: Lens.Lens' DescribeFargateProfileResponse (Prelude.Maybe FargateProfile)
describeFargateProfileResponse_fargateProfile = Lens.lens (\DescribeFargateProfileResponse' {fargateProfile} -> fargateProfile) (\s@DescribeFargateProfileResponse' {} a -> s {fargateProfile = a} :: DescribeFargateProfileResponse)

-- | The response's http status code.
describeFargateProfileResponse_httpStatus :: Lens.Lens' DescribeFargateProfileResponse Prelude.Int
describeFargateProfileResponse_httpStatus = Lens.lens (\DescribeFargateProfileResponse' {httpStatus} -> httpStatus) (\s@DescribeFargateProfileResponse' {} a -> s {httpStatus = a} :: DescribeFargateProfileResponse)

instance
  Prelude.NFData
    DescribeFargateProfileResponse
  where
  rnf DescribeFargateProfileResponse' {..} =
    Prelude.rnf fargateProfile
      `Prelude.seq` Prelude.rnf httpStatus
