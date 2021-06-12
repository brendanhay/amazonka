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
-- Module      : Network.AWS.EKS.DescribeFargateProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns descriptive information about an AWS Fargate profile.
module Network.AWS.EKS.DescribeFargateProfile
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

import qualified Network.AWS.Core as Core
import Network.AWS.EKS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeFargateProfile' smart constructor.
data DescribeFargateProfile = DescribeFargateProfile'
  { -- | The name of the Amazon EKS cluster associated with the Fargate profile.
    clusterName :: Core.Text,
    -- | The name of the Fargate profile to describe.
    fargateProfileName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'fargateProfileName'
  Core.Text ->
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
describeFargateProfile_clusterName :: Lens.Lens' DescribeFargateProfile Core.Text
describeFargateProfile_clusterName = Lens.lens (\DescribeFargateProfile' {clusterName} -> clusterName) (\s@DescribeFargateProfile' {} a -> s {clusterName = a} :: DescribeFargateProfile)

-- | The name of the Fargate profile to describe.
describeFargateProfile_fargateProfileName :: Lens.Lens' DescribeFargateProfile Core.Text
describeFargateProfile_fargateProfileName = Lens.lens (\DescribeFargateProfile' {fargateProfileName} -> fargateProfileName) (\s@DescribeFargateProfile' {} a -> s {fargateProfileName = a} :: DescribeFargateProfile)

instance Core.AWSRequest DescribeFargateProfile where
  type
    AWSResponse DescribeFargateProfile =
      DescribeFargateProfileResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFargateProfileResponse'
            Core.<$> (x Core..?> "fargateProfile")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeFargateProfile

instance Core.NFData DescribeFargateProfile

instance Core.ToHeaders DescribeFargateProfile where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DescribeFargateProfile where
  toPath DescribeFargateProfile' {..} =
    Core.mconcat
      [ "/clusters/",
        Core.toBS clusterName,
        "/fargate-profiles/",
        Core.toBS fargateProfileName
      ]

instance Core.ToQuery DescribeFargateProfile where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeFargateProfileResponse' smart constructor.
data DescribeFargateProfileResponse = DescribeFargateProfileResponse'
  { -- | The full description of your Fargate profile.
    fargateProfile :: Core.Maybe FargateProfile,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeFargateProfileResponse
newDescribeFargateProfileResponse pHttpStatus_ =
  DescribeFargateProfileResponse'
    { fargateProfile =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The full description of your Fargate profile.
describeFargateProfileResponse_fargateProfile :: Lens.Lens' DescribeFargateProfileResponse (Core.Maybe FargateProfile)
describeFargateProfileResponse_fargateProfile = Lens.lens (\DescribeFargateProfileResponse' {fargateProfile} -> fargateProfile) (\s@DescribeFargateProfileResponse' {} a -> s {fargateProfile = a} :: DescribeFargateProfileResponse)

-- | The response's http status code.
describeFargateProfileResponse_httpStatus :: Lens.Lens' DescribeFargateProfileResponse Core.Int
describeFargateProfileResponse_httpStatus = Lens.lens (\DescribeFargateProfileResponse' {httpStatus} -> httpStatus) (\s@DescribeFargateProfileResponse' {} a -> s {httpStatus = a} :: DescribeFargateProfileResponse)

instance Core.NFData DescribeFargateProfileResponse
