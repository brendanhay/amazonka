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
-- Module      : Network.AWS.EC2.GetEbsEncryptionByDefault
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes whether EBS encryption by default is enabled for your account
-- in the current Region.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS encryption>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Network.AWS.EC2.GetEbsEncryptionByDefault
  ( -- * Creating a Request
    GetEbsEncryptionByDefault (..),
    newGetEbsEncryptionByDefault,

    -- * Request Lenses
    getEbsEncryptionByDefault_dryRun,

    -- * Destructuring the Response
    GetEbsEncryptionByDefaultResponse (..),
    newGetEbsEncryptionByDefaultResponse,

    -- * Response Lenses
    getEbsEncryptionByDefaultResponse_ebsEncryptionByDefault,
    getEbsEncryptionByDefaultResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetEbsEncryptionByDefault' smart constructor.
data GetEbsEncryptionByDefault = GetEbsEncryptionByDefault'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetEbsEncryptionByDefault' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'getEbsEncryptionByDefault_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
newGetEbsEncryptionByDefault ::
  GetEbsEncryptionByDefault
newGetEbsEncryptionByDefault =
  GetEbsEncryptionByDefault' {dryRun = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
getEbsEncryptionByDefault_dryRun :: Lens.Lens' GetEbsEncryptionByDefault (Core.Maybe Core.Bool)
getEbsEncryptionByDefault_dryRun = Lens.lens (\GetEbsEncryptionByDefault' {dryRun} -> dryRun) (\s@GetEbsEncryptionByDefault' {} a -> s {dryRun = a} :: GetEbsEncryptionByDefault)

instance Core.AWSRequest GetEbsEncryptionByDefault where
  type
    AWSResponse GetEbsEncryptionByDefault =
      GetEbsEncryptionByDefaultResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetEbsEncryptionByDefaultResponse'
            Core.<$> (x Core..@? "ebsEncryptionByDefault")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetEbsEncryptionByDefault

instance Core.NFData GetEbsEncryptionByDefault

instance Core.ToHeaders GetEbsEncryptionByDefault where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetEbsEncryptionByDefault where
  toPath = Core.const "/"

instance Core.ToQuery GetEbsEncryptionByDefault where
  toQuery GetEbsEncryptionByDefault' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("GetEbsEncryptionByDefault" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun
      ]

-- | /See:/ 'newGetEbsEncryptionByDefaultResponse' smart constructor.
data GetEbsEncryptionByDefaultResponse = GetEbsEncryptionByDefaultResponse'
  { -- | Indicates whether encryption by default is enabled.
    ebsEncryptionByDefault :: Core.Maybe Core.Bool,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetEbsEncryptionByDefaultResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ebsEncryptionByDefault', 'getEbsEncryptionByDefaultResponse_ebsEncryptionByDefault' - Indicates whether encryption by default is enabled.
--
-- 'httpStatus', 'getEbsEncryptionByDefaultResponse_httpStatus' - The response's http status code.
newGetEbsEncryptionByDefaultResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetEbsEncryptionByDefaultResponse
newGetEbsEncryptionByDefaultResponse pHttpStatus_ =
  GetEbsEncryptionByDefaultResponse'
    { ebsEncryptionByDefault =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Indicates whether encryption by default is enabled.
getEbsEncryptionByDefaultResponse_ebsEncryptionByDefault :: Lens.Lens' GetEbsEncryptionByDefaultResponse (Core.Maybe Core.Bool)
getEbsEncryptionByDefaultResponse_ebsEncryptionByDefault = Lens.lens (\GetEbsEncryptionByDefaultResponse' {ebsEncryptionByDefault} -> ebsEncryptionByDefault) (\s@GetEbsEncryptionByDefaultResponse' {} a -> s {ebsEncryptionByDefault = a} :: GetEbsEncryptionByDefaultResponse)

-- | The response's http status code.
getEbsEncryptionByDefaultResponse_httpStatus :: Lens.Lens' GetEbsEncryptionByDefaultResponse Core.Int
getEbsEncryptionByDefaultResponse_httpStatus = Lens.lens (\GetEbsEncryptionByDefaultResponse' {httpStatus} -> httpStatus) (\s@GetEbsEncryptionByDefaultResponse' {} a -> s {httpStatus = a} :: GetEbsEncryptionByDefaultResponse)

instance
  Core.NFData
    GetEbsEncryptionByDefaultResponse
