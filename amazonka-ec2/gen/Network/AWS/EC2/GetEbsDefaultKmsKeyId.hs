{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.GetEbsDefaultKmsKeyId
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the default customer master key (CMK) for EBS encryption by
-- default for your account in this Region. You can change the default CMK
-- for encryption by default using ModifyEbsDefaultKmsKeyId or
-- ResetEbsDefaultKmsKeyId.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS encryption>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Network.AWS.EC2.GetEbsDefaultKmsKeyId
  ( -- * Creating a Request
    GetEbsDefaultKmsKeyId (..),
    newGetEbsDefaultKmsKeyId,

    -- * Request Lenses
    getEbsDefaultKmsKeyId_dryRun,

    -- * Destructuring the Response
    GetEbsDefaultKmsKeyIdResponse (..),
    newGetEbsDefaultKmsKeyIdResponse,

    -- * Response Lenses
    getEbsDefaultKmsKeyIdResponse_kmsKeyId,
    getEbsDefaultKmsKeyIdResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetEbsDefaultKmsKeyId' smart constructor.
data GetEbsDefaultKmsKeyId = GetEbsDefaultKmsKeyId'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetEbsDefaultKmsKeyId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'getEbsDefaultKmsKeyId_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
newGetEbsDefaultKmsKeyId ::
  GetEbsDefaultKmsKeyId
newGetEbsDefaultKmsKeyId =
  GetEbsDefaultKmsKeyId' {dryRun = Prelude.Nothing}

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
getEbsDefaultKmsKeyId_dryRun :: Lens.Lens' GetEbsDefaultKmsKeyId (Prelude.Maybe Prelude.Bool)
getEbsDefaultKmsKeyId_dryRun = Lens.lens (\GetEbsDefaultKmsKeyId' {dryRun} -> dryRun) (\s@GetEbsDefaultKmsKeyId' {} a -> s {dryRun = a} :: GetEbsDefaultKmsKeyId)

instance Prelude.AWSRequest GetEbsDefaultKmsKeyId where
  type
    Rs GetEbsDefaultKmsKeyId =
      GetEbsDefaultKmsKeyIdResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetEbsDefaultKmsKeyIdResponse'
            Prelude.<$> (x Prelude..@? "kmsKeyId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetEbsDefaultKmsKeyId

instance Prelude.NFData GetEbsDefaultKmsKeyId

instance Prelude.ToHeaders GetEbsDefaultKmsKeyId where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath GetEbsDefaultKmsKeyId where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetEbsDefaultKmsKeyId where
  toQuery GetEbsDefaultKmsKeyId' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("GetEbsDefaultKmsKeyId" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun
      ]

-- | /See:/ 'newGetEbsDefaultKmsKeyIdResponse' smart constructor.
data GetEbsDefaultKmsKeyIdResponse = GetEbsDefaultKmsKeyIdResponse'
  { -- | The Amazon Resource Name (ARN) of the default CMK for encryption by
    -- default.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetEbsDefaultKmsKeyIdResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'getEbsDefaultKmsKeyIdResponse_kmsKeyId' - The Amazon Resource Name (ARN) of the default CMK for encryption by
-- default.
--
-- 'httpStatus', 'getEbsDefaultKmsKeyIdResponse_httpStatus' - The response's http status code.
newGetEbsDefaultKmsKeyIdResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetEbsDefaultKmsKeyIdResponse
newGetEbsDefaultKmsKeyIdResponse pHttpStatus_ =
  GetEbsDefaultKmsKeyIdResponse'
    { kmsKeyId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the default CMK for encryption by
-- default.
getEbsDefaultKmsKeyIdResponse_kmsKeyId :: Lens.Lens' GetEbsDefaultKmsKeyIdResponse (Prelude.Maybe Prelude.Text)
getEbsDefaultKmsKeyIdResponse_kmsKeyId = Lens.lens (\GetEbsDefaultKmsKeyIdResponse' {kmsKeyId} -> kmsKeyId) (\s@GetEbsDefaultKmsKeyIdResponse' {} a -> s {kmsKeyId = a} :: GetEbsDefaultKmsKeyIdResponse)

-- | The response's http status code.
getEbsDefaultKmsKeyIdResponse_httpStatus :: Lens.Lens' GetEbsDefaultKmsKeyIdResponse Prelude.Int
getEbsDefaultKmsKeyIdResponse_httpStatus = Lens.lens (\GetEbsDefaultKmsKeyIdResponse' {httpStatus} -> httpStatus) (\s@GetEbsDefaultKmsKeyIdResponse' {} a -> s {httpStatus = a} :: GetEbsDefaultKmsKeyIdResponse)

instance Prelude.NFData GetEbsDefaultKmsKeyIdResponse
