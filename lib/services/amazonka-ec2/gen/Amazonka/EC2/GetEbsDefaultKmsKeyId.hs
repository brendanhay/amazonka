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
-- Module      : Amazonka.EC2.GetEbsDefaultKmsKeyId
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the default KMS key for EBS encryption by default for your
-- account in this Region. You can change the default KMS key for
-- encryption by default using ModifyEbsDefaultKmsKeyId or
-- ResetEbsDefaultKmsKeyId.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS encryption>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Amazonka.EC2.GetEbsDefaultKmsKeyId
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetEbsDefaultKmsKeyId' smart constructor.
data GetEbsDefaultKmsKeyId = GetEbsDefaultKmsKeyId'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest GetEbsDefaultKmsKeyId where
  type
    AWSResponse GetEbsDefaultKmsKeyId =
      GetEbsDefaultKmsKeyIdResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetEbsDefaultKmsKeyIdResponse'
            Prelude.<$> (x Data..@? "kmsKeyId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetEbsDefaultKmsKeyId where
  hashWithSalt _salt GetEbsDefaultKmsKeyId' {..} =
    _salt `Prelude.hashWithSalt` dryRun

instance Prelude.NFData GetEbsDefaultKmsKeyId where
  rnf GetEbsDefaultKmsKeyId' {..} = Prelude.rnf dryRun

instance Data.ToHeaders GetEbsDefaultKmsKeyId where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetEbsDefaultKmsKeyId where
  toPath = Prelude.const "/"

instance Data.ToQuery GetEbsDefaultKmsKeyId where
  toQuery GetEbsDefaultKmsKeyId' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("GetEbsDefaultKmsKeyId" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun
      ]

-- | /See:/ 'newGetEbsDefaultKmsKeyIdResponse' smart constructor.
data GetEbsDefaultKmsKeyIdResponse = GetEbsDefaultKmsKeyIdResponse'
  { -- | The Amazon Resource Name (ARN) of the default KMS key for encryption by
    -- default.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEbsDefaultKmsKeyIdResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'getEbsDefaultKmsKeyIdResponse_kmsKeyId' - The Amazon Resource Name (ARN) of the default KMS key for encryption by
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

-- | The Amazon Resource Name (ARN) of the default KMS key for encryption by
-- default.
getEbsDefaultKmsKeyIdResponse_kmsKeyId :: Lens.Lens' GetEbsDefaultKmsKeyIdResponse (Prelude.Maybe Prelude.Text)
getEbsDefaultKmsKeyIdResponse_kmsKeyId = Lens.lens (\GetEbsDefaultKmsKeyIdResponse' {kmsKeyId} -> kmsKeyId) (\s@GetEbsDefaultKmsKeyIdResponse' {} a -> s {kmsKeyId = a} :: GetEbsDefaultKmsKeyIdResponse)

-- | The response's http status code.
getEbsDefaultKmsKeyIdResponse_httpStatus :: Lens.Lens' GetEbsDefaultKmsKeyIdResponse Prelude.Int
getEbsDefaultKmsKeyIdResponse_httpStatus = Lens.lens (\GetEbsDefaultKmsKeyIdResponse' {httpStatus} -> httpStatus) (\s@GetEbsDefaultKmsKeyIdResponse' {} a -> s {httpStatus = a} :: GetEbsDefaultKmsKeyIdResponse)

instance Prelude.NFData GetEbsDefaultKmsKeyIdResponse where
  rnf GetEbsDefaultKmsKeyIdResponse' {..} =
    Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf httpStatus
