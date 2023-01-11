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
-- Module      : Amazonka.EC2.ResetEbsDefaultKmsKeyId
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets the default KMS key for EBS encryption for your account in this
-- Region to the Amazon Web Services managed KMS key for EBS.
--
-- After resetting the default KMS key to the Amazon Web Services managed
-- KMS key, you can continue to encrypt by a customer managed KMS key by
-- specifying it when you create the volume. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS encryption>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Amazonka.EC2.ResetEbsDefaultKmsKeyId
  ( -- * Creating a Request
    ResetEbsDefaultKmsKeyId (..),
    newResetEbsDefaultKmsKeyId,

    -- * Request Lenses
    resetEbsDefaultKmsKeyId_dryRun,

    -- * Destructuring the Response
    ResetEbsDefaultKmsKeyIdResponse (..),
    newResetEbsDefaultKmsKeyIdResponse,

    -- * Response Lenses
    resetEbsDefaultKmsKeyIdResponse_kmsKeyId,
    resetEbsDefaultKmsKeyIdResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newResetEbsDefaultKmsKeyId' smart constructor.
data ResetEbsDefaultKmsKeyId = ResetEbsDefaultKmsKeyId'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResetEbsDefaultKmsKeyId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'resetEbsDefaultKmsKeyId_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
newResetEbsDefaultKmsKeyId ::
  ResetEbsDefaultKmsKeyId
newResetEbsDefaultKmsKeyId =
  ResetEbsDefaultKmsKeyId' {dryRun = Prelude.Nothing}

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
resetEbsDefaultKmsKeyId_dryRun :: Lens.Lens' ResetEbsDefaultKmsKeyId (Prelude.Maybe Prelude.Bool)
resetEbsDefaultKmsKeyId_dryRun = Lens.lens (\ResetEbsDefaultKmsKeyId' {dryRun} -> dryRun) (\s@ResetEbsDefaultKmsKeyId' {} a -> s {dryRun = a} :: ResetEbsDefaultKmsKeyId)

instance Core.AWSRequest ResetEbsDefaultKmsKeyId where
  type
    AWSResponse ResetEbsDefaultKmsKeyId =
      ResetEbsDefaultKmsKeyIdResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ResetEbsDefaultKmsKeyIdResponse'
            Prelude.<$> (x Data..@? "kmsKeyId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ResetEbsDefaultKmsKeyId where
  hashWithSalt _salt ResetEbsDefaultKmsKeyId' {..} =
    _salt `Prelude.hashWithSalt` dryRun

instance Prelude.NFData ResetEbsDefaultKmsKeyId where
  rnf ResetEbsDefaultKmsKeyId' {..} = Prelude.rnf dryRun

instance Data.ToHeaders ResetEbsDefaultKmsKeyId where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ResetEbsDefaultKmsKeyId where
  toPath = Prelude.const "/"

instance Data.ToQuery ResetEbsDefaultKmsKeyId where
  toQuery ResetEbsDefaultKmsKeyId' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ResetEbsDefaultKmsKeyId" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun
      ]

-- | /See:/ 'newResetEbsDefaultKmsKeyIdResponse' smart constructor.
data ResetEbsDefaultKmsKeyIdResponse = ResetEbsDefaultKmsKeyIdResponse'
  { -- | The Amazon Resource Name (ARN) of the default KMS key for EBS encryption
    -- by default.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResetEbsDefaultKmsKeyIdResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'resetEbsDefaultKmsKeyIdResponse_kmsKeyId' - The Amazon Resource Name (ARN) of the default KMS key for EBS encryption
-- by default.
--
-- 'httpStatus', 'resetEbsDefaultKmsKeyIdResponse_httpStatus' - The response's http status code.
newResetEbsDefaultKmsKeyIdResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ResetEbsDefaultKmsKeyIdResponse
newResetEbsDefaultKmsKeyIdResponse pHttpStatus_ =
  ResetEbsDefaultKmsKeyIdResponse'
    { kmsKeyId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the default KMS key for EBS encryption
-- by default.
resetEbsDefaultKmsKeyIdResponse_kmsKeyId :: Lens.Lens' ResetEbsDefaultKmsKeyIdResponse (Prelude.Maybe Prelude.Text)
resetEbsDefaultKmsKeyIdResponse_kmsKeyId = Lens.lens (\ResetEbsDefaultKmsKeyIdResponse' {kmsKeyId} -> kmsKeyId) (\s@ResetEbsDefaultKmsKeyIdResponse' {} a -> s {kmsKeyId = a} :: ResetEbsDefaultKmsKeyIdResponse)

-- | The response's http status code.
resetEbsDefaultKmsKeyIdResponse_httpStatus :: Lens.Lens' ResetEbsDefaultKmsKeyIdResponse Prelude.Int
resetEbsDefaultKmsKeyIdResponse_httpStatus = Lens.lens (\ResetEbsDefaultKmsKeyIdResponse' {httpStatus} -> httpStatus) (\s@ResetEbsDefaultKmsKeyIdResponse' {} a -> s {httpStatus = a} :: ResetEbsDefaultKmsKeyIdResponse)

instance
  Prelude.NFData
    ResetEbsDefaultKmsKeyIdResponse
  where
  rnf ResetEbsDefaultKmsKeyIdResponse' {..} =
    Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf httpStatus
