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
-- Module      : Network.AWS.EC2.ResetEbsDefaultKmsKeyId
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets the default customer master key (CMK) for EBS encryption for your
-- account in this Region to the AWS managed CMK for EBS.
--
-- After resetting the default CMK to the AWS managed CMK, you can continue
-- to encrypt by a customer managed CMK by specifying it when you create
-- the volume. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS encryption>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Network.AWS.EC2.ResetEbsDefaultKmsKeyId
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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newResetEbsDefaultKmsKeyId' smart constructor.
data ResetEbsDefaultKmsKeyId = ResetEbsDefaultKmsKeyId'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest ResetEbsDefaultKmsKeyId where
  type
    Rs ResetEbsDefaultKmsKeyId =
      ResetEbsDefaultKmsKeyIdResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ResetEbsDefaultKmsKeyIdResponse'
            Prelude.<$> (x Prelude..@? "kmsKeyId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ResetEbsDefaultKmsKeyId

instance Prelude.NFData ResetEbsDefaultKmsKeyId

instance Prelude.ToHeaders ResetEbsDefaultKmsKeyId where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ResetEbsDefaultKmsKeyId where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ResetEbsDefaultKmsKeyId where
  toQuery ResetEbsDefaultKmsKeyId' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("ResetEbsDefaultKmsKeyId" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun
      ]

-- | /See:/ 'newResetEbsDefaultKmsKeyIdResponse' smart constructor.
data ResetEbsDefaultKmsKeyIdResponse = ResetEbsDefaultKmsKeyIdResponse'
  { -- | The Amazon Resource Name (ARN) of the default CMK for EBS encryption by
    -- default.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResetEbsDefaultKmsKeyIdResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'resetEbsDefaultKmsKeyIdResponse_kmsKeyId' - The Amazon Resource Name (ARN) of the default CMK for EBS encryption by
-- default.
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

-- | The Amazon Resource Name (ARN) of the default CMK for EBS encryption by
-- default.
resetEbsDefaultKmsKeyIdResponse_kmsKeyId :: Lens.Lens' ResetEbsDefaultKmsKeyIdResponse (Prelude.Maybe Prelude.Text)
resetEbsDefaultKmsKeyIdResponse_kmsKeyId = Lens.lens (\ResetEbsDefaultKmsKeyIdResponse' {kmsKeyId} -> kmsKeyId) (\s@ResetEbsDefaultKmsKeyIdResponse' {} a -> s {kmsKeyId = a} :: ResetEbsDefaultKmsKeyIdResponse)

-- | The response's http status code.
resetEbsDefaultKmsKeyIdResponse_httpStatus :: Lens.Lens' ResetEbsDefaultKmsKeyIdResponse Prelude.Int
resetEbsDefaultKmsKeyIdResponse_httpStatus = Lens.lens (\ResetEbsDefaultKmsKeyIdResponse' {httpStatus} -> httpStatus) (\s@ResetEbsDefaultKmsKeyIdResponse' {} a -> s {httpStatus = a} :: ResetEbsDefaultKmsKeyIdResponse)

instance
  Prelude.NFData
    ResetEbsDefaultKmsKeyIdResponse
