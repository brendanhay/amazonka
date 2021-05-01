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
-- Module      : Network.AWS.EC2.EnableEbsEncryptionByDefault
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables EBS encryption by default for your account in the current
-- Region.
--
-- After you enable encryption by default, the EBS volumes that you create
-- are are always encrypted, either using the default CMK or the CMK that
-- you specified when you created each volume. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS encryption>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- You can specify the default CMK for encryption by default using
-- ModifyEbsDefaultKmsKeyId or ResetEbsDefaultKmsKeyId.
--
-- Enabling encryption by default has no effect on the encryption status of
-- your existing volumes.
--
-- After you enable encryption by default, you can no longer launch
-- instances using instance types that do not support encryption. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#EBSEncryption_supported_instances Supported instance types>.
module Network.AWS.EC2.EnableEbsEncryptionByDefault
  ( -- * Creating a Request
    EnableEbsEncryptionByDefault (..),
    newEnableEbsEncryptionByDefault,

    -- * Request Lenses
    enableEbsEncryptionByDefault_dryRun,

    -- * Destructuring the Response
    EnableEbsEncryptionByDefaultResponse (..),
    newEnableEbsEncryptionByDefaultResponse,

    -- * Response Lenses
    enableEbsEncryptionByDefaultResponse_ebsEncryptionByDefault,
    enableEbsEncryptionByDefaultResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newEnableEbsEncryptionByDefault' smart constructor.
data EnableEbsEncryptionByDefault = EnableEbsEncryptionByDefault'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EnableEbsEncryptionByDefault' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'enableEbsEncryptionByDefault_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
newEnableEbsEncryptionByDefault ::
  EnableEbsEncryptionByDefault
newEnableEbsEncryptionByDefault =
  EnableEbsEncryptionByDefault'
    { dryRun =
        Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
enableEbsEncryptionByDefault_dryRun :: Lens.Lens' EnableEbsEncryptionByDefault (Prelude.Maybe Prelude.Bool)
enableEbsEncryptionByDefault_dryRun = Lens.lens (\EnableEbsEncryptionByDefault' {dryRun} -> dryRun) (\s@EnableEbsEncryptionByDefault' {} a -> s {dryRun = a} :: EnableEbsEncryptionByDefault)

instance
  Prelude.AWSRequest
    EnableEbsEncryptionByDefault
  where
  type
    Rs EnableEbsEncryptionByDefault =
      EnableEbsEncryptionByDefaultResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          EnableEbsEncryptionByDefaultResponse'
            Prelude.<$> (x Prelude..@? "ebsEncryptionByDefault")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    EnableEbsEncryptionByDefault

instance Prelude.NFData EnableEbsEncryptionByDefault

instance
  Prelude.ToHeaders
    EnableEbsEncryptionByDefault
  where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath EnableEbsEncryptionByDefault where
  toPath = Prelude.const "/"

instance Prelude.ToQuery EnableEbsEncryptionByDefault where
  toQuery EnableEbsEncryptionByDefault' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "EnableEbsEncryptionByDefault" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun
      ]

-- | /See:/ 'newEnableEbsEncryptionByDefaultResponse' smart constructor.
data EnableEbsEncryptionByDefaultResponse = EnableEbsEncryptionByDefaultResponse'
  { -- | The updated status of encryption by default.
    ebsEncryptionByDefault :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EnableEbsEncryptionByDefaultResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ebsEncryptionByDefault', 'enableEbsEncryptionByDefaultResponse_ebsEncryptionByDefault' - The updated status of encryption by default.
--
-- 'httpStatus', 'enableEbsEncryptionByDefaultResponse_httpStatus' - The response's http status code.
newEnableEbsEncryptionByDefaultResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  EnableEbsEncryptionByDefaultResponse
newEnableEbsEncryptionByDefaultResponse pHttpStatus_ =
  EnableEbsEncryptionByDefaultResponse'
    { ebsEncryptionByDefault =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated status of encryption by default.
enableEbsEncryptionByDefaultResponse_ebsEncryptionByDefault :: Lens.Lens' EnableEbsEncryptionByDefaultResponse (Prelude.Maybe Prelude.Bool)
enableEbsEncryptionByDefaultResponse_ebsEncryptionByDefault = Lens.lens (\EnableEbsEncryptionByDefaultResponse' {ebsEncryptionByDefault} -> ebsEncryptionByDefault) (\s@EnableEbsEncryptionByDefaultResponse' {} a -> s {ebsEncryptionByDefault = a} :: EnableEbsEncryptionByDefaultResponse)

-- | The response's http status code.
enableEbsEncryptionByDefaultResponse_httpStatus :: Lens.Lens' EnableEbsEncryptionByDefaultResponse Prelude.Int
enableEbsEncryptionByDefaultResponse_httpStatus = Lens.lens (\EnableEbsEncryptionByDefaultResponse' {httpStatus} -> httpStatus) (\s@EnableEbsEncryptionByDefaultResponse' {} a -> s {httpStatus = a} :: EnableEbsEncryptionByDefaultResponse)

instance
  Prelude.NFData
    EnableEbsEncryptionByDefaultResponse
