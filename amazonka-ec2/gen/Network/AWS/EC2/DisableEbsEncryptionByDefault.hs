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
-- Module      : Network.AWS.EC2.DisableEbsEncryptionByDefault
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables EBS encryption by default for your account in the current
-- Region.
--
-- After you disable encryption by default, you can still create encrypted
-- volumes by enabling encryption when you create each volume.
--
-- Disabling encryption by default does not change the encryption status of
-- your existing volumes.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS encryption>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Network.AWS.EC2.DisableEbsEncryptionByDefault
  ( -- * Creating a Request
    DisableEbsEncryptionByDefault (..),
    newDisableEbsEncryptionByDefault,

    -- * Request Lenses
    disableEbsEncryptionByDefault_dryRun,

    -- * Destructuring the Response
    DisableEbsEncryptionByDefaultResponse (..),
    newDisableEbsEncryptionByDefaultResponse,

    -- * Response Lenses
    disableEbsEncryptionByDefaultResponse_ebsEncryptionByDefault,
    disableEbsEncryptionByDefaultResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisableEbsEncryptionByDefault' smart constructor.
data DisableEbsEncryptionByDefault = DisableEbsEncryptionByDefault'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisableEbsEncryptionByDefault' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'disableEbsEncryptionByDefault_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
newDisableEbsEncryptionByDefault ::
  DisableEbsEncryptionByDefault
newDisableEbsEncryptionByDefault =
  DisableEbsEncryptionByDefault'
    { dryRun =
        Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
disableEbsEncryptionByDefault_dryRun :: Lens.Lens' DisableEbsEncryptionByDefault (Prelude.Maybe Prelude.Bool)
disableEbsEncryptionByDefault_dryRun = Lens.lens (\DisableEbsEncryptionByDefault' {dryRun} -> dryRun) (\s@DisableEbsEncryptionByDefault' {} a -> s {dryRun = a} :: DisableEbsEncryptionByDefault)

instance
  Prelude.AWSRequest
    DisableEbsEncryptionByDefault
  where
  type
    Rs DisableEbsEncryptionByDefault =
      DisableEbsEncryptionByDefaultResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DisableEbsEncryptionByDefaultResponse'
            Prelude.<$> (x Prelude..@? "ebsEncryptionByDefault")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisableEbsEncryptionByDefault

instance Prelude.NFData DisableEbsEncryptionByDefault

instance
  Prelude.ToHeaders
    DisableEbsEncryptionByDefault
  where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DisableEbsEncryptionByDefault where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DisableEbsEncryptionByDefault
  where
  toQuery DisableEbsEncryptionByDefault' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "DisableEbsEncryptionByDefault" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun
      ]

-- | /See:/ 'newDisableEbsEncryptionByDefaultResponse' smart constructor.
data DisableEbsEncryptionByDefaultResponse = DisableEbsEncryptionByDefaultResponse'
  { -- | The updated status of encryption by default.
    ebsEncryptionByDefault :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisableEbsEncryptionByDefaultResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ebsEncryptionByDefault', 'disableEbsEncryptionByDefaultResponse_ebsEncryptionByDefault' - The updated status of encryption by default.
--
-- 'httpStatus', 'disableEbsEncryptionByDefaultResponse_httpStatus' - The response's http status code.
newDisableEbsEncryptionByDefaultResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisableEbsEncryptionByDefaultResponse
newDisableEbsEncryptionByDefaultResponse pHttpStatus_ =
  DisableEbsEncryptionByDefaultResponse'
    { ebsEncryptionByDefault =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated status of encryption by default.
disableEbsEncryptionByDefaultResponse_ebsEncryptionByDefault :: Lens.Lens' DisableEbsEncryptionByDefaultResponse (Prelude.Maybe Prelude.Bool)
disableEbsEncryptionByDefaultResponse_ebsEncryptionByDefault = Lens.lens (\DisableEbsEncryptionByDefaultResponse' {ebsEncryptionByDefault} -> ebsEncryptionByDefault) (\s@DisableEbsEncryptionByDefaultResponse' {} a -> s {ebsEncryptionByDefault = a} :: DisableEbsEncryptionByDefaultResponse)

-- | The response's http status code.
disableEbsEncryptionByDefaultResponse_httpStatus :: Lens.Lens' DisableEbsEncryptionByDefaultResponse Prelude.Int
disableEbsEncryptionByDefaultResponse_httpStatus = Lens.lens (\DisableEbsEncryptionByDefaultResponse' {httpStatus} -> httpStatus) (\s@DisableEbsEncryptionByDefaultResponse' {} a -> s {httpStatus = a} :: DisableEbsEncryptionByDefaultResponse)

instance
  Prelude.NFData
    DisableEbsEncryptionByDefaultResponse
