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
-- Module      : Network.AWS.STS.GetAccessKeyInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the account identifier for the specified access key ID.
--
-- Access keys consist of two parts: an access key ID (for example,
-- @AKIAIOSFODNN7EXAMPLE@) and a secret access key (for example,
-- @wJalrXUtnFEMI\/K7MDENG\/bPxRfiCYEXAMPLEKEY@). For more information
-- about access keys, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_access-keys.html Managing Access Keys for IAM Users>
-- in the /IAM User Guide/.
--
-- When you pass an access key ID to this operation, it returns the ID of
-- the AWS account to which the keys belong. Access key IDs beginning with
-- @AKIA@ are long-term credentials for an IAM user or the AWS account root
-- user. Access key IDs beginning with @ASIA@ are temporary credentials
-- that are created using STS operations. If the account in the response
-- belongs to you, you can sign in as the root user and review your root
-- user access keys. Then, you can pull a
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_getting-report.html credentials report>
-- to learn which IAM user owns the keys. To learn who requested the
-- temporary credentials for an @ASIA@ access key, view the STS events in
-- your
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/cloudtrail-integration.html CloudTrail logs>
-- in the /IAM User Guide/.
--
-- This operation does not indicate the state of the access key. The key
-- might be active, inactive, or deleted. Active keys might not have
-- permissions to perform an operation. Providing a deleted access key
-- might return an error that the key doesn\'t exist.
module Network.AWS.STS.GetAccessKeyInfo
  ( -- * Creating a Request
    GetAccessKeyInfo (..),
    newGetAccessKeyInfo,

    -- * Request Lenses
    getAccessKeyInfo_accessKeyId,

    -- * Destructuring the Response
    GetAccessKeyInfoResponse (..),
    newGetAccessKeyInfoResponse,

    -- * Response Lenses
    getAccessKeyInfoResponse_account,
    getAccessKeyInfoResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.STS.Types

-- | /See:/ 'newGetAccessKeyInfo' smart constructor.
data GetAccessKeyInfo = GetAccessKeyInfo'
  { -- | The identifier of an access key.
    --
    -- This parameter allows (through its regex pattern) a string of characters
    -- that can consist of any upper- or lowercase letter or digit.
    accessKeyId :: Prelude.AccessKey
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetAccessKeyInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessKeyId', 'getAccessKeyInfo_accessKeyId' - The identifier of an access key.
--
-- This parameter allows (through its regex pattern) a string of characters
-- that can consist of any upper- or lowercase letter or digit.
newGetAccessKeyInfo ::
  -- | 'accessKeyId'
  Prelude.AccessKey ->
  GetAccessKeyInfo
newGetAccessKeyInfo pAccessKeyId_ =
  GetAccessKeyInfo' {accessKeyId = pAccessKeyId_}

-- | The identifier of an access key.
--
-- This parameter allows (through its regex pattern) a string of characters
-- that can consist of any upper- or lowercase letter or digit.
getAccessKeyInfo_accessKeyId :: Lens.Lens' GetAccessKeyInfo Prelude.AccessKey
getAccessKeyInfo_accessKeyId = Lens.lens (\GetAccessKeyInfo' {accessKeyId} -> accessKeyId) (\s@GetAccessKeyInfo' {} a -> s {accessKeyId = a} :: GetAccessKeyInfo)

instance Prelude.AWSRequest GetAccessKeyInfo where
  type Rs GetAccessKeyInfo = GetAccessKeyInfoResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "GetAccessKeyInfoResult"
      ( \s h x ->
          GetAccessKeyInfoResponse'
            Prelude.<$> (x Prelude..@? "Account")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAccessKeyInfo

instance Prelude.NFData GetAccessKeyInfo

instance Prelude.ToHeaders GetAccessKeyInfo where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath GetAccessKeyInfo where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetAccessKeyInfo where
  toQuery GetAccessKeyInfo' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("GetAccessKeyInfo" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2011-06-15" :: Prelude.ByteString),
        "AccessKeyId" Prelude.=: accessKeyId
      ]

-- | /See:/ 'newGetAccessKeyInfoResponse' smart constructor.
data GetAccessKeyInfoResponse = GetAccessKeyInfoResponse'
  { -- | The number used to identify the AWS account.
    account :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetAccessKeyInfoResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'account', 'getAccessKeyInfoResponse_account' - The number used to identify the AWS account.
--
-- 'httpStatus', 'getAccessKeyInfoResponse_httpStatus' - The response's http status code.
newGetAccessKeyInfoResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAccessKeyInfoResponse
newGetAccessKeyInfoResponse pHttpStatus_ =
  GetAccessKeyInfoResponse'
    { account =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The number used to identify the AWS account.
getAccessKeyInfoResponse_account :: Lens.Lens' GetAccessKeyInfoResponse (Prelude.Maybe Prelude.Text)
getAccessKeyInfoResponse_account = Lens.lens (\GetAccessKeyInfoResponse' {account} -> account) (\s@GetAccessKeyInfoResponse' {} a -> s {account = a} :: GetAccessKeyInfoResponse)

-- | The response's http status code.
getAccessKeyInfoResponse_httpStatus :: Lens.Lens' GetAccessKeyInfoResponse Prelude.Int
getAccessKeyInfoResponse_httpStatus = Lens.lens (\GetAccessKeyInfoResponse' {httpStatus} -> httpStatus) (\s@GetAccessKeyInfoResponse' {} a -> s {httpStatus = a} :: GetAccessKeyInfoResponse)

instance Prelude.NFData GetAccessKeyInfoResponse
