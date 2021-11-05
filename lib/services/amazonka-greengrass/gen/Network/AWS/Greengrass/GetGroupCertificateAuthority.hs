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
-- Module      : Amazonka.Greengrass.GetGroupCertificateAuthority
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retreives the CA associated with a group. Returns the public key of the
-- CA.
module Amazonka.Greengrass.GetGroupCertificateAuthority
  ( -- * Creating a Request
    GetGroupCertificateAuthority (..),
    newGetGroupCertificateAuthority,

    -- * Request Lenses
    getGroupCertificateAuthority_certificateAuthorityId,
    getGroupCertificateAuthority_groupId,

    -- * Destructuring the Response
    GetGroupCertificateAuthorityResponse (..),
    newGetGroupCertificateAuthorityResponse,

    -- * Response Lenses
    getGroupCertificateAuthorityResponse_pemEncodedCertificate,
    getGroupCertificateAuthorityResponse_groupCertificateAuthorityArn,
    getGroupCertificateAuthorityResponse_groupCertificateAuthorityId,
    getGroupCertificateAuthorityResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.Greengrass.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetGroupCertificateAuthority' smart constructor.
data GetGroupCertificateAuthority = GetGroupCertificateAuthority'
  { -- | The ID of the certificate authority.
    certificateAuthorityId :: Prelude.Text,
    -- | The ID of the Greengrass group.
    groupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetGroupCertificateAuthority' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateAuthorityId', 'getGroupCertificateAuthority_certificateAuthorityId' - The ID of the certificate authority.
--
-- 'groupId', 'getGroupCertificateAuthority_groupId' - The ID of the Greengrass group.
newGetGroupCertificateAuthority ::
  -- | 'certificateAuthorityId'
  Prelude.Text ->
  -- | 'groupId'
  Prelude.Text ->
  GetGroupCertificateAuthority
newGetGroupCertificateAuthority
  pCertificateAuthorityId_
  pGroupId_ =
    GetGroupCertificateAuthority'
      { certificateAuthorityId =
          pCertificateAuthorityId_,
        groupId = pGroupId_
      }

-- | The ID of the certificate authority.
getGroupCertificateAuthority_certificateAuthorityId :: Lens.Lens' GetGroupCertificateAuthority Prelude.Text
getGroupCertificateAuthority_certificateAuthorityId = Lens.lens (\GetGroupCertificateAuthority' {certificateAuthorityId} -> certificateAuthorityId) (\s@GetGroupCertificateAuthority' {} a -> s {certificateAuthorityId = a} :: GetGroupCertificateAuthority)

-- | The ID of the Greengrass group.
getGroupCertificateAuthority_groupId :: Lens.Lens' GetGroupCertificateAuthority Prelude.Text
getGroupCertificateAuthority_groupId = Lens.lens (\GetGroupCertificateAuthority' {groupId} -> groupId) (\s@GetGroupCertificateAuthority' {} a -> s {groupId = a} :: GetGroupCertificateAuthority)

instance Core.AWSRequest GetGroupCertificateAuthority where
  type
    AWSResponse GetGroupCertificateAuthority =
      GetGroupCertificateAuthorityResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetGroupCertificateAuthorityResponse'
            Prelude.<$> (x Core..?> "PemEncodedCertificate")
            Prelude.<*> (x Core..?> "GroupCertificateAuthorityArn")
            Prelude.<*> (x Core..?> "GroupCertificateAuthorityId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetGroupCertificateAuthority

instance Prelude.NFData GetGroupCertificateAuthority

instance Core.ToHeaders GetGroupCertificateAuthority where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetGroupCertificateAuthority where
  toPath GetGroupCertificateAuthority' {..} =
    Prelude.mconcat
      [ "/greengrass/groups/",
        Core.toBS groupId,
        "/certificateauthorities/",
        Core.toBS certificateAuthorityId
      ]

instance Core.ToQuery GetGroupCertificateAuthority where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetGroupCertificateAuthorityResponse' smart constructor.
data GetGroupCertificateAuthorityResponse = GetGroupCertificateAuthorityResponse'
  { -- | The PEM encoded certificate for the group.
    pemEncodedCertificate :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the certificate authority for the group.
    groupCertificateAuthorityArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the certificate authority for the group.
    groupCertificateAuthorityId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetGroupCertificateAuthorityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pemEncodedCertificate', 'getGroupCertificateAuthorityResponse_pemEncodedCertificate' - The PEM encoded certificate for the group.
--
-- 'groupCertificateAuthorityArn', 'getGroupCertificateAuthorityResponse_groupCertificateAuthorityArn' - The ARN of the certificate authority for the group.
--
-- 'groupCertificateAuthorityId', 'getGroupCertificateAuthorityResponse_groupCertificateAuthorityId' - The ID of the certificate authority for the group.
--
-- 'httpStatus', 'getGroupCertificateAuthorityResponse_httpStatus' - The response's http status code.
newGetGroupCertificateAuthorityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetGroupCertificateAuthorityResponse
newGetGroupCertificateAuthorityResponse pHttpStatus_ =
  GetGroupCertificateAuthorityResponse'
    { pemEncodedCertificate =
        Prelude.Nothing,
      groupCertificateAuthorityArn =
        Prelude.Nothing,
      groupCertificateAuthorityId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The PEM encoded certificate for the group.
getGroupCertificateAuthorityResponse_pemEncodedCertificate :: Lens.Lens' GetGroupCertificateAuthorityResponse (Prelude.Maybe Prelude.Text)
getGroupCertificateAuthorityResponse_pemEncodedCertificate = Lens.lens (\GetGroupCertificateAuthorityResponse' {pemEncodedCertificate} -> pemEncodedCertificate) (\s@GetGroupCertificateAuthorityResponse' {} a -> s {pemEncodedCertificate = a} :: GetGroupCertificateAuthorityResponse)

-- | The ARN of the certificate authority for the group.
getGroupCertificateAuthorityResponse_groupCertificateAuthorityArn :: Lens.Lens' GetGroupCertificateAuthorityResponse (Prelude.Maybe Prelude.Text)
getGroupCertificateAuthorityResponse_groupCertificateAuthorityArn = Lens.lens (\GetGroupCertificateAuthorityResponse' {groupCertificateAuthorityArn} -> groupCertificateAuthorityArn) (\s@GetGroupCertificateAuthorityResponse' {} a -> s {groupCertificateAuthorityArn = a} :: GetGroupCertificateAuthorityResponse)

-- | The ID of the certificate authority for the group.
getGroupCertificateAuthorityResponse_groupCertificateAuthorityId :: Lens.Lens' GetGroupCertificateAuthorityResponse (Prelude.Maybe Prelude.Text)
getGroupCertificateAuthorityResponse_groupCertificateAuthorityId = Lens.lens (\GetGroupCertificateAuthorityResponse' {groupCertificateAuthorityId} -> groupCertificateAuthorityId) (\s@GetGroupCertificateAuthorityResponse' {} a -> s {groupCertificateAuthorityId = a} :: GetGroupCertificateAuthorityResponse)

-- | The response's http status code.
getGroupCertificateAuthorityResponse_httpStatus :: Lens.Lens' GetGroupCertificateAuthorityResponse Prelude.Int
getGroupCertificateAuthorityResponse_httpStatus = Lens.lens (\GetGroupCertificateAuthorityResponse' {httpStatus} -> httpStatus) (\s@GetGroupCertificateAuthorityResponse' {} a -> s {httpStatus = a} :: GetGroupCertificateAuthorityResponse)

instance
  Prelude.NFData
    GetGroupCertificateAuthorityResponse
