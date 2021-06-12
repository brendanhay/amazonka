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
-- Module      : Network.AWS.Redshift.CreateHsmClientCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an HSM client certificate that an Amazon Redshift cluster will
-- use to connect to the client\'s HSM in order to store and retrieve the
-- keys used to encrypt the cluster databases.
--
-- The command returns a public key, which you must store in the HSM. In
-- addition to creating the HSM certificate, you must create an Amazon
-- Redshift HSM configuration that provides a cluster the information
-- needed to store and use encryption keys in the HSM. For more
-- information, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-HSM.html Hardware Security Modules>
-- in the Amazon Redshift Cluster Management Guide.
module Network.AWS.Redshift.CreateHsmClientCertificate
  ( -- * Creating a Request
    CreateHsmClientCertificate (..),
    newCreateHsmClientCertificate,

    -- * Request Lenses
    createHsmClientCertificate_tags,
    createHsmClientCertificate_hsmClientCertificateIdentifier,

    -- * Destructuring the Response
    CreateHsmClientCertificateResponse (..),
    newCreateHsmClientCertificateResponse,

    -- * Response Lenses
    createHsmClientCertificateResponse_hsmClientCertificate,
    createHsmClientCertificateResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newCreateHsmClientCertificate' smart constructor.
data CreateHsmClientCertificate = CreateHsmClientCertificate'
  { -- | A list of tag instances.
    tags :: Core.Maybe [Tag],
    -- | The identifier to be assigned to the new HSM client certificate that the
    -- cluster will use to connect to the HSM to use the database encryption
    -- keys.
    hsmClientCertificateIdentifier :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateHsmClientCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createHsmClientCertificate_tags' - A list of tag instances.
--
-- 'hsmClientCertificateIdentifier', 'createHsmClientCertificate_hsmClientCertificateIdentifier' - The identifier to be assigned to the new HSM client certificate that the
-- cluster will use to connect to the HSM to use the database encryption
-- keys.
newCreateHsmClientCertificate ::
  -- | 'hsmClientCertificateIdentifier'
  Core.Text ->
  CreateHsmClientCertificate
newCreateHsmClientCertificate
  pHsmClientCertificateIdentifier_ =
    CreateHsmClientCertificate'
      { tags = Core.Nothing,
        hsmClientCertificateIdentifier =
          pHsmClientCertificateIdentifier_
      }

-- | A list of tag instances.
createHsmClientCertificate_tags :: Lens.Lens' CreateHsmClientCertificate (Core.Maybe [Tag])
createHsmClientCertificate_tags = Lens.lens (\CreateHsmClientCertificate' {tags} -> tags) (\s@CreateHsmClientCertificate' {} a -> s {tags = a} :: CreateHsmClientCertificate) Core.. Lens.mapping Lens._Coerce

-- | The identifier to be assigned to the new HSM client certificate that the
-- cluster will use to connect to the HSM to use the database encryption
-- keys.
createHsmClientCertificate_hsmClientCertificateIdentifier :: Lens.Lens' CreateHsmClientCertificate Core.Text
createHsmClientCertificate_hsmClientCertificateIdentifier = Lens.lens (\CreateHsmClientCertificate' {hsmClientCertificateIdentifier} -> hsmClientCertificateIdentifier) (\s@CreateHsmClientCertificate' {} a -> s {hsmClientCertificateIdentifier = a} :: CreateHsmClientCertificate)

instance Core.AWSRequest CreateHsmClientCertificate where
  type
    AWSResponse CreateHsmClientCertificate =
      CreateHsmClientCertificateResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateHsmClientCertificateResult"
      ( \s h x ->
          CreateHsmClientCertificateResponse'
            Core.<$> (x Core..@? "HsmClientCertificate")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateHsmClientCertificate

instance Core.NFData CreateHsmClientCertificate

instance Core.ToHeaders CreateHsmClientCertificate where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateHsmClientCertificate where
  toPath = Core.const "/"

instance Core.ToQuery CreateHsmClientCertificate where
  toQuery CreateHsmClientCertificate' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("CreateHsmClientCertificate" :: Core.ByteString),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "Tags"
          Core.=: Core.toQuery (Core.toQueryList "Tag" Core.<$> tags),
        "HsmClientCertificateIdentifier"
          Core.=: hsmClientCertificateIdentifier
      ]

-- | /See:/ 'newCreateHsmClientCertificateResponse' smart constructor.
data CreateHsmClientCertificateResponse = CreateHsmClientCertificateResponse'
  { hsmClientCertificate :: Core.Maybe HsmClientCertificate,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateHsmClientCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hsmClientCertificate', 'createHsmClientCertificateResponse_hsmClientCertificate' - Undocumented member.
--
-- 'httpStatus', 'createHsmClientCertificateResponse_httpStatus' - The response's http status code.
newCreateHsmClientCertificateResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateHsmClientCertificateResponse
newCreateHsmClientCertificateResponse pHttpStatus_ =
  CreateHsmClientCertificateResponse'
    { hsmClientCertificate =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createHsmClientCertificateResponse_hsmClientCertificate :: Lens.Lens' CreateHsmClientCertificateResponse (Core.Maybe HsmClientCertificate)
createHsmClientCertificateResponse_hsmClientCertificate = Lens.lens (\CreateHsmClientCertificateResponse' {hsmClientCertificate} -> hsmClientCertificate) (\s@CreateHsmClientCertificateResponse' {} a -> s {hsmClientCertificate = a} :: CreateHsmClientCertificateResponse)

-- | The response's http status code.
createHsmClientCertificateResponse_httpStatus :: Lens.Lens' CreateHsmClientCertificateResponse Core.Int
createHsmClientCertificateResponse_httpStatus = Lens.lens (\CreateHsmClientCertificateResponse' {httpStatus} -> httpStatus) (\s@CreateHsmClientCertificateResponse' {} a -> s {httpStatus = a} :: CreateHsmClientCertificateResponse)

instance
  Core.NFData
    CreateHsmClientCertificateResponse
