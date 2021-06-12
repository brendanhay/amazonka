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
-- Module      : Network.AWS.IAM.GetSAMLProvider
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the SAML provider metadocument that was uploaded when the IAM
-- SAML provider resource object was created or updated.
--
-- This operation requires
-- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4>.
module Network.AWS.IAM.GetSAMLProvider
  ( -- * Creating a Request
    GetSAMLProvider (..),
    newGetSAMLProvider,

    -- * Request Lenses
    getSAMLProvider_sAMLProviderArn,

    -- * Destructuring the Response
    GetSAMLProviderResponse (..),
    newGetSAMLProviderResponse,

    -- * Response Lenses
    getSAMLProviderResponse_createDate,
    getSAMLProviderResponse_validUntil,
    getSAMLProviderResponse_tags,
    getSAMLProviderResponse_sAMLMetadataDocument,
    getSAMLProviderResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetSAMLProvider' smart constructor.
data GetSAMLProvider = GetSAMLProvider'
  { -- | The Amazon Resource Name (ARN) of the SAML provider resource object in
    -- IAM to get information about.
    --
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    sAMLProviderArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSAMLProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sAMLProviderArn', 'getSAMLProvider_sAMLProviderArn' - The Amazon Resource Name (ARN) of the SAML provider resource object in
-- IAM to get information about.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
newGetSAMLProvider ::
  -- | 'sAMLProviderArn'
  Core.Text ->
  GetSAMLProvider
newGetSAMLProvider pSAMLProviderArn_ =
  GetSAMLProvider'
    { sAMLProviderArn =
        pSAMLProviderArn_
    }

-- | The Amazon Resource Name (ARN) of the SAML provider resource object in
-- IAM to get information about.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
getSAMLProvider_sAMLProviderArn :: Lens.Lens' GetSAMLProvider Core.Text
getSAMLProvider_sAMLProviderArn = Lens.lens (\GetSAMLProvider' {sAMLProviderArn} -> sAMLProviderArn) (\s@GetSAMLProvider' {} a -> s {sAMLProviderArn = a} :: GetSAMLProvider)

instance Core.AWSRequest GetSAMLProvider where
  type
    AWSResponse GetSAMLProvider =
      GetSAMLProviderResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "GetSAMLProviderResult"
      ( \s h x ->
          GetSAMLProviderResponse'
            Core.<$> (x Core..@? "CreateDate")
            Core.<*> (x Core..@? "ValidUntil")
            Core.<*> ( x Core..@? "Tags" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (x Core..@? "SAMLMetadataDocument")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetSAMLProvider

instance Core.NFData GetSAMLProvider

instance Core.ToHeaders GetSAMLProvider where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetSAMLProvider where
  toPath = Core.const "/"

instance Core.ToQuery GetSAMLProvider where
  toQuery GetSAMLProvider' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("GetSAMLProvider" :: Core.ByteString),
        "Version" Core.=: ("2010-05-08" :: Core.ByteString),
        "SAMLProviderArn" Core.=: sAMLProviderArn
      ]

-- | Contains the response to a successful GetSAMLProvider request.
--
-- /See:/ 'newGetSAMLProviderResponse' smart constructor.
data GetSAMLProviderResponse = GetSAMLProviderResponse'
  { -- | The date and time when the SAML provider was created.
    createDate :: Core.Maybe Core.ISO8601,
    -- | The expiration date and time for the SAML provider.
    validUntil :: Core.Maybe Core.ISO8601,
    -- | A list of tags that are attached to the specified IAM SAML provider. The
    -- returned list of tags is sorted by tag key. For more information about
    -- tagging, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
    -- in the /IAM User Guide/.
    tags :: Core.Maybe [Tag],
    -- | The XML metadata document that includes information about an identity
    -- provider.
    sAMLMetadataDocument :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSAMLProviderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createDate', 'getSAMLProviderResponse_createDate' - The date and time when the SAML provider was created.
--
-- 'validUntil', 'getSAMLProviderResponse_validUntil' - The expiration date and time for the SAML provider.
--
-- 'tags', 'getSAMLProviderResponse_tags' - A list of tags that are attached to the specified IAM SAML provider. The
-- returned list of tags is sorted by tag key. For more information about
-- tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
--
-- 'sAMLMetadataDocument', 'getSAMLProviderResponse_sAMLMetadataDocument' - The XML metadata document that includes information about an identity
-- provider.
--
-- 'httpStatus', 'getSAMLProviderResponse_httpStatus' - The response's http status code.
newGetSAMLProviderResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetSAMLProviderResponse
newGetSAMLProviderResponse pHttpStatus_ =
  GetSAMLProviderResponse'
    { createDate = Core.Nothing,
      validUntil = Core.Nothing,
      tags = Core.Nothing,
      sAMLMetadataDocument = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date and time when the SAML provider was created.
getSAMLProviderResponse_createDate :: Lens.Lens' GetSAMLProviderResponse (Core.Maybe Core.UTCTime)
getSAMLProviderResponse_createDate = Lens.lens (\GetSAMLProviderResponse' {createDate} -> createDate) (\s@GetSAMLProviderResponse' {} a -> s {createDate = a} :: GetSAMLProviderResponse) Core.. Lens.mapping Core._Time

-- | The expiration date and time for the SAML provider.
getSAMLProviderResponse_validUntil :: Lens.Lens' GetSAMLProviderResponse (Core.Maybe Core.UTCTime)
getSAMLProviderResponse_validUntil = Lens.lens (\GetSAMLProviderResponse' {validUntil} -> validUntil) (\s@GetSAMLProviderResponse' {} a -> s {validUntil = a} :: GetSAMLProviderResponse) Core.. Lens.mapping Core._Time

-- | A list of tags that are attached to the specified IAM SAML provider. The
-- returned list of tags is sorted by tag key. For more information about
-- tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
getSAMLProviderResponse_tags :: Lens.Lens' GetSAMLProviderResponse (Core.Maybe [Tag])
getSAMLProviderResponse_tags = Lens.lens (\GetSAMLProviderResponse' {tags} -> tags) (\s@GetSAMLProviderResponse' {} a -> s {tags = a} :: GetSAMLProviderResponse) Core.. Lens.mapping Lens._Coerce

-- | The XML metadata document that includes information about an identity
-- provider.
getSAMLProviderResponse_sAMLMetadataDocument :: Lens.Lens' GetSAMLProviderResponse (Core.Maybe Core.Text)
getSAMLProviderResponse_sAMLMetadataDocument = Lens.lens (\GetSAMLProviderResponse' {sAMLMetadataDocument} -> sAMLMetadataDocument) (\s@GetSAMLProviderResponse' {} a -> s {sAMLMetadataDocument = a} :: GetSAMLProviderResponse)

-- | The response's http status code.
getSAMLProviderResponse_httpStatus :: Lens.Lens' GetSAMLProviderResponse Core.Int
getSAMLProviderResponse_httpStatus = Lens.lens (\GetSAMLProviderResponse' {httpStatus} -> httpStatus) (\s@GetSAMLProviderResponse' {} a -> s {httpStatus = a} :: GetSAMLProviderResponse)

instance Core.NFData GetSAMLProviderResponse
