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
-- Module      : Amazonka.IAM.GetSAMLProvider
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the SAML provider metadocument that was uploaded when the IAM
-- SAML provider resource object was created or updated.
--
-- This operation requires
-- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4>.
module Amazonka.IAM.GetSAMLProvider
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
    getSAMLProviderResponse_sAMLMetadataDocument,
    getSAMLProviderResponse_tags,
    getSAMLProviderResponse_validUntil,
    getSAMLProviderResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSAMLProvider' smart constructor.
data GetSAMLProvider = GetSAMLProvider'
  { -- | The Amazon Resource Name (ARN) of the SAML provider resource object in
    -- IAM to get information about.
    --
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /Amazon Web Services General Reference/.
    sAMLProviderArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- in the /Amazon Web Services General Reference/.
newGetSAMLProvider ::
  -- | 'sAMLProviderArn'
  Prelude.Text ->
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
-- in the /Amazon Web Services General Reference/.
getSAMLProvider_sAMLProviderArn :: Lens.Lens' GetSAMLProvider Prelude.Text
getSAMLProvider_sAMLProviderArn = Lens.lens (\GetSAMLProvider' {sAMLProviderArn} -> sAMLProviderArn) (\s@GetSAMLProvider' {} a -> s {sAMLProviderArn = a} :: GetSAMLProvider)

instance Core.AWSRequest GetSAMLProvider where
  type
    AWSResponse GetSAMLProvider =
      GetSAMLProviderResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "GetSAMLProviderResult"
      ( \s h x ->
          GetSAMLProviderResponse'
            Prelude.<$> (x Data..@? "CreateDate")
            Prelude.<*> (x Data..@? "SAMLMetadataDocument")
            Prelude.<*> ( x
                            Data..@? "Tags"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (x Data..@? "ValidUntil")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSAMLProvider where
  hashWithSalt _salt GetSAMLProvider' {..} =
    _salt `Prelude.hashWithSalt` sAMLProviderArn

instance Prelude.NFData GetSAMLProvider where
  rnf GetSAMLProvider' {..} =
    Prelude.rnf sAMLProviderArn

instance Data.ToHeaders GetSAMLProvider where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetSAMLProvider where
  toPath = Prelude.const "/"

instance Data.ToQuery GetSAMLProvider where
  toQuery GetSAMLProvider' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("GetSAMLProvider" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "SAMLProviderArn" Data.=: sAMLProviderArn
      ]

-- | Contains the response to a successful GetSAMLProvider request.
--
-- /See:/ 'newGetSAMLProviderResponse' smart constructor.
data GetSAMLProviderResponse = GetSAMLProviderResponse'
  { -- | The date and time when the SAML provider was created.
    createDate :: Prelude.Maybe Data.ISO8601,
    -- | The XML metadata document that includes information about an identity
    -- provider.
    sAMLMetadataDocument :: Prelude.Maybe Prelude.Text,
    -- | A list of tags that are attached to the specified IAM SAML provider. The
    -- returned list of tags is sorted by tag key. For more information about
    -- tagging, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
    -- in the /IAM User Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | The expiration date and time for the SAML provider.
    validUntil :: Prelude.Maybe Data.ISO8601,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'sAMLMetadataDocument', 'getSAMLProviderResponse_sAMLMetadataDocument' - The XML metadata document that includes information about an identity
-- provider.
--
-- 'tags', 'getSAMLProviderResponse_tags' - A list of tags that are attached to the specified IAM SAML provider. The
-- returned list of tags is sorted by tag key. For more information about
-- tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
--
-- 'validUntil', 'getSAMLProviderResponse_validUntil' - The expiration date and time for the SAML provider.
--
-- 'httpStatus', 'getSAMLProviderResponse_httpStatus' - The response's http status code.
newGetSAMLProviderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSAMLProviderResponse
newGetSAMLProviderResponse pHttpStatus_ =
  GetSAMLProviderResponse'
    { createDate =
        Prelude.Nothing,
      sAMLMetadataDocument = Prelude.Nothing,
      tags = Prelude.Nothing,
      validUntil = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date and time when the SAML provider was created.
getSAMLProviderResponse_createDate :: Lens.Lens' GetSAMLProviderResponse (Prelude.Maybe Prelude.UTCTime)
getSAMLProviderResponse_createDate = Lens.lens (\GetSAMLProviderResponse' {createDate} -> createDate) (\s@GetSAMLProviderResponse' {} a -> s {createDate = a} :: GetSAMLProviderResponse) Prelude.. Lens.mapping Data._Time

-- | The XML metadata document that includes information about an identity
-- provider.
getSAMLProviderResponse_sAMLMetadataDocument :: Lens.Lens' GetSAMLProviderResponse (Prelude.Maybe Prelude.Text)
getSAMLProviderResponse_sAMLMetadataDocument = Lens.lens (\GetSAMLProviderResponse' {sAMLMetadataDocument} -> sAMLMetadataDocument) (\s@GetSAMLProviderResponse' {} a -> s {sAMLMetadataDocument = a} :: GetSAMLProviderResponse)

-- | A list of tags that are attached to the specified IAM SAML provider. The
-- returned list of tags is sorted by tag key. For more information about
-- tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
getSAMLProviderResponse_tags :: Lens.Lens' GetSAMLProviderResponse (Prelude.Maybe [Tag])
getSAMLProviderResponse_tags = Lens.lens (\GetSAMLProviderResponse' {tags} -> tags) (\s@GetSAMLProviderResponse' {} a -> s {tags = a} :: GetSAMLProviderResponse) Prelude.. Lens.mapping Lens.coerced

-- | The expiration date and time for the SAML provider.
getSAMLProviderResponse_validUntil :: Lens.Lens' GetSAMLProviderResponse (Prelude.Maybe Prelude.UTCTime)
getSAMLProviderResponse_validUntil = Lens.lens (\GetSAMLProviderResponse' {validUntil} -> validUntil) (\s@GetSAMLProviderResponse' {} a -> s {validUntil = a} :: GetSAMLProviderResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
getSAMLProviderResponse_httpStatus :: Lens.Lens' GetSAMLProviderResponse Prelude.Int
getSAMLProviderResponse_httpStatus = Lens.lens (\GetSAMLProviderResponse' {httpStatus} -> httpStatus) (\s@GetSAMLProviderResponse' {} a -> s {httpStatus = a} :: GetSAMLProviderResponse)

instance Prelude.NFData GetSAMLProviderResponse where
  rnf GetSAMLProviderResponse' {..} =
    Prelude.rnf createDate
      `Prelude.seq` Prelude.rnf sAMLMetadataDocument
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf validUntil
      `Prelude.seq` Prelude.rnf httpStatus
