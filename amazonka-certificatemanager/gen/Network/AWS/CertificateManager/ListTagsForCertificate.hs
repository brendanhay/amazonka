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
-- Module      : Network.AWS.CertificateManager.ListTagsForCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags that have been applied to the ACM certificate. Use the
-- certificate\'s Amazon Resource Name (ARN) to specify the certificate. To
-- add a tag to an ACM certificate, use the AddTagsToCertificate action. To
-- delete a tag, use the RemoveTagsFromCertificate action.
module Network.AWS.CertificateManager.ListTagsForCertificate
  ( -- * Creating a Request
    ListTagsForCertificate (..),
    newListTagsForCertificate,

    -- * Request Lenses
    listTagsForCertificate_certificateArn,

    -- * Destructuring the Response
    ListTagsForCertificateResponse (..),
    newListTagsForCertificateResponse,

    -- * Response Lenses
    listTagsForCertificateResponse_tags,
    listTagsForCertificateResponse_httpStatus,
  )
where

import Network.AWS.CertificateManager.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListTagsForCertificate' smart constructor.
data ListTagsForCertificate = ListTagsForCertificate'
  { -- | String that contains the ARN of the ACM certificate for which you want
    -- to list the tags. This must have the following form:
    --
    -- @arn:aws:acm:region:123456789012:certificate\/12345678-1234-1234-1234-123456789012@
    --
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>.
    certificateArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTagsForCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateArn', 'listTagsForCertificate_certificateArn' - String that contains the ARN of the ACM certificate for which you want
-- to list the tags. This must have the following form:
--
-- @arn:aws:acm:region:123456789012:certificate\/12345678-1234-1234-1234-123456789012@
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>.
newListTagsForCertificate ::
  -- | 'certificateArn'
  Prelude.Text ->
  ListTagsForCertificate
newListTagsForCertificate pCertificateArn_ =
  ListTagsForCertificate'
    { certificateArn =
        pCertificateArn_
    }

-- | String that contains the ARN of the ACM certificate for which you want
-- to list the tags. This must have the following form:
--
-- @arn:aws:acm:region:123456789012:certificate\/12345678-1234-1234-1234-123456789012@
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>.
listTagsForCertificate_certificateArn :: Lens.Lens' ListTagsForCertificate Prelude.Text
listTagsForCertificate_certificateArn = Lens.lens (\ListTagsForCertificate' {certificateArn} -> certificateArn) (\s@ListTagsForCertificate' {} a -> s {certificateArn = a} :: ListTagsForCertificate)

instance Core.AWSRequest ListTagsForCertificate where
  type
    AWSResponse ListTagsForCertificate =
      ListTagsForCertificateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTagsForCertificateResponse'
            Prelude.<$> (x Core..?> "Tags")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTagsForCertificate

instance Prelude.NFData ListTagsForCertificate

instance Core.ToHeaders ListTagsForCertificate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CertificateManager.ListTagsForCertificate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListTagsForCertificate where
  toJSON ListTagsForCertificate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("CertificateArn" Core..= certificateArn)
          ]
      )

instance Core.ToPath ListTagsForCertificate where
  toPath = Prelude.const "/"

instance Core.ToQuery ListTagsForCertificate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTagsForCertificateResponse' smart constructor.
data ListTagsForCertificateResponse = ListTagsForCertificateResponse'
  { -- | The key-value pairs that define the applied tags.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTagsForCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'listTagsForCertificateResponse_tags' - The key-value pairs that define the applied tags.
--
-- 'httpStatus', 'listTagsForCertificateResponse_httpStatus' - The response's http status code.
newListTagsForCertificateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTagsForCertificateResponse
newListTagsForCertificateResponse pHttpStatus_ =
  ListTagsForCertificateResponse'
    { tags =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The key-value pairs that define the applied tags.
listTagsForCertificateResponse_tags :: Lens.Lens' ListTagsForCertificateResponse (Prelude.Maybe (Prelude.NonEmpty Tag))
listTagsForCertificateResponse_tags = Lens.lens (\ListTagsForCertificateResponse' {tags} -> tags) (\s@ListTagsForCertificateResponse' {} a -> s {tags = a} :: ListTagsForCertificateResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listTagsForCertificateResponse_httpStatus :: Lens.Lens' ListTagsForCertificateResponse Prelude.Int
listTagsForCertificateResponse_httpStatus = Lens.lens (\ListTagsForCertificateResponse' {httpStatus} -> httpStatus) (\s@ListTagsForCertificateResponse' {} a -> s {httpStatus = a} :: ListTagsForCertificateResponse)

instance
  Prelude.NFData
    ListTagsForCertificateResponse
