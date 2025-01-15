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
-- Module      : Amazonka.CertificateManager.RemoveTagsFromCertificate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove one or more tags from an ACM certificate. A tag consists of a
-- key-value pair. If you do not specify the value portion of the tag when
-- calling this function, the tag will be removed regardless of value. If
-- you specify a value, the tag is removed only if it is associated with
-- the specified value.
--
-- To add tags to a certificate, use the AddTagsToCertificate action. To
-- view all of the tags that have been applied to a specific ACM
-- certificate, use the ListTagsForCertificate action.
module Amazonka.CertificateManager.RemoveTagsFromCertificate
  ( -- * Creating a Request
    RemoveTagsFromCertificate (..),
    newRemoveTagsFromCertificate,

    -- * Request Lenses
    removeTagsFromCertificate_certificateArn,
    removeTagsFromCertificate_tags,

    -- * Destructuring the Response
    RemoveTagsFromCertificateResponse (..),
    newRemoveTagsFromCertificateResponse,
  )
where

import Amazonka.CertificateManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRemoveTagsFromCertificate' smart constructor.
data RemoveTagsFromCertificate = RemoveTagsFromCertificate'
  { -- | String that contains the ARN of the ACM Certificate with one or more
    -- tags that you want to remove. This must be of the form:
    --
    -- @arn:aws:acm:region:123456789012:certificate\/12345678-1234-1234-1234-123456789012@
    --
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>.
    certificateArn :: Prelude.Text,
    -- | The key-value pair that defines the tag to remove.
    tags :: Prelude.NonEmpty Tag
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveTagsFromCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateArn', 'removeTagsFromCertificate_certificateArn' - String that contains the ARN of the ACM Certificate with one or more
-- tags that you want to remove. This must be of the form:
--
-- @arn:aws:acm:region:123456789012:certificate\/12345678-1234-1234-1234-123456789012@
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>.
--
-- 'tags', 'removeTagsFromCertificate_tags' - The key-value pair that defines the tag to remove.
newRemoveTagsFromCertificate ::
  -- | 'certificateArn'
  Prelude.Text ->
  -- | 'tags'
  Prelude.NonEmpty Tag ->
  RemoveTagsFromCertificate
newRemoveTagsFromCertificate pCertificateArn_ pTags_ =
  RemoveTagsFromCertificate'
    { certificateArn =
        pCertificateArn_,
      tags = Lens.coerced Lens.# pTags_
    }

-- | String that contains the ARN of the ACM Certificate with one or more
-- tags that you want to remove. This must be of the form:
--
-- @arn:aws:acm:region:123456789012:certificate\/12345678-1234-1234-1234-123456789012@
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>.
removeTagsFromCertificate_certificateArn :: Lens.Lens' RemoveTagsFromCertificate Prelude.Text
removeTagsFromCertificate_certificateArn = Lens.lens (\RemoveTagsFromCertificate' {certificateArn} -> certificateArn) (\s@RemoveTagsFromCertificate' {} a -> s {certificateArn = a} :: RemoveTagsFromCertificate)

-- | The key-value pair that defines the tag to remove.
removeTagsFromCertificate_tags :: Lens.Lens' RemoveTagsFromCertificate (Prelude.NonEmpty Tag)
removeTagsFromCertificate_tags = Lens.lens (\RemoveTagsFromCertificate' {tags} -> tags) (\s@RemoveTagsFromCertificate' {} a -> s {tags = a} :: RemoveTagsFromCertificate) Prelude.. Lens.coerced

instance Core.AWSRequest RemoveTagsFromCertificate where
  type
    AWSResponse RemoveTagsFromCertificate =
      RemoveTagsFromCertificateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      RemoveTagsFromCertificateResponse'

instance Prelude.Hashable RemoveTagsFromCertificate where
  hashWithSalt _salt RemoveTagsFromCertificate' {..} =
    _salt
      `Prelude.hashWithSalt` certificateArn
      `Prelude.hashWithSalt` tags

instance Prelude.NFData RemoveTagsFromCertificate where
  rnf RemoveTagsFromCertificate' {..} =
    Prelude.rnf certificateArn `Prelude.seq`
      Prelude.rnf tags

instance Data.ToHeaders RemoveTagsFromCertificate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CertificateManager.RemoveTagsFromCertificate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RemoveTagsFromCertificate where
  toJSON RemoveTagsFromCertificate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("CertificateArn" Data..= certificateArn),
            Prelude.Just ("Tags" Data..= tags)
          ]
      )

instance Data.ToPath RemoveTagsFromCertificate where
  toPath = Prelude.const "/"

instance Data.ToQuery RemoveTagsFromCertificate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveTagsFromCertificateResponse' smart constructor.
data RemoveTagsFromCertificateResponse = RemoveTagsFromCertificateResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveTagsFromCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRemoveTagsFromCertificateResponse ::
  RemoveTagsFromCertificateResponse
newRemoveTagsFromCertificateResponse =
  RemoveTagsFromCertificateResponse'

instance
  Prelude.NFData
    RemoveTagsFromCertificateResponse
  where
  rnf _ = ()
