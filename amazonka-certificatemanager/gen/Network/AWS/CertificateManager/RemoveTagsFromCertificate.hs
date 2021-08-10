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
-- Module      : Network.AWS.CertificateManager.RemoveTagsFromCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.CertificateManager.RemoveTagsFromCertificate
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

import Network.AWS.CertificateManager.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
      tags = Lens._Coerce Lens.# pTags_
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
removeTagsFromCertificate_tags = Lens.lens (\RemoveTagsFromCertificate' {tags} -> tags) (\s@RemoveTagsFromCertificate' {} a -> s {tags = a} :: RemoveTagsFromCertificate) Prelude.. Lens._Coerce

instance Core.AWSRequest RemoveTagsFromCertificate where
  type
    AWSResponse RemoveTagsFromCertificate =
      RemoveTagsFromCertificateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      RemoveTagsFromCertificateResponse'

instance Prelude.Hashable RemoveTagsFromCertificate

instance Prelude.NFData RemoveTagsFromCertificate

instance Core.ToHeaders RemoveTagsFromCertificate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CertificateManager.RemoveTagsFromCertificate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RemoveTagsFromCertificate where
  toJSON RemoveTagsFromCertificate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("CertificateArn" Core..= certificateArn),
            Prelude.Just ("Tags" Core..= tags)
          ]
      )

instance Core.ToPath RemoveTagsFromCertificate where
  toPath = Prelude.const "/"

instance Core.ToQuery RemoveTagsFromCertificate where
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
