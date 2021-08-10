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
-- Module      : Network.AWS.CertificateManager.AddTagsToCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more tags to an ACM certificate. Tags are labels that you
-- can use to identify and organize your AWS resources. Each tag consists
-- of a @key@ and an optional @value@. You specify the certificate on input
-- by its Amazon Resource Name (ARN). You specify the tag by using a
-- key-value pair.
--
-- You can apply a tag to just one certificate if you want to identify a
-- specific characteristic of that certificate, or you can apply the same
-- tag to multiple certificates if you want to filter for a common
-- relationship among those certificates. Similarly, you can apply the same
-- tag to multiple resources if you want to specify a relationship among
-- those resources. For example, you can add the same tag to an ACM
-- certificate and an Elastic Load Balancing load balancer to indicate that
-- they are both used by the same website. For more information, see
-- <https://docs.aws.amazon.com/acm/latest/userguide/tags.html Tagging ACM certificates>.
--
-- To remove one or more tags, use the RemoveTagsFromCertificate action. To
-- view all of the tags that have been applied to the certificate, use the
-- ListTagsForCertificate action.
module Network.AWS.CertificateManager.AddTagsToCertificate
  ( -- * Creating a Request
    AddTagsToCertificate (..),
    newAddTagsToCertificate,

    -- * Request Lenses
    addTagsToCertificate_certificateArn,
    addTagsToCertificate_tags,

    -- * Destructuring the Response
    AddTagsToCertificateResponse (..),
    newAddTagsToCertificateResponse,
  )
where

import Network.AWS.CertificateManager.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAddTagsToCertificate' smart constructor.
data AddTagsToCertificate = AddTagsToCertificate'
  { -- | String that contains the ARN of the ACM certificate to which the tag is
    -- to be applied. This must be of the form:
    --
    -- @arn:aws:acm:region:123456789012:certificate\/12345678-1234-1234-1234-123456789012@
    --
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>.
    certificateArn :: Prelude.Text,
    -- | The key-value pair that defines the tag. The tag value is optional.
    tags :: Prelude.NonEmpty Tag
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddTagsToCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateArn', 'addTagsToCertificate_certificateArn' - String that contains the ARN of the ACM certificate to which the tag is
-- to be applied. This must be of the form:
--
-- @arn:aws:acm:region:123456789012:certificate\/12345678-1234-1234-1234-123456789012@
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>.
--
-- 'tags', 'addTagsToCertificate_tags' - The key-value pair that defines the tag. The tag value is optional.
newAddTagsToCertificate ::
  -- | 'certificateArn'
  Prelude.Text ->
  -- | 'tags'
  Prelude.NonEmpty Tag ->
  AddTagsToCertificate
newAddTagsToCertificate pCertificateArn_ pTags_ =
  AddTagsToCertificate'
    { certificateArn =
        pCertificateArn_,
      tags = Lens._Coerce Lens.# pTags_
    }

-- | String that contains the ARN of the ACM certificate to which the tag is
-- to be applied. This must be of the form:
--
-- @arn:aws:acm:region:123456789012:certificate\/12345678-1234-1234-1234-123456789012@
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>.
addTagsToCertificate_certificateArn :: Lens.Lens' AddTagsToCertificate Prelude.Text
addTagsToCertificate_certificateArn = Lens.lens (\AddTagsToCertificate' {certificateArn} -> certificateArn) (\s@AddTagsToCertificate' {} a -> s {certificateArn = a} :: AddTagsToCertificate)

-- | The key-value pair that defines the tag. The tag value is optional.
addTagsToCertificate_tags :: Lens.Lens' AddTagsToCertificate (Prelude.NonEmpty Tag)
addTagsToCertificate_tags = Lens.lens (\AddTagsToCertificate' {tags} -> tags) (\s@AddTagsToCertificate' {} a -> s {tags = a} :: AddTagsToCertificate) Prelude.. Lens._Coerce

instance Core.AWSRequest AddTagsToCertificate where
  type
    AWSResponse AddTagsToCertificate =
      AddTagsToCertificateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull AddTagsToCertificateResponse'

instance Prelude.Hashable AddTagsToCertificate

instance Prelude.NFData AddTagsToCertificate

instance Core.ToHeaders AddTagsToCertificate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CertificateManager.AddTagsToCertificate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON AddTagsToCertificate where
  toJSON AddTagsToCertificate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("CertificateArn" Core..= certificateArn),
            Prelude.Just ("Tags" Core..= tags)
          ]
      )

instance Core.ToPath AddTagsToCertificate where
  toPath = Prelude.const "/"

instance Core.ToQuery AddTagsToCertificate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAddTagsToCertificateResponse' smart constructor.
data AddTagsToCertificateResponse = AddTagsToCertificateResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddTagsToCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAddTagsToCertificateResponse ::
  AddTagsToCertificateResponse
newAddTagsToCertificateResponse =
  AddTagsToCertificateResponse'

instance Prelude.NFData AddTagsToCertificateResponse
