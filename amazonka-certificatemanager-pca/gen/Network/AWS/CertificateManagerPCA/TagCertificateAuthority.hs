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
-- Module      : Network.AWS.CertificateManagerPCA.TagCertificateAuthority
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more tags to your private CA. Tags are labels that you can
-- use to identify and organize your AWS resources. Each tag consists of a
-- key and an optional value. You specify the private CA on input by its
-- Amazon Resource Name (ARN). You specify the tag by using a key-value
-- pair. You can apply a tag to just one private CA if you want to identify
-- a specific characteristic of that CA, or you can apply the same tag to
-- multiple private CAs if you want to filter for a common relationship
-- among those CAs. To remove one or more tags, use the
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_UntagCertificateAuthority.html UntagCertificateAuthority>
-- action. Call the
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListTags.html ListTags>
-- action to see what tags are associated with your CA.
module Network.AWS.CertificateManagerPCA.TagCertificateAuthority
  ( -- * Creating a Request
    TagCertificateAuthority (..),
    newTagCertificateAuthority,

    -- * Request Lenses
    tagCertificateAuthority_certificateAuthorityArn,
    tagCertificateAuthority_tags,

    -- * Destructuring the Response
    TagCertificateAuthorityResponse (..),
    newTagCertificateAuthorityResponse,
  )
where

import Network.AWS.CertificateManagerPCA.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newTagCertificateAuthority' smart constructor.
data TagCertificateAuthority = TagCertificateAuthority'
  { -- | The Amazon Resource Name (ARN) that was returned when you called
    -- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority>.
    -- This must be of the form:
    --
    -- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012 @
    certificateAuthorityArn :: Prelude.Text,
    -- | List of tags to be associated with the CA.
    tags :: Prelude.NonEmpty Tag
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagCertificateAuthority' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateAuthorityArn', 'tagCertificateAuthority_certificateAuthorityArn' - The Amazon Resource Name (ARN) that was returned when you called
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority>.
-- This must be of the form:
--
-- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012 @
--
-- 'tags', 'tagCertificateAuthority_tags' - List of tags to be associated with the CA.
newTagCertificateAuthority ::
  -- | 'certificateAuthorityArn'
  Prelude.Text ->
  -- | 'tags'
  Prelude.NonEmpty Tag ->
  TagCertificateAuthority
newTagCertificateAuthority
  pCertificateAuthorityArn_
  pTags_ =
    TagCertificateAuthority'
      { certificateAuthorityArn =
          pCertificateAuthorityArn_,
        tags = Lens._Coerce Lens.# pTags_
      }

-- | The Amazon Resource Name (ARN) that was returned when you called
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority>.
-- This must be of the form:
--
-- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012 @
tagCertificateAuthority_certificateAuthorityArn :: Lens.Lens' TagCertificateAuthority Prelude.Text
tagCertificateAuthority_certificateAuthorityArn = Lens.lens (\TagCertificateAuthority' {certificateAuthorityArn} -> certificateAuthorityArn) (\s@TagCertificateAuthority' {} a -> s {certificateAuthorityArn = a} :: TagCertificateAuthority)

-- | List of tags to be associated with the CA.
tagCertificateAuthority_tags :: Lens.Lens' TagCertificateAuthority (Prelude.NonEmpty Tag)
tagCertificateAuthority_tags = Lens.lens (\TagCertificateAuthority' {tags} -> tags) (\s@TagCertificateAuthority' {} a -> s {tags = a} :: TagCertificateAuthority) Prelude.. Lens._Coerce

instance Core.AWSRequest TagCertificateAuthority where
  type
    AWSResponse TagCertificateAuthority =
      TagCertificateAuthorityResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      TagCertificateAuthorityResponse'

instance Prelude.Hashable TagCertificateAuthority

instance Prelude.NFData TagCertificateAuthority

instance Core.ToHeaders TagCertificateAuthority where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ACMPrivateCA.TagCertificateAuthority" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON TagCertificateAuthority where
  toJSON TagCertificateAuthority' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "CertificateAuthorityArn"
                  Core..= certificateAuthorityArn
              ),
            Prelude.Just ("Tags" Core..= tags)
          ]
      )

instance Core.ToPath TagCertificateAuthority where
  toPath = Prelude.const "/"

instance Core.ToQuery TagCertificateAuthority where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newTagCertificateAuthorityResponse' smart constructor.
data TagCertificateAuthorityResponse = TagCertificateAuthorityResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagCertificateAuthorityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newTagCertificateAuthorityResponse ::
  TagCertificateAuthorityResponse
newTagCertificateAuthorityResponse =
  TagCertificateAuthorityResponse'

instance
  Prelude.NFData
    TagCertificateAuthorityResponse
