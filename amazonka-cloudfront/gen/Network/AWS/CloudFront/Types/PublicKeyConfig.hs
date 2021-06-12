{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.PublicKeyConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.PublicKeyConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Configuration information about a public key that you can use with
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html signed URLs and signed cookies>,
-- or with
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/field-level-encryption.html field-level encryption>.
--
-- /See:/ 'newPublicKeyConfig' smart constructor.
data PublicKeyConfig = PublicKeyConfig'
  { -- | A comment to describe the public key.
    comment :: Core.Maybe Core.Text,
    -- | A string included in the request to help make sure that the request
    -- can’t be replayed.
    callerReference :: Core.Text,
    -- | A name to help identify the public key.
    name :: Core.Text,
    -- | The public key that you can use with
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html signed URLs and signed cookies>,
    -- or with
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/field-level-encryption.html field-level encryption>.
    encodedKey :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PublicKeyConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comment', 'publicKeyConfig_comment' - A comment to describe the public key.
--
-- 'callerReference', 'publicKeyConfig_callerReference' - A string included in the request to help make sure that the request
-- can’t be replayed.
--
-- 'name', 'publicKeyConfig_name' - A name to help identify the public key.
--
-- 'encodedKey', 'publicKeyConfig_encodedKey' - The public key that you can use with
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html signed URLs and signed cookies>,
-- or with
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/field-level-encryption.html field-level encryption>.
newPublicKeyConfig ::
  -- | 'callerReference'
  Core.Text ->
  -- | 'name'
  Core.Text ->
  -- | 'encodedKey'
  Core.Text ->
  PublicKeyConfig
newPublicKeyConfig
  pCallerReference_
  pName_
  pEncodedKey_ =
    PublicKeyConfig'
      { comment = Core.Nothing,
        callerReference = pCallerReference_,
        name = pName_,
        encodedKey = pEncodedKey_
      }

-- | A comment to describe the public key.
publicKeyConfig_comment :: Lens.Lens' PublicKeyConfig (Core.Maybe Core.Text)
publicKeyConfig_comment = Lens.lens (\PublicKeyConfig' {comment} -> comment) (\s@PublicKeyConfig' {} a -> s {comment = a} :: PublicKeyConfig)

-- | A string included in the request to help make sure that the request
-- can’t be replayed.
publicKeyConfig_callerReference :: Lens.Lens' PublicKeyConfig Core.Text
publicKeyConfig_callerReference = Lens.lens (\PublicKeyConfig' {callerReference} -> callerReference) (\s@PublicKeyConfig' {} a -> s {callerReference = a} :: PublicKeyConfig)

-- | A name to help identify the public key.
publicKeyConfig_name :: Lens.Lens' PublicKeyConfig Core.Text
publicKeyConfig_name = Lens.lens (\PublicKeyConfig' {name} -> name) (\s@PublicKeyConfig' {} a -> s {name = a} :: PublicKeyConfig)

-- | The public key that you can use with
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html signed URLs and signed cookies>,
-- or with
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/field-level-encryption.html field-level encryption>.
publicKeyConfig_encodedKey :: Lens.Lens' PublicKeyConfig Core.Text
publicKeyConfig_encodedKey = Lens.lens (\PublicKeyConfig' {encodedKey} -> encodedKey) (\s@PublicKeyConfig' {} a -> s {encodedKey = a} :: PublicKeyConfig)

instance Core.FromXML PublicKeyConfig where
  parseXML x =
    PublicKeyConfig'
      Core.<$> (x Core..@? "Comment")
      Core.<*> (x Core..@ "CallerReference")
      Core.<*> (x Core..@ "Name")
      Core.<*> (x Core..@ "EncodedKey")

instance Core.Hashable PublicKeyConfig

instance Core.NFData PublicKeyConfig

instance Core.ToXML PublicKeyConfig where
  toXML PublicKeyConfig' {..} =
    Core.mconcat
      [ "Comment" Core.@= comment,
        "CallerReference" Core.@= callerReference,
        "Name" Core.@= name,
        "EncodedKey" Core.@= encodedKey
      ]
