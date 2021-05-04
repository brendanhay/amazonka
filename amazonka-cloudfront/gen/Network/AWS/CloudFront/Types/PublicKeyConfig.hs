{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Configuration information about a public key that you can use with
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html signed URLs and signed cookies>,
-- or with
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/field-level-encryption.html field-level encryption>.
--
-- /See:/ 'newPublicKeyConfig' smart constructor.
data PublicKeyConfig = PublicKeyConfig'
  { -- | A comment to describe the public key.
    comment :: Prelude.Maybe Prelude.Text,
    -- | A string included in the request to help make sure that the request
    -- can’t be replayed.
    callerReference :: Prelude.Text,
    -- | A name to help identify the public key.
    name :: Prelude.Text,
    -- | The public key that you can use with
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html signed URLs and signed cookies>,
    -- or with
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/field-level-encryption.html field-level encryption>.
    encodedKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'encodedKey'
  Prelude.Text ->
  PublicKeyConfig
newPublicKeyConfig
  pCallerReference_
  pName_
  pEncodedKey_ =
    PublicKeyConfig'
      { comment = Prelude.Nothing,
        callerReference = pCallerReference_,
        name = pName_,
        encodedKey = pEncodedKey_
      }

-- | A comment to describe the public key.
publicKeyConfig_comment :: Lens.Lens' PublicKeyConfig (Prelude.Maybe Prelude.Text)
publicKeyConfig_comment = Lens.lens (\PublicKeyConfig' {comment} -> comment) (\s@PublicKeyConfig' {} a -> s {comment = a} :: PublicKeyConfig)

-- | A string included in the request to help make sure that the request
-- can’t be replayed.
publicKeyConfig_callerReference :: Lens.Lens' PublicKeyConfig Prelude.Text
publicKeyConfig_callerReference = Lens.lens (\PublicKeyConfig' {callerReference} -> callerReference) (\s@PublicKeyConfig' {} a -> s {callerReference = a} :: PublicKeyConfig)

-- | A name to help identify the public key.
publicKeyConfig_name :: Lens.Lens' PublicKeyConfig Prelude.Text
publicKeyConfig_name = Lens.lens (\PublicKeyConfig' {name} -> name) (\s@PublicKeyConfig' {} a -> s {name = a} :: PublicKeyConfig)

-- | The public key that you can use with
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html signed URLs and signed cookies>,
-- or with
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/field-level-encryption.html field-level encryption>.
publicKeyConfig_encodedKey :: Lens.Lens' PublicKeyConfig Prelude.Text
publicKeyConfig_encodedKey = Lens.lens (\PublicKeyConfig' {encodedKey} -> encodedKey) (\s@PublicKeyConfig' {} a -> s {encodedKey = a} :: PublicKeyConfig)

instance Prelude.FromXML PublicKeyConfig where
  parseXML x =
    PublicKeyConfig'
      Prelude.<$> (x Prelude..@? "Comment")
      Prelude.<*> (x Prelude..@ "CallerReference")
      Prelude.<*> (x Prelude..@ "Name")
      Prelude.<*> (x Prelude..@ "EncodedKey")

instance Prelude.Hashable PublicKeyConfig

instance Prelude.NFData PublicKeyConfig

instance Prelude.ToXML PublicKeyConfig where
  toXML PublicKeyConfig' {..} =
    Prelude.mconcat
      [ "Comment" Prelude.@= comment,
        "CallerReference" Prelude.@= callerReference,
        "Name" Prelude.@= name,
        "EncodedKey" Prelude.@= encodedKey
      ]
