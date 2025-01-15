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
-- Module      : Amazonka.CloudFront.Types.PublicKeyConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.PublicKeyConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configuration information about a public key that you can use with
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html signed URLs and signed cookies>,
-- or with
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/field-level-encryption.html field-level encryption>.
--
-- /See:/ 'newPublicKeyConfig' smart constructor.
data PublicKeyConfig = PublicKeyConfig'
  { -- | A comment to describe the public key. The comment cannot be longer than
    -- 128 characters.
    comment :: Prelude.Maybe Prelude.Text,
    -- | A string included in the request to help make sure that the request
    -- can\'t be replayed.
    callerReference :: Prelude.Text,
    -- | A name to help identify the public key.
    name :: Prelude.Text,
    -- | The public key that you can use with
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html signed URLs and signed cookies>,
    -- or with
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/field-level-encryption.html field-level encryption>.
    encodedKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PublicKeyConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comment', 'publicKeyConfig_comment' - A comment to describe the public key. The comment cannot be longer than
-- 128 characters.
--
-- 'callerReference', 'publicKeyConfig_callerReference' - A string included in the request to help make sure that the request
-- can\'t be replayed.
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

-- | A comment to describe the public key. The comment cannot be longer than
-- 128 characters.
publicKeyConfig_comment :: Lens.Lens' PublicKeyConfig (Prelude.Maybe Prelude.Text)
publicKeyConfig_comment = Lens.lens (\PublicKeyConfig' {comment} -> comment) (\s@PublicKeyConfig' {} a -> s {comment = a} :: PublicKeyConfig)

-- | A string included in the request to help make sure that the request
-- can\'t be replayed.
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

instance Data.FromXML PublicKeyConfig where
  parseXML x =
    PublicKeyConfig'
      Prelude.<$> (x Data..@? "Comment")
      Prelude.<*> (x Data..@ "CallerReference")
      Prelude.<*> (x Data..@ "Name")
      Prelude.<*> (x Data..@ "EncodedKey")

instance Prelude.Hashable PublicKeyConfig where
  hashWithSalt _salt PublicKeyConfig' {..} =
    _salt
      `Prelude.hashWithSalt` comment
      `Prelude.hashWithSalt` callerReference
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` encodedKey

instance Prelude.NFData PublicKeyConfig where
  rnf PublicKeyConfig' {..} =
    Prelude.rnf comment `Prelude.seq`
      Prelude.rnf callerReference `Prelude.seq`
        Prelude.rnf name `Prelude.seq`
          Prelude.rnf encodedKey

instance Data.ToXML PublicKeyConfig where
  toXML PublicKeyConfig' {..} =
    Prelude.mconcat
      [ "Comment" Data.@= comment,
        "CallerReference" Data.@= callerReference,
        "Name" Data.@= name,
        "EncodedKey" Data.@= encodedKey
      ]
