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
-- Module      : Amazonka.CloudFront.Types.PublicKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.PublicKey where

import Amazonka.CloudFront.Types.PublicKeyConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A public key that you can use with
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html signed URLs and signed cookies>,
-- or with
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/field-level-encryption.html field-level encryption>.
--
-- /See:/ 'newPublicKey' smart constructor.
data PublicKey = PublicKey'
  { -- | The identifier of the public key.
    id :: Prelude.Text,
    -- | The date and time when the public key was uploaded.
    createdTime :: Data.ISO8601,
    -- | Configuration information about a public key that you can use with
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html signed URLs and signed cookies>,
    -- or with
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/field-level-encryption.html field-level encryption>.
    publicKeyConfig :: PublicKeyConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PublicKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'publicKey_id' - The identifier of the public key.
--
-- 'createdTime', 'publicKey_createdTime' - The date and time when the public key was uploaded.
--
-- 'publicKeyConfig', 'publicKey_publicKeyConfig' - Configuration information about a public key that you can use with
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html signed URLs and signed cookies>,
-- or with
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/field-level-encryption.html field-level encryption>.
newPublicKey ::
  -- | 'id'
  Prelude.Text ->
  -- | 'createdTime'
  Prelude.UTCTime ->
  -- | 'publicKeyConfig'
  PublicKeyConfig ->
  PublicKey
newPublicKey pId_ pCreatedTime_ pPublicKeyConfig_ =
  PublicKey'
    { id = pId_,
      createdTime = Data._Time Lens.# pCreatedTime_,
      publicKeyConfig = pPublicKeyConfig_
    }

-- | The identifier of the public key.
publicKey_id :: Lens.Lens' PublicKey Prelude.Text
publicKey_id = Lens.lens (\PublicKey' {id} -> id) (\s@PublicKey' {} a -> s {id = a} :: PublicKey)

-- | The date and time when the public key was uploaded.
publicKey_createdTime :: Lens.Lens' PublicKey Prelude.UTCTime
publicKey_createdTime = Lens.lens (\PublicKey' {createdTime} -> createdTime) (\s@PublicKey' {} a -> s {createdTime = a} :: PublicKey) Prelude.. Data._Time

-- | Configuration information about a public key that you can use with
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html signed URLs and signed cookies>,
-- or with
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/field-level-encryption.html field-level encryption>.
publicKey_publicKeyConfig :: Lens.Lens' PublicKey PublicKeyConfig
publicKey_publicKeyConfig = Lens.lens (\PublicKey' {publicKeyConfig} -> publicKeyConfig) (\s@PublicKey' {} a -> s {publicKeyConfig = a} :: PublicKey)

instance Data.FromXML PublicKey where
  parseXML x =
    PublicKey'
      Prelude.<$> (x Data..@ "Id")
      Prelude.<*> (x Data..@ "CreatedTime")
      Prelude.<*> (x Data..@ "PublicKeyConfig")

instance Prelude.Hashable PublicKey where
  hashWithSalt _salt PublicKey' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` publicKeyConfig

instance Prelude.NFData PublicKey where
  rnf PublicKey' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf publicKeyConfig
