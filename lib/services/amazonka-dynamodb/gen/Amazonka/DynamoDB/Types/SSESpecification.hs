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
-- Module      : Amazonka.DynamoDB.Types.SSESpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.SSESpecification where

import qualified Amazonka.Core as Core
import Amazonka.DynamoDB.Internal
import Amazonka.DynamoDB.Types.SSEType
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents the settings used to enable server-side encryption.
--
-- /See:/ 'newSSESpecification' smart constructor.
data SSESpecification = SSESpecification'
  { -- | The AWS KMS customer master key (CMK) that should be used for the AWS
    -- KMS encryption. To specify a CMK, use its key ID, Amazon Resource Name
    -- (ARN), alias name, or alias ARN. Note that you should only provide this
    -- parameter if the key is different from the default DynamoDB customer
    -- master key alias\/aws\/dynamodb.
    kmsMasterKeyId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether server-side encryption is done using an AWS managed
    -- CMK or an AWS owned CMK. If enabled (true), server-side encryption type
    -- is set to @KMS@ and an AWS managed CMK is used (AWS KMS charges apply).
    -- If disabled (false) or not specified, server-side encryption is set to
    -- AWS owned CMK.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | Server-side encryption type. The only supported value is:
    --
    -- -   @KMS@ - Server-side encryption that uses AWS Key Management Service.
    --     The key is stored in your account and is managed by AWS KMS (AWS KMS
    --     charges apply).
    sSEType :: Prelude.Maybe SSEType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SSESpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsMasterKeyId', 'sSESpecification_kmsMasterKeyId' - The AWS KMS customer master key (CMK) that should be used for the AWS
-- KMS encryption. To specify a CMK, use its key ID, Amazon Resource Name
-- (ARN), alias name, or alias ARN. Note that you should only provide this
-- parameter if the key is different from the default DynamoDB customer
-- master key alias\/aws\/dynamodb.
--
-- 'enabled', 'sSESpecification_enabled' - Indicates whether server-side encryption is done using an AWS managed
-- CMK or an AWS owned CMK. If enabled (true), server-side encryption type
-- is set to @KMS@ and an AWS managed CMK is used (AWS KMS charges apply).
-- If disabled (false) or not specified, server-side encryption is set to
-- AWS owned CMK.
--
-- 'sSEType', 'sSESpecification_sSEType' - Server-side encryption type. The only supported value is:
--
-- -   @KMS@ - Server-side encryption that uses AWS Key Management Service.
--     The key is stored in your account and is managed by AWS KMS (AWS KMS
--     charges apply).
newSSESpecification ::
  SSESpecification
newSSESpecification =
  SSESpecification'
    { kmsMasterKeyId = Prelude.Nothing,
      enabled = Prelude.Nothing,
      sSEType = Prelude.Nothing
    }

-- | The AWS KMS customer master key (CMK) that should be used for the AWS
-- KMS encryption. To specify a CMK, use its key ID, Amazon Resource Name
-- (ARN), alias name, or alias ARN. Note that you should only provide this
-- parameter if the key is different from the default DynamoDB customer
-- master key alias\/aws\/dynamodb.
sSESpecification_kmsMasterKeyId :: Lens.Lens' SSESpecification (Prelude.Maybe Prelude.Text)
sSESpecification_kmsMasterKeyId = Lens.lens (\SSESpecification' {kmsMasterKeyId} -> kmsMasterKeyId) (\s@SSESpecification' {} a -> s {kmsMasterKeyId = a} :: SSESpecification)

-- | Indicates whether server-side encryption is done using an AWS managed
-- CMK or an AWS owned CMK. If enabled (true), server-side encryption type
-- is set to @KMS@ and an AWS managed CMK is used (AWS KMS charges apply).
-- If disabled (false) or not specified, server-side encryption is set to
-- AWS owned CMK.
sSESpecification_enabled :: Lens.Lens' SSESpecification (Prelude.Maybe Prelude.Bool)
sSESpecification_enabled = Lens.lens (\SSESpecification' {enabled} -> enabled) (\s@SSESpecification' {} a -> s {enabled = a} :: SSESpecification)

-- | Server-side encryption type. The only supported value is:
--
-- -   @KMS@ - Server-side encryption that uses AWS Key Management Service.
--     The key is stored in your account and is managed by AWS KMS (AWS KMS
--     charges apply).
sSESpecification_sSEType :: Lens.Lens' SSESpecification (Prelude.Maybe SSEType)
sSESpecification_sSEType = Lens.lens (\SSESpecification' {sSEType} -> sSEType) (\s@SSESpecification' {} a -> s {sSEType = a} :: SSESpecification)

instance Prelude.Hashable SSESpecification where
  hashWithSalt _salt SSESpecification' {..} =
    _salt `Prelude.hashWithSalt` kmsMasterKeyId
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` sSEType

instance Prelude.NFData SSESpecification where
  rnf SSESpecification' {..} =
    Prelude.rnf kmsMasterKeyId
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf sSEType

instance Core.ToJSON SSESpecification where
  toJSON SSESpecification' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("KMSMasterKeyId" Core..=)
              Prelude.<$> kmsMasterKeyId,
            ("Enabled" Core..=) Prelude.<$> enabled,
            ("SSEType" Core..=) Prelude.<$> sSEType
          ]
      )
