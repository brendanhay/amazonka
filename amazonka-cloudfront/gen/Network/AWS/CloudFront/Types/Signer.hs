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
-- Module      : Network.AWS.CloudFront.Types.Signer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.Signer where

import Network.AWS.CloudFront.Types.KeyPairIds
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A list of AWS accounts and the active CloudFront key pairs in each
-- account that CloudFront can use to verify the signatures of signed URLs
-- and signed cookies.
--
-- /See:/ 'newSigner' smart constructor.
data Signer = Signer'
  { -- | An AWS account number that contains active CloudFront key pairs that
    -- CloudFront can use to verify the signatures of signed URLs and signed
    -- cookies. If the AWS account that owns the key pairs is the same account
    -- that owns the CloudFront distribution, the value of this field is
    -- @self@.
    awsAccountNumber :: Core.Maybe Core.Text,
    -- | A list of CloudFront key pair identifiers.
    keyPairIds :: Core.Maybe KeyPairIds
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Signer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountNumber', 'signer_awsAccountNumber' - An AWS account number that contains active CloudFront key pairs that
-- CloudFront can use to verify the signatures of signed URLs and signed
-- cookies. If the AWS account that owns the key pairs is the same account
-- that owns the CloudFront distribution, the value of this field is
-- @self@.
--
-- 'keyPairIds', 'signer_keyPairIds' - A list of CloudFront key pair identifiers.
newSigner ::
  Signer
newSigner =
  Signer'
    { awsAccountNumber = Core.Nothing,
      keyPairIds = Core.Nothing
    }

-- | An AWS account number that contains active CloudFront key pairs that
-- CloudFront can use to verify the signatures of signed URLs and signed
-- cookies. If the AWS account that owns the key pairs is the same account
-- that owns the CloudFront distribution, the value of this field is
-- @self@.
signer_awsAccountNumber :: Lens.Lens' Signer (Core.Maybe Core.Text)
signer_awsAccountNumber = Lens.lens (\Signer' {awsAccountNumber} -> awsAccountNumber) (\s@Signer' {} a -> s {awsAccountNumber = a} :: Signer)

-- | A list of CloudFront key pair identifiers.
signer_keyPairIds :: Lens.Lens' Signer (Core.Maybe KeyPairIds)
signer_keyPairIds = Lens.lens (\Signer' {keyPairIds} -> keyPairIds) (\s@Signer' {} a -> s {keyPairIds = a} :: Signer)

instance Core.FromXML Signer where
  parseXML x =
    Signer'
      Core.<$> (x Core..@? "AwsAccountNumber")
      Core.<*> (x Core..@? "KeyPairIds")

instance Core.Hashable Signer

instance Core.NFData Signer
