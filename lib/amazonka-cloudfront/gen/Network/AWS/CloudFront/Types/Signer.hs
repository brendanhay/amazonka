{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.Signer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.Signer
  ( Signer (..),

    -- * Smart constructor
    mkSigner,

    -- * Lenses
    sAwsAccountNumber,
    sKeyPairIds,
  )
where

import qualified Network.AWS.CloudFront.Types.AwsAccountNumber as Types
import qualified Network.AWS.CloudFront.Types.KeyPairIds as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A list of AWS accounts and the active CloudFront key pairs in each account that CloudFront can use to verify the signatures of signed URLs and signed cookies.
--
-- /See:/ 'mkSigner' smart constructor.
data Signer = Signer'
  { -- | An AWS account number that contains active CloudFront key pairs that CloudFront can use to verify the signatures of signed URLs and signed cookies. If the AWS account that owns the key pairs is the same account that owns the CloudFront distribution, the value of this field is @self@ .
    awsAccountNumber :: Core.Maybe Types.AwsAccountNumber,
    -- | A list of CloudFront key pair identifiers.
    keyPairIds :: Core.Maybe Types.KeyPairIds
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Signer' value with any optional fields omitted.
mkSigner ::
  Signer
mkSigner =
  Signer'
    { awsAccountNumber = Core.Nothing,
      keyPairIds = Core.Nothing
    }

-- | An AWS account number that contains active CloudFront key pairs that CloudFront can use to verify the signatures of signed URLs and signed cookies. If the AWS account that owns the key pairs is the same account that owns the CloudFront distribution, the value of this field is @self@ .
--
-- /Note:/ Consider using 'awsAccountNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAwsAccountNumber :: Lens.Lens' Signer (Core.Maybe Types.AwsAccountNumber)
sAwsAccountNumber = Lens.field @"awsAccountNumber"
{-# DEPRECATED sAwsAccountNumber "Use generic-lens or generic-optics with 'awsAccountNumber' instead." #-}

-- | A list of CloudFront key pair identifiers.
--
-- /Note:/ Consider using 'keyPairIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sKeyPairIds :: Lens.Lens' Signer (Core.Maybe Types.KeyPairIds)
sKeyPairIds = Lens.field @"keyPairIds"
{-# DEPRECATED sKeyPairIds "Use generic-lens or generic-optics with 'keyPairIds' instead." #-}

instance Core.FromXML Signer where
  parseXML x =
    Signer'
      Core.<$> (x Core..@? "AwsAccountNumber") Core.<*> (x Core..@? "KeyPairIds")
