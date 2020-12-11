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
    sAWSAccountNumber,
    sKeyPairIds,
  )
where

import Network.AWS.CloudFront.Types.KeyPairIds
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A list of AWS accounts and the active CloudFront key pairs in each account that CloudFront can use to verify the signatures of signed URLs and signed cookies.
--
-- /See:/ 'mkSigner' smart constructor.
data Signer = Signer'
  { awsAccountNumber :: Lude.Maybe Lude.Text,
    keyPairIds :: Lude.Maybe KeyPairIds
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Signer' with the minimum fields required to make a request.
--
-- * 'awsAccountNumber' - An AWS account number that contains active CloudFront key pairs that CloudFront can use to verify the signatures of signed URLs and signed cookies. If the AWS account that owns the key pairs is the same account that owns the CloudFront distribution, the value of this field is @self@ .
-- * 'keyPairIds' - A list of CloudFront key pair identifiers.
mkSigner ::
  Signer
mkSigner =
  Signer'
    { awsAccountNumber = Lude.Nothing,
      keyPairIds = Lude.Nothing
    }

-- | An AWS account number that contains active CloudFront key pairs that CloudFront can use to verify the signatures of signed URLs and signed cookies. If the AWS account that owns the key pairs is the same account that owns the CloudFront distribution, the value of this field is @self@ .
--
-- /Note:/ Consider using 'awsAccountNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAWSAccountNumber :: Lens.Lens' Signer (Lude.Maybe Lude.Text)
sAWSAccountNumber = Lens.lens (awsAccountNumber :: Signer -> Lude.Maybe Lude.Text) (\s a -> s {awsAccountNumber = a} :: Signer)
{-# DEPRECATED sAWSAccountNumber "Use generic-lens or generic-optics with 'awsAccountNumber' instead." #-}

-- | A list of CloudFront key pair identifiers.
--
-- /Note:/ Consider using 'keyPairIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sKeyPairIds :: Lens.Lens' Signer (Lude.Maybe KeyPairIds)
sKeyPairIds = Lens.lens (keyPairIds :: Signer -> Lude.Maybe KeyPairIds) (\s a -> s {keyPairIds = a} :: Signer)
{-# DEPRECATED sKeyPairIds "Use generic-lens or generic-optics with 'keyPairIds' instead." #-}

instance Lude.FromXML Signer where
  parseXML x =
    Signer'
      Lude.<$> (x Lude..@? "AwsAccountNumber") Lude.<*> (x Lude..@? "KeyPairIds")
