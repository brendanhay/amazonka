{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CidrAuthorizationContext
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CidrAuthorizationContext
  ( CidrAuthorizationContext (..),

    -- * Smart constructor
    mkCidrAuthorizationContext,

    -- * Lenses
    cacSignature,
    cacMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides authorization for Amazon to bring a specific IP address range to a specific AWS account using bring your own IP addresses (BYOIP). For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-byoip.html#prepare-for-byoip Prepare to Bring Your Address Range to Your AWS Account> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /See:/ 'mkCidrAuthorizationContext' smart constructor.
data CidrAuthorizationContext = CidrAuthorizationContext'
  { -- | The signed authorization message for the prefix and account.
    signature :: Lude.Text,
    -- | The plain-text authorization message for the prefix and account.
    message :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CidrAuthorizationContext' with the minimum fields required to make a request.
--
-- * 'signature' - The signed authorization message for the prefix and account.
-- * 'message' - The plain-text authorization message for the prefix and account.
mkCidrAuthorizationContext ::
  -- | 'signature'
  Lude.Text ->
  -- | 'message'
  Lude.Text ->
  CidrAuthorizationContext
mkCidrAuthorizationContext pSignature_ pMessage_ =
  CidrAuthorizationContext'
    { signature = pSignature_,
      message = pMessage_
    }

-- | The signed authorization message for the prefix and account.
--
-- /Note:/ Consider using 'signature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacSignature :: Lens.Lens' CidrAuthorizationContext Lude.Text
cacSignature = Lens.lens (signature :: CidrAuthorizationContext -> Lude.Text) (\s a -> s {signature = a} :: CidrAuthorizationContext)
{-# DEPRECATED cacSignature "Use generic-lens or generic-optics with 'signature' instead." #-}

-- | The plain-text authorization message for the prefix and account.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacMessage :: Lens.Lens' CidrAuthorizationContext Lude.Text
cacMessage = Lens.lens (message :: CidrAuthorizationContext -> Lude.Text) (\s a -> s {message = a} :: CidrAuthorizationContext)
{-# DEPRECATED cacMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.ToQuery CidrAuthorizationContext where
  toQuery CidrAuthorizationContext' {..} =
    Lude.mconcat
      ["Signature" Lude.=: signature, "Message" Lude.=: message]
