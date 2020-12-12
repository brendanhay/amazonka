{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.STS.Types.FederatedUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.STS.Types.FederatedUser
  ( FederatedUser (..),

    -- * Smart constructor
    mkFederatedUser,

    -- * Lenses
    fuFederatedUserId,
    fuARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Identifiers for the federated user that is associated with the credentials.
--
-- /See:/ 'mkFederatedUser' smart constructor.
data FederatedUser = FederatedUser'
  { federatedUserId :: Lude.Text,
    arn :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FederatedUser' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN that specifies the federated user that is associated with the credentials. For more information about ARNs and how to use them in policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers> in the /IAM User Guide/ .
-- * 'federatedUserId' - The string that identifies the federated user associated with the credentials, similar to the unique ID of an IAM user.
mkFederatedUser ::
  -- | 'federatedUserId'
  Lude.Text ->
  -- | 'arn'
  Lude.Text ->
  FederatedUser
mkFederatedUser pFederatedUserId_ pARN_ =
  FederatedUser' {federatedUserId = pFederatedUserId_, arn = pARN_}

-- | The string that identifies the federated user associated with the credentials, similar to the unique ID of an IAM user.
--
-- /Note:/ Consider using 'federatedUserId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fuFederatedUserId :: Lens.Lens' FederatedUser Lude.Text
fuFederatedUserId = Lens.lens (federatedUserId :: FederatedUser -> Lude.Text) (\s a -> s {federatedUserId = a} :: FederatedUser)
{-# DEPRECATED fuFederatedUserId "Use generic-lens or generic-optics with 'federatedUserId' instead." #-}

-- | The ARN that specifies the federated user that is associated with the credentials. For more information about ARNs and how to use them in policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fuARN :: Lens.Lens' FederatedUser Lude.Text
fuARN = Lens.lens (arn :: FederatedUser -> Lude.Text) (\s a -> s {arn = a} :: FederatedUser)
{-# DEPRECATED fuARN "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.FromXML FederatedUser where
  parseXML x =
    FederatedUser'
      Lude.<$> (x Lude..@ "FederatedUserId") Lude.<*> (x Lude..@ "Arn")
