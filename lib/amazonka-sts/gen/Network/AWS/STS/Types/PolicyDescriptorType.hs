{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.STS.Types.PolicyDescriptorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.STS.Types.PolicyDescriptorType
  ( PolicyDescriptorType (..),

    -- * Smart constructor
    mkPolicyDescriptorType,

    -- * Lenses
    pdtArn,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A reference to the IAM managed policy that is passed as a session policy for a role session or a federated user session.
--
-- /See:/ 'mkPolicyDescriptorType' smart constructor.
newtype PolicyDescriptorType = PolicyDescriptorType'
  { -- | The Amazon Resource Name (ARN) of the IAM managed policy to use as a session policy for the role. For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
    arn :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PolicyDescriptorType' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the IAM managed policy to use as a session policy for the role. For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
mkPolicyDescriptorType ::
  PolicyDescriptorType
mkPolicyDescriptorType = PolicyDescriptorType' {arn = Lude.Nothing}

-- | The Amazon Resource Name (ARN) of the IAM managed policy to use as a session policy for the role. For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdtArn :: Lens.Lens' PolicyDescriptorType (Lude.Maybe Lude.Text)
pdtArn = Lens.lens (arn :: PolicyDescriptorType -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: PolicyDescriptorType)
{-# DEPRECATED pdtArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.ToQuery PolicyDescriptorType where
  toQuery PolicyDescriptorType' {..} =
    Lude.mconcat ["arn" Lude.=: arn]
