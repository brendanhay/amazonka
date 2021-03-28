{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.STS.Types.PolicyDescriptorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.STS.Types.PolicyDescriptorType
  ( PolicyDescriptorType (..)
  -- * Smart constructor
  , mkPolicyDescriptorType
  -- * Lenses
  , pdtArn
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.STS.Types.ArnType as Types

-- | A reference to the IAM managed policy that is passed as a session policy for a role session or a federated user session.
--
-- /See:/ 'mkPolicyDescriptorType' smart constructor.
newtype PolicyDescriptorType = PolicyDescriptorType'
  { arn :: Core.Maybe Types.ArnType
    -- ^ The Amazon Resource Name (ARN) of the IAM managed policy to use as a session policy for the role. For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PolicyDescriptorType' value with any optional fields omitted.
mkPolicyDescriptorType
    :: PolicyDescriptorType
mkPolicyDescriptorType = PolicyDescriptorType'{arn = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the IAM managed policy to use as a session policy for the role. For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdtArn :: Lens.Lens' PolicyDescriptorType (Core.Maybe Types.ArnType)
pdtArn = Lens.field @"arn"
{-# INLINEABLE pdtArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

instance Core.ToQuery PolicyDescriptorType where
        toQuery PolicyDescriptorType{..}
          = Core.maybe Core.mempty (Core.toQueryPair "arn") arn
