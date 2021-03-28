{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.Types.RollbackTrigger
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServerlessApplicationRepository.Types.RollbackTrigger
  ( RollbackTrigger (..)
  -- * Smart constructor
  , mkRollbackTrigger
  -- * Lenses
  , rtType
  , rtArn
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | This property corresponds to the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/RollbackTrigger RollbackTrigger> / Data Type.
--
-- /See:/ 'mkRollbackTrigger' smart constructor.
data RollbackTrigger = RollbackTrigger'
  { type' :: Core.Text
    -- ^ This property corresponds to the content of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/RollbackTrigger RollbackTrigger> / Data Type.
  , arn :: Core.Text
    -- ^ This property corresponds to the content of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/RollbackTrigger RollbackTrigger> / Data Type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RollbackTrigger' value with any optional fields omitted.
mkRollbackTrigger
    :: Core.Text -- ^ 'type\''
    -> Core.Text -- ^ 'arn'
    -> RollbackTrigger
mkRollbackTrigger type' arn = RollbackTrigger'{type', arn}

-- | This property corresponds to the content of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/RollbackTrigger RollbackTrigger> / Data Type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtType :: Lens.Lens' RollbackTrigger Core.Text
rtType = Lens.field @"type'"
{-# INLINEABLE rtType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | This property corresponds to the content of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/RollbackTrigger RollbackTrigger> / Data Type.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtArn :: Lens.Lens' RollbackTrigger Core.Text
rtArn = Lens.field @"arn"
{-# INLINEABLE rtArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

instance Core.FromJSON RollbackTrigger where
        toJSON RollbackTrigger{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("type" Core..= type'), Core.Just ("arn" Core..= arn)])
