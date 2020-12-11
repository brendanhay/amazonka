-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.Types.RollbackTrigger
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServerlessApplicationRepository.Types.RollbackTrigger
  ( RollbackTrigger (..),

    -- * Smart constructor
    mkRollbackTrigger,

    -- * Lenses
    rtType,
    rtARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | This property corresponds to the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/RollbackTrigger RollbackTrigger> / Data Type.
--
-- /See:/ 'mkRollbackTrigger' smart constructor.
data RollbackTrigger = RollbackTrigger'
  { type' :: Lude.Text,
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

-- | Creates a value of 'RollbackTrigger' with the minimum fields required to make a request.
--
-- * 'arn' - This property corresponds to the content of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/RollbackTrigger RollbackTrigger> / Data Type.
-- * 'type'' - This property corresponds to the content of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/RollbackTrigger RollbackTrigger> / Data Type.
mkRollbackTrigger ::
  -- | 'type''
  Lude.Text ->
  -- | 'arn'
  Lude.Text ->
  RollbackTrigger
mkRollbackTrigger pType_ pARN_ =
  RollbackTrigger' {type' = pType_, arn = pARN_}

-- | This property corresponds to the content of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/RollbackTrigger RollbackTrigger> / Data Type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtType :: Lens.Lens' RollbackTrigger Lude.Text
rtType = Lens.lens (type' :: RollbackTrigger -> Lude.Text) (\s a -> s {type' = a} :: RollbackTrigger)
{-# DEPRECATED rtType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | This property corresponds to the content of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/RollbackTrigger RollbackTrigger> / Data Type.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtARN :: Lens.Lens' RollbackTrigger Lude.Text
rtARN = Lens.lens (arn :: RollbackTrigger -> Lude.Text) (\s a -> s {arn = a} :: RollbackTrigger)
{-# DEPRECATED rtARN "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.ToJSON RollbackTrigger where
  toJSON RollbackTrigger' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("type" Lude..= type'), Lude.Just ("arn" Lude..= arn)]
      )
