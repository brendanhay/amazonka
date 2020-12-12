{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.Types.RollbackConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServerlessApplicationRepository.Types.RollbackConfiguration
  ( RollbackConfiguration (..),

    -- * Smart constructor
    mkRollbackConfiguration,

    -- * Lenses
    rcRollbackTriggers,
    rcMonitoringTimeInMinutes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.ServerlessApplicationRepository.Types.RollbackTrigger

-- | This property corresponds to the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/RollbackConfiguration RollbackConfiguration> / Data Type.
--
-- /See:/ 'mkRollbackConfiguration' smart constructor.
data RollbackConfiguration = RollbackConfiguration'
  { rollbackTriggers ::
      Lude.Maybe [RollbackTrigger],
    monitoringTimeInMinutes :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RollbackConfiguration' with the minimum fields required to make a request.
--
-- * 'monitoringTimeInMinutes' - This property corresponds to the content of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/RollbackConfiguration RollbackConfiguration> / Data Type.
-- * 'rollbackTriggers' - This property corresponds to the content of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/RollbackConfiguration RollbackConfiguration> / Data Type.
mkRollbackConfiguration ::
  RollbackConfiguration
mkRollbackConfiguration =
  RollbackConfiguration'
    { rollbackTriggers = Lude.Nothing,
      monitoringTimeInMinutes = Lude.Nothing
    }

-- | This property corresponds to the content of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/RollbackConfiguration RollbackConfiguration> / Data Type.
--
-- /Note:/ Consider using 'rollbackTriggers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcRollbackTriggers :: Lens.Lens' RollbackConfiguration (Lude.Maybe [RollbackTrigger])
rcRollbackTriggers = Lens.lens (rollbackTriggers :: RollbackConfiguration -> Lude.Maybe [RollbackTrigger]) (\s a -> s {rollbackTriggers = a} :: RollbackConfiguration)
{-# DEPRECATED rcRollbackTriggers "Use generic-lens or generic-optics with 'rollbackTriggers' instead." #-}

-- | This property corresponds to the content of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/RollbackConfiguration RollbackConfiguration> / Data Type.
--
-- /Note:/ Consider using 'monitoringTimeInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcMonitoringTimeInMinutes :: Lens.Lens' RollbackConfiguration (Lude.Maybe Lude.Int)
rcMonitoringTimeInMinutes = Lens.lens (monitoringTimeInMinutes :: RollbackConfiguration -> Lude.Maybe Lude.Int) (\s a -> s {monitoringTimeInMinutes = a} :: RollbackConfiguration)
{-# DEPRECATED rcMonitoringTimeInMinutes "Use generic-lens or generic-optics with 'monitoringTimeInMinutes' instead." #-}

instance Lude.ToJSON RollbackConfiguration where
  toJSON RollbackConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("rollbackTriggers" Lude..=) Lude.<$> rollbackTriggers,
            ("monitoringTimeInMinutes" Lude..=)
              Lude.<$> monitoringTimeInMinutes
          ]
      )
