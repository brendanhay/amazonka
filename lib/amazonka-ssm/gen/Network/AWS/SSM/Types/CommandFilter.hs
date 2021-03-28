{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.CommandFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.CommandFilter
  ( CommandFilter (..)
  -- * Smart constructor
  , mkCommandFilter
  -- * Lenses
  , cfKey
  , cfValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.CommandFilterKey as Types
import qualified Network.AWS.SSM.Types.CommandFilterValue as Types

-- | Describes a command filter.
--
-- /See:/ 'mkCommandFilter' smart constructor.
data CommandFilter = CommandFilter'
  { key :: Types.CommandFilterKey
    -- ^ The name of the filter.
  , value :: Types.CommandFilterValue
    -- ^ The filter value. Valid values for each filter key are as follows:
--
--
--     * __InvokedAfter__ : Specify a timestamp to limit your results. For example, specify @2018-07-07T00:00:00Z@ to see a list of command executions occurring July 7, 2018, and later.
--
--
--     * __InvokedBefore__ : Specify a timestamp to limit your results. For example, specify @2018-07-07T00:00:00Z@ to see a list of command executions from before July 7, 2018.
--
--
--     * __Status__ : Specify a valid command status to see a list of all command executions with that status. Status values you can specify include:
--
--     * @Pending@ 
--
--
--     * @InProgress@ 
--
--
--     * @Success@ 
--
--
--     * @Cancelled@ 
--
--
--     * @Failed@ 
--
--
--     * @TimedOut@ 
--
--
--     * @Cancelling@ 
--
--
--
--
--     * __DocumentName__ : Specify name of the SSM document for which you want to see command execution results. For example, specify @AWS-RunPatchBaseline@ to see command executions that used this SSM document to perform security patching operations on instances. 
--
--
--     * __ExecutionStage__ : Specify one of the following values:
--
--     * @Executing@ : Returns a list of command executions that are currently still running.
--
--
--     * @Complete@ : Returns a list of command executions that have already completed. 
--
--
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CommandFilter' value with any optional fields omitted.
mkCommandFilter
    :: Types.CommandFilterKey -- ^ 'key'
    -> Types.CommandFilterValue -- ^ 'value'
    -> CommandFilter
mkCommandFilter key value = CommandFilter'{key, value}

-- | The name of the filter.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfKey :: Lens.Lens' CommandFilter Types.CommandFilterKey
cfKey = Lens.field @"key"
{-# INLINEABLE cfKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The filter value. Valid values for each filter key are as follows:
--
--
--     * __InvokedAfter__ : Specify a timestamp to limit your results. For example, specify @2018-07-07T00:00:00Z@ to see a list of command executions occurring July 7, 2018, and later.
--
--
--     * __InvokedBefore__ : Specify a timestamp to limit your results. For example, specify @2018-07-07T00:00:00Z@ to see a list of command executions from before July 7, 2018.
--
--
--     * __Status__ : Specify a valid command status to see a list of all command executions with that status. Status values you can specify include:
--
--     * @Pending@ 
--
--
--     * @InProgress@ 
--
--
--     * @Success@ 
--
--
--     * @Cancelled@ 
--
--
--     * @Failed@ 
--
--
--     * @TimedOut@ 
--
--
--     * @Cancelling@ 
--
--
--
--
--     * __DocumentName__ : Specify name of the SSM document for which you want to see command execution results. For example, specify @AWS-RunPatchBaseline@ to see command executions that used this SSM document to perform security patching operations on instances. 
--
--
--     * __ExecutionStage__ : Specify one of the following values:
--
--     * @Executing@ : Returns a list of command executions that are currently still running.
--
--
--     * @Complete@ : Returns a list of command executions that have already completed. 
--
--
--
--
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfValue :: Lens.Lens' CommandFilter Types.CommandFilterValue
cfValue = Lens.field @"value"
{-# INLINEABLE cfValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON CommandFilter where
        toJSON CommandFilter{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("key" Core..= key), Core.Just ("value" Core..= value)])
