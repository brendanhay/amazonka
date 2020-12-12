{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.CommandFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.CommandFilter
  ( CommandFilter (..),

    -- * Smart constructor
    mkCommandFilter,

    -- * Lenses
    cfKey,
    cfValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.CommandFilterKey

-- | Describes a command filter.
--
-- /See:/ 'mkCommandFilter' smart constructor.
data CommandFilter = CommandFilter'
  { key :: CommandFilterKey,
    value :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CommandFilter' with the minimum fields required to make a request.
--
-- * 'key' - The name of the filter.
-- * 'value' - The filter value. Valid values for each filter key are as follows:
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
mkCommandFilter ::
  -- | 'key'
  CommandFilterKey ->
  -- | 'value'
  Lude.Text ->
  CommandFilter
mkCommandFilter pKey_ pValue_ =
  CommandFilter' {key = pKey_, value = pValue_}

-- | The name of the filter.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfKey :: Lens.Lens' CommandFilter CommandFilterKey
cfKey = Lens.lens (key :: CommandFilter -> CommandFilterKey) (\s a -> s {key = a} :: CommandFilter)
{-# DEPRECATED cfKey "Use generic-lens or generic-optics with 'key' instead." #-}

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
cfValue :: Lens.Lens' CommandFilter Lude.Text
cfValue = Lens.lens (value :: CommandFilter -> Lude.Text) (\s a -> s {value = a} :: CommandFilter)
{-# DEPRECATED cfValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.ToJSON CommandFilter where
  toJSON CommandFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("key" Lude..= key), Lude.Just ("value" Lude..= value)]
      )
