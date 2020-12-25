{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HumanTaskUiSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HumanTaskUiSummary
  ( HumanTaskUiSummary (..),

    -- * Smart constructor
    mkHumanTaskUiSummary,

    -- * Lenses
    htusHumanTaskUiName,
    htusHumanTaskUiArn,
    htusCreationTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.HumanTaskUiArn as Types
import qualified Network.AWS.SageMaker.Types.HumanTaskUiName as Types

-- | Container for human task user interface information.
--
-- /See:/ 'mkHumanTaskUiSummary' smart constructor.
data HumanTaskUiSummary = HumanTaskUiSummary'
  { -- | The name of the human task user interface.
    humanTaskUiName :: Types.HumanTaskUiName,
    -- | The Amazon Resource Name (ARN) of the human task user interface.
    humanTaskUiArn :: Types.HumanTaskUiArn,
    -- | A timestamp when SageMaker created the human task user interface.
    creationTime :: Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'HumanTaskUiSummary' value with any optional fields omitted.
mkHumanTaskUiSummary ::
  -- | 'humanTaskUiName'
  Types.HumanTaskUiName ->
  -- | 'humanTaskUiArn'
  Types.HumanTaskUiArn ->
  -- | 'creationTime'
  Core.NominalDiffTime ->
  HumanTaskUiSummary
mkHumanTaskUiSummary humanTaskUiName humanTaskUiArn creationTime =
  HumanTaskUiSummary'
    { humanTaskUiName,
      humanTaskUiArn,
      creationTime
    }

-- | The name of the human task user interface.
--
-- /Note:/ Consider using 'humanTaskUiName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
htusHumanTaskUiName :: Lens.Lens' HumanTaskUiSummary Types.HumanTaskUiName
htusHumanTaskUiName = Lens.field @"humanTaskUiName"
{-# DEPRECATED htusHumanTaskUiName "Use generic-lens or generic-optics with 'humanTaskUiName' instead." #-}

-- | The Amazon Resource Name (ARN) of the human task user interface.
--
-- /Note:/ Consider using 'humanTaskUiArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
htusHumanTaskUiArn :: Lens.Lens' HumanTaskUiSummary Types.HumanTaskUiArn
htusHumanTaskUiArn = Lens.field @"humanTaskUiArn"
{-# DEPRECATED htusHumanTaskUiArn "Use generic-lens or generic-optics with 'humanTaskUiArn' instead." #-}

-- | A timestamp when SageMaker created the human task user interface.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
htusCreationTime :: Lens.Lens' HumanTaskUiSummary Core.NominalDiffTime
htusCreationTime = Lens.field @"creationTime"
{-# DEPRECATED htusCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

instance Core.FromJSON HumanTaskUiSummary where
  parseJSON =
    Core.withObject "HumanTaskUiSummary" Core.$
      \x ->
        HumanTaskUiSummary'
          Core.<$> (x Core..: "HumanTaskUiName")
          Core.<*> (x Core..: "HumanTaskUiArn")
          Core.<*> (x Core..: "CreationTime")
