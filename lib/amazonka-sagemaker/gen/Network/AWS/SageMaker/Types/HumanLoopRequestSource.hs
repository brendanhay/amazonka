{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HumanLoopRequestSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HumanLoopRequestSource
  ( HumanLoopRequestSource (..),

    -- * Smart constructor
    mkHumanLoopRequestSource,

    -- * Lenses
    hlrsAwsManagedHumanLoopRequestSource,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.AwsManagedHumanLoopRequestSource as Types

-- | Container for configuring the source of human task requests.
--
-- /See:/ 'mkHumanLoopRequestSource' smart constructor.
newtype HumanLoopRequestSource = HumanLoopRequestSource'
  { -- | Specifies whether Amazon Rekognition or Amazon Textract are used as the integration source. The default field settings and JSON parsing rules are different based on the integration source. Valid values:
    awsManagedHumanLoopRequestSource :: Types.AwsManagedHumanLoopRequestSource
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'HumanLoopRequestSource' value with any optional fields omitted.
mkHumanLoopRequestSource ::
  -- | 'awsManagedHumanLoopRequestSource'
  Types.AwsManagedHumanLoopRequestSource ->
  HumanLoopRequestSource
mkHumanLoopRequestSource awsManagedHumanLoopRequestSource =
  HumanLoopRequestSource' {awsManagedHumanLoopRequestSource}

-- | Specifies whether Amazon Rekognition or Amazon Textract are used as the integration source. The default field settings and JSON parsing rules are different based on the integration source. Valid values:
--
-- /Note:/ Consider using 'awsManagedHumanLoopRequestSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hlrsAwsManagedHumanLoopRequestSource :: Lens.Lens' HumanLoopRequestSource Types.AwsManagedHumanLoopRequestSource
hlrsAwsManagedHumanLoopRequestSource = Lens.field @"awsManagedHumanLoopRequestSource"
{-# DEPRECATED hlrsAwsManagedHumanLoopRequestSource "Use generic-lens or generic-optics with 'awsManagedHumanLoopRequestSource' instead." #-}

instance Core.FromJSON HumanLoopRequestSource where
  toJSON HumanLoopRequestSource {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "AwsManagedHumanLoopRequestSource"
                  Core..= awsManagedHumanLoopRequestSource
              )
          ]
      )

instance Core.FromJSON HumanLoopRequestSource where
  parseJSON =
    Core.withObject "HumanLoopRequestSource" Core.$
      \x ->
        HumanLoopRequestSource'
          Core.<$> (x Core..: "AwsManagedHumanLoopRequestSource")
