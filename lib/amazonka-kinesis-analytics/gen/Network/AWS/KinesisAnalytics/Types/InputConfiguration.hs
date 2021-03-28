{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.InputConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisAnalytics.Types.InputConfiguration
  ( InputConfiguration (..)
  -- * Smart constructor
  , mkInputConfiguration
  -- * Lenses
  , icId
  , icInputStartingPositionConfiguration
  ) where

import qualified Network.AWS.KinesisAnalytics.Types.Id as Types
import qualified Network.AWS.KinesisAnalytics.Types.InputStartingPositionConfiguration as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | When you start your application, you provide this configuration, which identifies the input source and the point in the input source at which you want the application to start processing records.
--
-- /See:/ 'mkInputConfiguration' smart constructor.
data InputConfiguration = InputConfiguration'
  { id :: Types.Id
    -- ^ Input source ID. You can get this ID by calling the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation.
  , inputStartingPositionConfiguration :: Types.InputStartingPositionConfiguration
    -- ^ Point at which you want the application to start processing records from the streaming source.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InputConfiguration' value with any optional fields omitted.
mkInputConfiguration
    :: Types.Id -- ^ 'id'
    -> Types.InputStartingPositionConfiguration -- ^ 'inputStartingPositionConfiguration'
    -> InputConfiguration
mkInputConfiguration id inputStartingPositionConfiguration
  = InputConfiguration'{id, inputStartingPositionConfiguration}

-- | Input source ID. You can get this ID by calling the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icId :: Lens.Lens' InputConfiguration Types.Id
icId = Lens.field @"id"
{-# INLINEABLE icId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | Point at which you want the application to start processing records from the streaming source.
--
-- /Note:/ Consider using 'inputStartingPositionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icInputStartingPositionConfiguration :: Lens.Lens' InputConfiguration Types.InputStartingPositionConfiguration
icInputStartingPositionConfiguration = Lens.field @"inputStartingPositionConfiguration"
{-# INLINEABLE icInputStartingPositionConfiguration #-}
{-# DEPRECATED inputStartingPositionConfiguration "Use generic-lens or generic-optics with 'inputStartingPositionConfiguration' instead"  #-}

instance Core.FromJSON InputConfiguration where
        toJSON InputConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Id" Core..= id),
                  Core.Just
                    ("InputStartingPositionConfiguration" Core..=
                       inputStartingPositionConfiguration)])
