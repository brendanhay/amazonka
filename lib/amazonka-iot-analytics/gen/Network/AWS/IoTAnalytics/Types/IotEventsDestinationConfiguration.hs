{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.IotEventsDestinationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.IotEventsDestinationConfiguration
  ( IotEventsDestinationConfiguration (..)
  -- * Smart constructor
  , mkIotEventsDestinationConfiguration
  -- * Lenses
  , iedcInputName
  , iedcRoleArn
  ) where

import qualified Network.AWS.IoTAnalytics.Types.IotEventsInputName as Types
import qualified Network.AWS.IoTAnalytics.Types.RoleArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Configuration information for delivery of dataset contents to AWS IoT Events.
--
-- /See:/ 'mkIotEventsDestinationConfiguration' smart constructor.
data IotEventsDestinationConfiguration = IotEventsDestinationConfiguration'
  { inputName :: Types.IotEventsInputName
    -- ^ The name of the AWS IoT Events input to which dataset contents are delivered.
  , roleArn :: Types.RoleArn
    -- ^ The ARN of the role that grants AWS IoT Analytics permission to deliver dataset contents to an AWS IoT Events input.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IotEventsDestinationConfiguration' value with any optional fields omitted.
mkIotEventsDestinationConfiguration
    :: Types.IotEventsInputName -- ^ 'inputName'
    -> Types.RoleArn -- ^ 'roleArn'
    -> IotEventsDestinationConfiguration
mkIotEventsDestinationConfiguration inputName roleArn
  = IotEventsDestinationConfiguration'{inputName, roleArn}

-- | The name of the AWS IoT Events input to which dataset contents are delivered.
--
-- /Note:/ Consider using 'inputName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iedcInputName :: Lens.Lens' IotEventsDestinationConfiguration Types.IotEventsInputName
iedcInputName = Lens.field @"inputName"
{-# INLINEABLE iedcInputName #-}
{-# DEPRECATED inputName "Use generic-lens or generic-optics with 'inputName' instead"  #-}

-- | The ARN of the role that grants AWS IoT Analytics permission to deliver dataset contents to an AWS IoT Events input.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iedcRoleArn :: Lens.Lens' IotEventsDestinationConfiguration Types.RoleArn
iedcRoleArn = Lens.field @"roleArn"
{-# INLINEABLE iedcRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

instance Core.FromJSON IotEventsDestinationConfiguration where
        toJSON IotEventsDestinationConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("inputName" Core..= inputName),
                  Core.Just ("roleArn" Core..= roleArn)])

instance Core.FromJSON IotEventsDestinationConfiguration where
        parseJSON
          = Core.withObject "IotEventsDestinationConfiguration" Core.$
              \ x ->
                IotEventsDestinationConfiguration' Core.<$>
                  (x Core..: "inputName") Core.<*> x Core..: "roleArn"
