{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InstanceInformationStringFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.InstanceInformationStringFilter
  ( InstanceInformationStringFilter (..)
  -- * Smart constructor
  , mkInstanceInformationStringFilter
  -- * Lenses
  , iisfKey
  , iisfValues
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.InstanceInformationFilterValue as Types
import qualified Network.AWS.SSM.Types.Key as Types

-- | The filters to describe or get information about your managed instances.
--
-- /See:/ 'mkInstanceInformationStringFilter' smart constructor.
data InstanceInformationStringFilter = InstanceInformationStringFilter'
  { key :: Types.Key
    -- ^ The filter key name to describe your instances. For example:
--
-- "InstanceIds"|"AgentVersion"|"PingStatus"|"PlatformTypes"|"ActivationIds"|"IamRole"|"ResourceType"|"AssociationStatus"|"Tag Key"
  , values :: Core.NonEmpty Types.InstanceInformationFilterValue
    -- ^ The filter values.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceInformationStringFilter' value with any optional fields omitted.
mkInstanceInformationStringFilter
    :: Types.Key -- ^ 'key'
    -> Core.NonEmpty Types.InstanceInformationFilterValue -- ^ 'values'
    -> InstanceInformationStringFilter
mkInstanceInformationStringFilter key values
  = InstanceInformationStringFilter'{key, values}

-- | The filter key name to describe your instances. For example:
--
-- "InstanceIds"|"AgentVersion"|"PingStatus"|"PlatformTypes"|"ActivationIds"|"IamRole"|"ResourceType"|"AssociationStatus"|"Tag Key"
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iisfKey :: Lens.Lens' InstanceInformationStringFilter Types.Key
iisfKey = Lens.field @"key"
{-# INLINEABLE iisfKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The filter values.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iisfValues :: Lens.Lens' InstanceInformationStringFilter (Core.NonEmpty Types.InstanceInformationFilterValue)
iisfValues = Lens.field @"values"
{-# INLINEABLE iisfValues #-}
{-# DEPRECATED values "Use generic-lens or generic-optics with 'values' instead"  #-}

instance Core.FromJSON InstanceInformationStringFilter where
        toJSON InstanceInformationStringFilter{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Key" Core..= key),
                  Core.Just ("Values" Core..= values)])
