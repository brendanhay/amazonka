{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.ClientPropertiesResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types.ClientPropertiesResult
  ( ClientPropertiesResult (..)
  -- * Smart constructor
  , mkClientPropertiesResult
  -- * Lenses
  , cprClientProperties
  , cprResourceId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkSpaces.Types.ClientProperties as Types
import qualified Network.AWS.WorkSpaces.Types.ResourceId as Types

-- | Information about the Amazon WorkSpaces client.
--
-- /See:/ 'mkClientPropertiesResult' smart constructor.
data ClientPropertiesResult = ClientPropertiesResult'
  { clientProperties :: Core.Maybe Types.ClientProperties
    -- ^ Information about the Amazon WorkSpaces client.
  , resourceId :: Core.Maybe Types.ResourceId
    -- ^ The resource identifier, in the form of a directory ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ClientPropertiesResult' value with any optional fields omitted.
mkClientPropertiesResult
    :: ClientPropertiesResult
mkClientPropertiesResult
  = ClientPropertiesResult'{clientProperties = Core.Nothing,
                            resourceId = Core.Nothing}

-- | Information about the Amazon WorkSpaces client.
--
-- /Note:/ Consider using 'clientProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprClientProperties :: Lens.Lens' ClientPropertiesResult (Core.Maybe Types.ClientProperties)
cprClientProperties = Lens.field @"clientProperties"
{-# INLINEABLE cprClientProperties #-}
{-# DEPRECATED clientProperties "Use generic-lens or generic-optics with 'clientProperties' instead"  #-}

-- | The resource identifier, in the form of a directory ID.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprResourceId :: Lens.Lens' ClientPropertiesResult (Core.Maybe Types.ResourceId)
cprResourceId = Lens.field @"resourceId"
{-# INLINEABLE cprResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

instance Core.FromJSON ClientPropertiesResult where
        parseJSON
          = Core.withObject "ClientPropertiesResult" Core.$
              \ x ->
                ClientPropertiesResult' Core.<$>
                  (x Core..:? "ClientProperties") Core.<*> x Core..:? "ResourceId"
