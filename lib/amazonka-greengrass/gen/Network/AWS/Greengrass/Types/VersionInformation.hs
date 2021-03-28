{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.VersionInformation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Greengrass.Types.VersionInformation
  ( VersionInformation (..)
  -- * Smart constructor
  , mkVersionInformation
  -- * Lenses
  , viArn
  , viCreationTimestamp
  , viId
  , viVersion
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a version.
--
-- /See:/ 'mkVersionInformation' smart constructor.
data VersionInformation = VersionInformation'
  { arn :: Core.Maybe Core.Text
    -- ^ The ARN of the version.
  , creationTimestamp :: Core.Maybe Core.Text
    -- ^ The time, in milliseconds since the epoch, when the version was created.
  , id :: Core.Maybe Core.Text
    -- ^ The ID of the parent definition that the version is associated with.
  , version :: Core.Maybe Core.Text
    -- ^ The ID of the version.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VersionInformation' value with any optional fields omitted.
mkVersionInformation
    :: VersionInformation
mkVersionInformation
  = VersionInformation'{arn = Core.Nothing,
                        creationTimestamp = Core.Nothing, id = Core.Nothing,
                        version = Core.Nothing}

-- | The ARN of the version.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viArn :: Lens.Lens' VersionInformation (Core.Maybe Core.Text)
viArn = Lens.field @"arn"
{-# INLINEABLE viArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The time, in milliseconds since the epoch, when the version was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viCreationTimestamp :: Lens.Lens' VersionInformation (Core.Maybe Core.Text)
viCreationTimestamp = Lens.field @"creationTimestamp"
{-# INLINEABLE viCreationTimestamp #-}
{-# DEPRECATED creationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead"  #-}

-- | The ID of the parent definition that the version is associated with.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viId :: Lens.Lens' VersionInformation (Core.Maybe Core.Text)
viId = Lens.field @"id"
{-# INLINEABLE viId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The ID of the version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
viVersion :: Lens.Lens' VersionInformation (Core.Maybe Core.Text)
viVersion = Lens.field @"version"
{-# INLINEABLE viVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.FromJSON VersionInformation where
        parseJSON
          = Core.withObject "VersionInformation" Core.$
              \ x ->
                VersionInformation' Core.<$>
                  (x Core..:? "Arn") Core.<*> x Core..:? "CreationTimestamp" Core.<*>
                    x Core..:? "Id"
                    Core.<*> x Core..:? "Version"
