{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.RegistryId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.RegistryId
  ( RegistryId (..)
  -- * Smart constructor
  , mkRegistryId
  -- * Lenses
  , riRegistryArn
  , riRegistryName
  ) where

import qualified Network.AWS.Glue.Types.GlueResourceArn as Types
import qualified Network.AWS.Glue.Types.RegistryName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A wrapper structure that may contain the registry name and Amazon Resource Name (ARN).
--
-- /See:/ 'mkRegistryId' smart constructor.
data RegistryId = RegistryId'
  { registryArn :: Core.Maybe Types.GlueResourceArn
    -- ^ Arn of the registry to be updated. One of @RegistryArn@ or @RegistryName@ has to be provided.
  , registryName :: Core.Maybe Types.RegistryName
    -- ^ Name of the registry. Used only for lookup. One of @RegistryArn@ or @RegistryName@ has to be provided. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegistryId' value with any optional fields omitted.
mkRegistryId
    :: RegistryId
mkRegistryId
  = RegistryId'{registryArn = Core.Nothing,
                registryName = Core.Nothing}

-- | Arn of the registry to be updated. One of @RegistryArn@ or @RegistryName@ has to be provided.
--
-- /Note:/ Consider using 'registryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riRegistryArn :: Lens.Lens' RegistryId (Core.Maybe Types.GlueResourceArn)
riRegistryArn = Lens.field @"registryArn"
{-# INLINEABLE riRegistryArn #-}
{-# DEPRECATED registryArn "Use generic-lens or generic-optics with 'registryArn' instead"  #-}

-- | Name of the registry. Used only for lookup. One of @RegistryArn@ or @RegistryName@ has to be provided. 
--
-- /Note:/ Consider using 'registryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riRegistryName :: Lens.Lens' RegistryId (Core.Maybe Types.RegistryName)
riRegistryName = Lens.field @"registryName"
{-# INLINEABLE riRegistryName #-}
{-# DEPRECATED registryName "Use generic-lens or generic-optics with 'registryName' instead"  #-}

instance Core.FromJSON RegistryId where
        toJSON RegistryId{..}
          = Core.object
              (Core.catMaybes
                 [("RegistryArn" Core..=) Core.<$> registryArn,
                  ("RegistryName" Core..=) Core.<$> registryName])
