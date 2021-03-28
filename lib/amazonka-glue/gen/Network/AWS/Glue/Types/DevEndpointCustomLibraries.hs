{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.DevEndpointCustomLibraries
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.DevEndpointCustomLibraries
  ( DevEndpointCustomLibraries (..)
  -- * Smart constructor
  , mkDevEndpointCustomLibraries
  -- * Lenses
  , declExtraJarsS3Path
  , declExtraPythonLibsS3Path
  ) where

import qualified Network.AWS.Glue.Types.GenericString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Custom libraries to be loaded into a development endpoint.
--
-- /See:/ 'mkDevEndpointCustomLibraries' smart constructor.
data DevEndpointCustomLibraries = DevEndpointCustomLibraries'
  { extraJarsS3Path :: Core.Maybe Types.GenericString
    -- ^ The path to one or more Java @.jar@ files in an S3 bucket that should be loaded in your @DevEndpoint@ .
  , extraPythonLibsS3Path :: Core.Maybe Types.GenericString
    -- ^ The paths to one or more Python libraries in an Amazon Simple Storage Service (Amazon S3) bucket that should be loaded in your @DevEndpoint@ . Multiple values must be complete paths separated by a comma.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DevEndpointCustomLibraries' value with any optional fields omitted.
mkDevEndpointCustomLibraries
    :: DevEndpointCustomLibraries
mkDevEndpointCustomLibraries
  = DevEndpointCustomLibraries'{extraJarsS3Path = Core.Nothing,
                                extraPythonLibsS3Path = Core.Nothing}

-- | The path to one or more Java @.jar@ files in an S3 bucket that should be loaded in your @DevEndpoint@ .
--
-- /Note:/ Consider using 'extraJarsS3Path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
declExtraJarsS3Path :: Lens.Lens' DevEndpointCustomLibraries (Core.Maybe Types.GenericString)
declExtraJarsS3Path = Lens.field @"extraJarsS3Path"
{-# INLINEABLE declExtraJarsS3Path #-}
{-# DEPRECATED extraJarsS3Path "Use generic-lens or generic-optics with 'extraJarsS3Path' instead"  #-}

-- | The paths to one or more Python libraries in an Amazon Simple Storage Service (Amazon S3) bucket that should be loaded in your @DevEndpoint@ . Multiple values must be complete paths separated by a comma.
--
-- /Note:/ Consider using 'extraPythonLibsS3Path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
declExtraPythonLibsS3Path :: Lens.Lens' DevEndpointCustomLibraries (Core.Maybe Types.GenericString)
declExtraPythonLibsS3Path = Lens.field @"extraPythonLibsS3Path"
{-# INLINEABLE declExtraPythonLibsS3Path #-}
{-# DEPRECATED extraPythonLibsS3Path "Use generic-lens or generic-optics with 'extraPythonLibsS3Path' instead"  #-}

instance Core.FromJSON DevEndpointCustomLibraries where
        toJSON DevEndpointCustomLibraries{..}
          = Core.object
              (Core.catMaybes
                 [("ExtraJarsS3Path" Core..=) Core.<$> extraJarsS3Path,
                  ("ExtraPythonLibsS3Path" Core..=) Core.<$> extraPythonLibsS3Path])
