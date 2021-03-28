{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.Types.Code
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeStar.Types.Code
  ( Code (..)
  -- * Smart constructor
  , mkCode
  -- * Lenses
  , cSource
  , cDestination
  ) where

import qualified Network.AWS.CodeStar.Types.CodeDestination as Types
import qualified Network.AWS.CodeStar.Types.CodeSource as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Location and destination information about the source code files provided with the project request. The source code is uploaded to the new project source repository after project creation.
--
-- /See:/ 'mkCode' smart constructor.
data Code = Code'
  { source :: Types.CodeSource
    -- ^ The location where the source code files provided with the project request are stored. AWS CodeStar retrieves the files during project creation.
  , destination :: Types.CodeDestination
    -- ^ The repository to be created in AWS CodeStar. Valid values are AWS CodeCommit or GitHub. After AWS CodeStar provisions the new repository, the source code files provided with the project request are placed in the repository.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Code' value with any optional fields omitted.
mkCode
    :: Types.CodeSource -- ^ 'source'
    -> Types.CodeDestination -- ^ 'destination'
    -> Code
mkCode source destination = Code'{source, destination}

-- | The location where the source code files provided with the project request are stored. AWS CodeStar retrieves the files during project creation.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSource :: Lens.Lens' Code Types.CodeSource
cSource = Lens.field @"source"
{-# INLINEABLE cSource #-}
{-# DEPRECATED source "Use generic-lens or generic-optics with 'source' instead"  #-}

-- | The repository to be created in AWS CodeStar. Valid values are AWS CodeCommit or GitHub. After AWS CodeStar provisions the new repository, the source code files provided with the project request are placed in the repository.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDestination :: Lens.Lens' Code Types.CodeDestination
cDestination = Lens.field @"destination"
{-# INLINEABLE cDestination #-}
{-# DEPRECATED destination "Use generic-lens or generic-optics with 'destination' instead"  #-}

instance Core.FromJSON Code where
        toJSON Code{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("source" Core..= source),
                  Core.Just ("destination" Core..= destination)])
