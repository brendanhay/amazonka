{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.Types.CodeSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeStar.Types.CodeSource
  ( CodeSource (..)
  -- * Smart constructor
  , mkCodeSource
  -- * Lenses
  , csS3
  ) where

import qualified Network.AWS.CodeStar.Types.S3Location as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The location where the source code files provided with the project request are stored. AWS CodeStar retrieves the files during project creation.
--
-- /See:/ 'mkCodeSource' smart constructor.
newtype CodeSource = CodeSource'
  { s3 :: Types.S3Location
    -- ^ Information about the Amazon S3 location where the source code files provided with the project request are stored. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CodeSource' value with any optional fields omitted.
mkCodeSource
    :: Types.S3Location -- ^ 's3'
    -> CodeSource
mkCodeSource s3 = CodeSource'{s3}

-- | Information about the Amazon S3 location where the source code files provided with the project request are stored. 
--
-- /Note:/ Consider using 's3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csS3 :: Lens.Lens' CodeSource Types.S3Location
csS3 = Lens.field @"s3"
{-# INLINEABLE csS3 #-}
{-# DEPRECATED s3 "Use generic-lens or generic-optics with 's3' instead"  #-}

instance Core.FromJSON CodeSource where
        toJSON CodeSource{..}
          = Core.object (Core.catMaybes [Core.Just ("s3" Core..= s3)])
