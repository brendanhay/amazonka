{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.Types.ToolchainSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeStar.Types.ToolchainSource
  ( ToolchainSource (..)
  -- * Smart constructor
  , mkToolchainSource
  -- * Lenses
  , tsS3
  ) where

import qualified Network.AWS.CodeStar.Types.S3Location as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The Amazon S3 location where the toolchain template file provided with the project request is stored. AWS CodeStar retrieves the file during project creation.
--
-- /See:/ 'mkToolchainSource' smart constructor.
newtype ToolchainSource = ToolchainSource'
  { s3 :: Types.S3Location
    -- ^ The Amazon S3 bucket where the toolchain template file provided with the project request is stored.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ToolchainSource' value with any optional fields omitted.
mkToolchainSource
    :: Types.S3Location -- ^ 's3'
    -> ToolchainSource
mkToolchainSource s3 = ToolchainSource'{s3}

-- | The Amazon S3 bucket where the toolchain template file provided with the project request is stored.
--
-- /Note:/ Consider using 's3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsS3 :: Lens.Lens' ToolchainSource Types.S3Location
tsS3 = Lens.field @"s3"
{-# INLINEABLE tsS3 #-}
{-# DEPRECATED s3 "Use generic-lens or generic-optics with 's3' instead"  #-}

instance Core.FromJSON ToolchainSource where
        toJSON ToolchainSource{..}
          = Core.object (Core.catMaybes [Core.Just ("s3" Core..= s3)])
