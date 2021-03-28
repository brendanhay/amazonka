{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.Types.Toolchain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeStar.Types.Toolchain
  ( Toolchain (..)
  -- * Smart constructor
  , mkToolchain
  -- * Lenses
  , tSource
  , tRoleArn
  , tStackParameters
  ) where

import qualified Network.AWS.CodeStar.Types.RoleArn as Types
import qualified Network.AWS.CodeStar.Types.TemplateParameterKey as Types
import qualified Network.AWS.CodeStar.Types.TemplateParameterValue as Types
import qualified Network.AWS.CodeStar.Types.ToolchainSource as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The toolchain template file provided with the project request. AWS CodeStar uses the template to provision the toolchain stack in AWS CloudFormation.
--
-- /See:/ 'mkToolchain' smart constructor.
data Toolchain = Toolchain'
  { source :: Types.ToolchainSource
    -- ^ The Amazon S3 location where the toolchain template file provided with the project request is stored. AWS CodeStar retrieves the file during project creation.
  , roleArn :: Core.Maybe Types.RoleArn
    -- ^ The service role ARN for AWS CodeStar to use for the toolchain template during stack provisioning.
  , stackParameters :: Core.Maybe (Core.HashMap Types.TemplateParameterKey Types.TemplateParameterValue)
    -- ^ The list of parameter overrides to be passed into the toolchain template during stack provisioning, if any.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Toolchain' value with any optional fields omitted.
mkToolchain
    :: Types.ToolchainSource -- ^ 'source'
    -> Toolchain
mkToolchain source
  = Toolchain'{source, roleArn = Core.Nothing,
               stackParameters = Core.Nothing}

-- | The Amazon S3 location where the toolchain template file provided with the project request is stored. AWS CodeStar retrieves the file during project creation.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tSource :: Lens.Lens' Toolchain Types.ToolchainSource
tSource = Lens.field @"source"
{-# INLINEABLE tSource #-}
{-# DEPRECATED source "Use generic-lens or generic-optics with 'source' instead"  #-}

-- | The service role ARN for AWS CodeStar to use for the toolchain template during stack provisioning.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tRoleArn :: Lens.Lens' Toolchain (Core.Maybe Types.RoleArn)
tRoleArn = Lens.field @"roleArn"
{-# INLINEABLE tRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | The list of parameter overrides to be passed into the toolchain template during stack provisioning, if any.
--
-- /Note:/ Consider using 'stackParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tStackParameters :: Lens.Lens' Toolchain (Core.Maybe (Core.HashMap Types.TemplateParameterKey Types.TemplateParameterValue))
tStackParameters = Lens.field @"stackParameters"
{-# INLINEABLE tStackParameters #-}
{-# DEPRECATED stackParameters "Use generic-lens or generic-optics with 'stackParameters' instead"  #-}

instance Core.FromJSON Toolchain where
        toJSON Toolchain{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("source" Core..= source),
                  ("roleArn" Core..=) Core.<$> roleArn,
                  ("stackParameters" Core..=) Core.<$> stackParameters])
