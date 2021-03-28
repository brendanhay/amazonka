{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.RawString
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.RawString
  ( RawString (..)
  -- * Smart constructor
  , mkRawString
  -- * Lenses
  , rsContent
  , rsSha256
  ) where

import qualified Network.AWS.CodeDeploy.Types.Content as Types
import qualified Network.AWS.CodeDeploy.Types.Sha256 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A revision for an AWS Lambda deployment that is a YAML-formatted or JSON-formatted string. For AWS Lambda deployments, the revision is the same as the AppSpec file.
--
-- /See:/ 'mkRawString' smart constructor.
data RawString = RawString'
  { content :: Core.Maybe Types.Content
    -- ^ The YAML-formatted or JSON-formatted revision string. It includes information about which Lambda function to update and optional Lambda functions that validate deployment lifecycle events.
  , sha256 :: Core.Maybe Types.Sha256
    -- ^ The SHA256 hash value of the revision content.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RawString' value with any optional fields omitted.
mkRawString
    :: RawString
mkRawString
  = RawString'{content = Core.Nothing, sha256 = Core.Nothing}

-- | The YAML-formatted or JSON-formatted revision string. It includes information about which Lambda function to update and optional Lambda functions that validate deployment lifecycle events.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsContent :: Lens.Lens' RawString (Core.Maybe Types.Content)
rsContent = Lens.field @"content"
{-# INLINEABLE rsContent #-}
{-# DEPRECATED content "Use generic-lens or generic-optics with 'content' instead"  #-}

-- | The SHA256 hash value of the revision content.
--
-- /Note:/ Consider using 'sha256' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsSha256 :: Lens.Lens' RawString (Core.Maybe Types.Sha256)
rsSha256 = Lens.field @"sha256"
{-# INLINEABLE rsSha256 #-}
{-# DEPRECATED sha256 "Use generic-lens or generic-optics with 'sha256' instead"  #-}

instance Core.FromJSON RawString where
        toJSON RawString{..}
          = Core.object
              (Core.catMaybes
                 [("content" Core..=) Core.<$> content,
                  ("sha256" Core..=) Core.<$> sha256])

instance Core.FromJSON RawString where
        parseJSON
          = Core.withObject "RawString" Core.$
              \ x ->
                RawString' Core.<$>
                  (x Core..:? "content") Core.<*> x Core..:? "sha256"
