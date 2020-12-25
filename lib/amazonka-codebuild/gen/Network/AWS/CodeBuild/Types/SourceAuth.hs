{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.SourceAuth
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.SourceAuth
  ( SourceAuth (..),

    -- * Smart constructor
    mkSourceAuth,

    -- * Lenses
    saType,
    saResource,
  )
where

import qualified Network.AWS.CodeBuild.Types.SourceAuthType as Types
import qualified Network.AWS.CodeBuild.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the authorization settings for AWS CodeBuild to access the source code to be built.
--
-- This information is for the AWS CodeBuild console's use only. Your code should not get or set this information directly.
--
-- /See:/ 'mkSourceAuth' smart constructor.
data SourceAuth = SourceAuth'
  { -- | The authorization type to use. The only valid value is @OAUTH@ , which represents the OAuth authorization type.
    type' :: Types.SourceAuthType,
    -- | The resource value that applies to the specified authorization type.
    resource :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SourceAuth' value with any optional fields omitted.
mkSourceAuth ::
  -- | 'type\''
  Types.SourceAuthType ->
  SourceAuth
mkSourceAuth type' = SourceAuth' {type', resource = Core.Nothing}

-- | The authorization type to use. The only valid value is @OAUTH@ , which represents the OAuth authorization type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saType :: Lens.Lens' SourceAuth Types.SourceAuthType
saType = Lens.field @"type'"
{-# DEPRECATED saType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The resource value that applies to the specified authorization type.
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saResource :: Lens.Lens' SourceAuth (Core.Maybe Types.String)
saResource = Lens.field @"resource"
{-# DEPRECATED saResource "Use generic-lens or generic-optics with 'resource' instead." #-}

instance Core.FromJSON SourceAuth where
  toJSON SourceAuth {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("type" Core..= type'),
            ("resource" Core..=) Core.<$> resource
          ]
      )

instance Core.FromJSON SourceAuth where
  parseJSON =
    Core.withObject "SourceAuth" Core.$
      \x ->
        SourceAuth'
          Core.<$> (x Core..: "type") Core.<*> (x Core..:? "resource")
