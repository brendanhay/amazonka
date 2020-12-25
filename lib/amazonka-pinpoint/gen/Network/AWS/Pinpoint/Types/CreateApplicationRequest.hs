{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.CreateApplicationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.CreateApplicationRequest
  ( CreateApplicationRequest (..),

    -- * Smart constructor
    mkCreateApplicationRequest,

    -- * Lenses
    carName,
    carTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the display name of an application and the tags to associate with the application.
--
-- /See:/ 'mkCreateApplicationRequest' smart constructor.
data CreateApplicationRequest = CreateApplicationRequest'
  { -- | The display name of the application. This name is displayed as the __Project name__ on the Amazon Pinpoint console.
    name :: Core.Text,
    -- | A string-to-string map of key-value pairs that defines the tags to associate with the application. Each tag consists of a required tag key and an associated tag value.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateApplicationRequest' value with any optional fields omitted.
mkCreateApplicationRequest ::
  -- | 'name'
  Core.Text ->
  CreateApplicationRequest
mkCreateApplicationRequest name =
  CreateApplicationRequest' {name, tags = Core.Nothing}

-- | The display name of the application. This name is displayed as the __Project name__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carName :: Lens.Lens' CreateApplicationRequest Core.Text
carName = Lens.field @"name"
{-# DEPRECATED carName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A string-to-string map of key-value pairs that defines the tags to associate with the application. Each tag consists of a required tag key and an associated tag value.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carTags :: Lens.Lens' CreateApplicationRequest (Core.Maybe (Core.HashMap Core.Text Core.Text))
carTags = Lens.field @"tags"
{-# DEPRECATED carTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateApplicationRequest where
  toJSON CreateApplicationRequest {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("Name" Core..= name), ("tags" Core..=) Core.<$> tags]
      )
