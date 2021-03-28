{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ApplicationResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.ApplicationResponse
  ( ApplicationResponse (..)
  -- * Smart constructor
  , mkApplicationResponse
  -- * Lenses
  , arId
  , arArn
  , arName
  , arTags
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information about an application.
--
-- /See:/ 'mkApplicationResponse' smart constructor.
data ApplicationResponse = ApplicationResponse'
  { id :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  , arn :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of the application.
  , name :: Core.Text
    -- ^ The display name of the application. This name is displayed as the __Project name__ on the Amazon Pinpoint console.
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ A string-to-string map of key-value pairs that identifies the tags that are associated with the application. Each tag consists of a required tag key and an associated tag value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ApplicationResponse' value with any optional fields omitted.
mkApplicationResponse
    :: Core.Text -- ^ 'id'
    -> Core.Text -- ^ 'arn'
    -> Core.Text -- ^ 'name'
    -> ApplicationResponse
mkApplicationResponse id arn name
  = ApplicationResponse'{id, arn, name, tags = Core.Nothing}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arId :: Lens.Lens' ApplicationResponse Core.Text
arId = Lens.field @"id"
{-# INLINEABLE arId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The Amazon Resource Name (ARN) of the application.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arArn :: Lens.Lens' ApplicationResponse Core.Text
arArn = Lens.field @"arn"
{-# INLINEABLE arArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The display name of the application. This name is displayed as the __Project name__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arName :: Lens.Lens' ApplicationResponse Core.Text
arName = Lens.field @"name"
{-# INLINEABLE arName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A string-to-string map of key-value pairs that identifies the tags that are associated with the application. Each tag consists of a required tag key and an associated tag value.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arTags :: Lens.Lens' ApplicationResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
arTags = Lens.field @"tags"
{-# INLINEABLE arTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromJSON ApplicationResponse where
        parseJSON
          = Core.withObject "ApplicationResponse" Core.$
              \ x ->
                ApplicationResponse' Core.<$>
                  (x Core..: "Id") Core.<*> x Core..: "Arn" Core.<*> x Core..: "Name"
                    Core.<*> x Core..:? "tags"
