{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.ContactFlow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.ContactFlow
  ( ContactFlow (..)
  -- * Smart constructor
  , mkContactFlow
  -- * Lenses
  , cfArn
  , cfContent
  , cfDescription
  , cfId
  , cfName
  , cfTags
  , cfType
  ) where

import qualified Network.AWS.Connect.Types.Arn as Types
import qualified Network.AWS.Connect.Types.ContactFlowDescription as Types
import qualified Network.AWS.Connect.Types.ContactFlowType as Types
import qualified Network.AWS.Connect.Types.Content as Types
import qualified Network.AWS.Connect.Types.Id as Types
import qualified Network.AWS.Connect.Types.Name as Types
import qualified Network.AWS.Connect.Types.TagKey as Types
import qualified Network.AWS.Connect.Types.TagValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a contact flow.
--
-- /See:/ 'mkContactFlow' smart constructor.
data ContactFlow = ContactFlow'
  { arn :: Core.Maybe Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the contact flow.
  , content :: Core.Maybe Types.Content
    -- ^ The content of the contact flow.
  , description :: Core.Maybe Types.ContactFlowDescription
    -- ^ The description of the contact flow.
  , id :: Core.Maybe Types.Id
    -- ^ The identifier of the contact flow.
  , name :: Core.Maybe Types.Name
    -- ^ The name of the contact flow.
  , tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
    -- ^ One or more tags.
  , type' :: Core.Maybe Types.ContactFlowType
    -- ^ The type of the contact flow. For descriptions of the available types, see <https://docs.aws.amazon.com/connect/latest/adminguide/create-contact-flow.html#contact-flow-types Choose a Contact Flow Type> in the /Amazon Connect Administrator Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ContactFlow' value with any optional fields omitted.
mkContactFlow
    :: ContactFlow
mkContactFlow
  = ContactFlow'{arn = Core.Nothing, content = Core.Nothing,
                 description = Core.Nothing, id = Core.Nothing, name = Core.Nothing,
                 tags = Core.Nothing, type' = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the contact flow.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfArn :: Lens.Lens' ContactFlow (Core.Maybe Types.Arn)
cfArn = Lens.field @"arn"
{-# INLINEABLE cfArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The content of the contact flow.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfContent :: Lens.Lens' ContactFlow (Core.Maybe Types.Content)
cfContent = Lens.field @"content"
{-# INLINEABLE cfContent #-}
{-# DEPRECATED content "Use generic-lens or generic-optics with 'content' instead"  #-}

-- | The description of the contact flow.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfDescription :: Lens.Lens' ContactFlow (Core.Maybe Types.ContactFlowDescription)
cfDescription = Lens.field @"description"
{-# INLINEABLE cfDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The identifier of the contact flow.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfId :: Lens.Lens' ContactFlow (Core.Maybe Types.Id)
cfId = Lens.field @"id"
{-# INLINEABLE cfId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The name of the contact flow.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfName :: Lens.Lens' ContactFlow (Core.Maybe Types.Name)
cfName = Lens.field @"name"
{-# INLINEABLE cfName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | One or more tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfTags :: Lens.Lens' ContactFlow (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
cfTags = Lens.field @"tags"
{-# INLINEABLE cfTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The type of the contact flow. For descriptions of the available types, see <https://docs.aws.amazon.com/connect/latest/adminguide/create-contact-flow.html#contact-flow-types Choose a Contact Flow Type> in the /Amazon Connect Administrator Guide/ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfType :: Lens.Lens' ContactFlow (Core.Maybe Types.ContactFlowType)
cfType = Lens.field @"type'"
{-# INLINEABLE cfType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON ContactFlow where
        parseJSON
          = Core.withObject "ContactFlow" Core.$
              \ x ->
                ContactFlow' Core.<$>
                  (x Core..:? "Arn") Core.<*> x Core..:? "Content" Core.<*>
                    x Core..:? "Description"
                    Core.<*> x Core..:? "Id"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "Tags"
                    Core.<*> x Core..:? "Type"
