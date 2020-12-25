{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.KeyGroupConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.KeyGroupConfig
  ( KeyGroupConfig (..),

    -- * Smart constructor
    mkKeyGroupConfig,

    -- * Lenses
    kgcName,
    kgcItems,
    kgcComment,
  )
where

import qualified Network.AWS.CloudFront.Types.Comment as Types
import qualified Network.AWS.CloudFront.Types.Name as Types
import qualified Network.AWS.CloudFront.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A key group configuration.
--
-- A key group contains a list of public keys that you can use with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html CloudFront signed URLs and signed cookies> .
--
-- /See:/ 'mkKeyGroupConfig' smart constructor.
data KeyGroupConfig = KeyGroupConfig'
  { -- | A name to identify the key group.
    name :: Types.Name,
    -- | A list of the identifiers of the public keys in the key group.
    items :: [Types.String],
    -- | A comment to describe the key group.
    comment :: Core.Maybe Types.Comment
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'KeyGroupConfig' value with any optional fields omitted.
mkKeyGroupConfig ::
  -- | 'name'
  Types.Name ->
  KeyGroupConfig
mkKeyGroupConfig name =
  KeyGroupConfig'
    { name,
      items = Core.mempty,
      comment = Core.Nothing
    }

-- | A name to identify the key group.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kgcName :: Lens.Lens' KeyGroupConfig Types.Name
kgcName = Lens.field @"name"
{-# DEPRECATED kgcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A list of the identifiers of the public keys in the key group.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kgcItems :: Lens.Lens' KeyGroupConfig [Types.String]
kgcItems = Lens.field @"items"
{-# DEPRECATED kgcItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | A comment to describe the key group.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kgcComment :: Lens.Lens' KeyGroupConfig (Core.Maybe Types.Comment)
kgcComment = Lens.field @"comment"
{-# DEPRECATED kgcComment "Use generic-lens or generic-optics with 'comment' instead." #-}

instance Core.ToXML KeyGroupConfig where
  toXML KeyGroupConfig {..} =
    Core.toXMLNode "Name" name
      Core.<> Core.toXMLNode "Items" (Core.toXMLList "PublicKey" items)
      Core.<> Core.toXMLNode "Comment" Core.<$> comment

instance Core.FromXML KeyGroupConfig where
  parseXML x =
    KeyGroupConfig'
      Core.<$> (x Core..@ "Name")
      Core.<*> ( x Core..@? "Items" Core..@! Core.mempty
                   Core..<@> Core.parseXMLList "PublicKey"
               )
      Core.<*> (x Core..@? "Comment")
