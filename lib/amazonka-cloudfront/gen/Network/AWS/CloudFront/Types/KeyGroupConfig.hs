{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.KeyGroupConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.KeyGroupConfig
  ( KeyGroupConfig (..)
  -- * Smart constructor
  , mkKeyGroupConfig
  -- * Lenses
  , kgcName
  , kgcItems
  , kgcComment
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A key group configuration.
--
-- A key group contains a list of public keys that you can use with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html CloudFront signed URLs and signed cookies> .
--
-- /See:/ 'mkKeyGroupConfig' smart constructor.
data KeyGroupConfig = KeyGroupConfig'
  { name :: Core.Text
    -- ^ A name to identify the key group.
  , items :: [Core.Text]
    -- ^ A list of the identifiers of the public keys in the key group.
  , comment :: Core.Maybe Core.Text
    -- ^ A comment to describe the key group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'KeyGroupConfig' value with any optional fields omitted.
mkKeyGroupConfig
    :: Core.Text -- ^ 'name'
    -> KeyGroupConfig
mkKeyGroupConfig name
  = KeyGroupConfig'{name, items = Core.mempty,
                    comment = Core.Nothing}

-- | A name to identify the key group.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kgcName :: Lens.Lens' KeyGroupConfig Core.Text
kgcName = Lens.field @"name"
{-# INLINEABLE kgcName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A list of the identifiers of the public keys in the key group.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kgcItems :: Lens.Lens' KeyGroupConfig [Core.Text]
kgcItems = Lens.field @"items"
{-# INLINEABLE kgcItems #-}
{-# DEPRECATED items "Use generic-lens or generic-optics with 'items' instead"  #-}

-- | A comment to describe the key group.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kgcComment :: Lens.Lens' KeyGroupConfig (Core.Maybe Core.Text)
kgcComment = Lens.field @"comment"
{-# INLINEABLE kgcComment #-}
{-# DEPRECATED comment "Use generic-lens or generic-optics with 'comment' instead"  #-}

instance Core.ToXML KeyGroupConfig where
        toXML KeyGroupConfig{..}
          = Core.toXMLElement "Name" name Core.<>
              Core.toXMLElement "Items" (Core.toXMLList "PublicKey" items)
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "Comment") comment

instance Core.FromXML KeyGroupConfig where
        parseXML x
          = KeyGroupConfig' Core.<$>
              (x Core..@ "Name") Core.<*>
                x Core..@ "Items" Core..@! Core.mempty Core..<@>
                  Core.parseXMLList "PublicKey"
                Core.<*> x Core..@? "Comment"
