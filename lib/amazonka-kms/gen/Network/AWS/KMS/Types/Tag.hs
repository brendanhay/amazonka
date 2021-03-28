{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.Tag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KMS.Types.Tag
  ( Tag (..)
  -- * Smart constructor
  , mkTag
  -- * Lenses
  , tTagKey
  , tTagValue
  ) where

import qualified Network.AWS.KMS.Types.TagKey as Types
import qualified Network.AWS.KMS.Types.TagValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A key-value pair. A tag consists of a tag key and a tag value. Tag keys and tag values are both required, but tag values can be empty (null) strings.
--
-- For information about the rules that apply to tag keys and tag values, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/allocation-tag-restrictions.html User-Defined Tag Restrictions> in the /AWS Billing and Cost Management User Guide/ .
--
-- /See:/ 'mkTag' smart constructor.
data Tag = Tag'
  { tagKey :: Types.TagKey
    -- ^ The key of the tag.
  , tagValue :: Types.TagValue
    -- ^ The value of the tag.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Tag' value with any optional fields omitted.
mkTag
    :: Types.TagKey -- ^ 'tagKey'
    -> Types.TagValue -- ^ 'tagValue'
    -> Tag
mkTag tagKey tagValue = Tag'{tagKey, tagValue}

-- | The key of the tag.
--
-- /Note:/ Consider using 'tagKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTagKey :: Lens.Lens' Tag Types.TagKey
tTagKey = Lens.field @"tagKey"
{-# INLINEABLE tTagKey #-}
{-# DEPRECATED tagKey "Use generic-lens or generic-optics with 'tagKey' instead"  #-}

-- | The value of the tag.
--
-- /Note:/ Consider using 'tagValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTagValue :: Lens.Lens' Tag Types.TagValue
tTagValue = Lens.field @"tagValue"
{-# INLINEABLE tTagValue #-}
{-# DEPRECATED tagValue "Use generic-lens or generic-optics with 'tagValue' instead"  #-}

instance Core.FromJSON Tag where
        toJSON Tag{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("TagKey" Core..= tagKey),
                  Core.Just ("TagValue" Core..= tagValue)])

instance Core.FromJSON Tag where
        parseJSON
          = Core.withObject "Tag" Core.$
              \ x ->
                Tag' Core.<$> (x Core..: "TagKey") Core.<*> x Core..: "TagValue"
