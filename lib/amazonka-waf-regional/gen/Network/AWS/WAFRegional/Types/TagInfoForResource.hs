-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.TagInfoForResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.TagInfoForResource
  ( TagInfoForResource (..),

    -- * Smart constructor
    mkTagInfoForResource,

    -- * Lenses
    tifrTagList,
    tifrResourceARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WAFRegional.Types.Tag

-- | Information for a tag associated with an AWS resource. Tags are key:value pairs that you can use to categorize and manage your resources, for purposes like billing. For example, you might set the tag key to "customer" and the value to the customer name or ID. You can specify one or more tags to add to each AWS resource, up to 50 tags for a resource.
--
-- Tagging is only available through the API, SDKs, and CLI. You can't manage or view tags through the AWS WAF Classic console. You can tag the AWS resources that you manage through AWS WAF Classic: web ACLs, rule groups, and rules.
--
-- /See:/ 'mkTagInfoForResource' smart constructor.
data TagInfoForResource = TagInfoForResource'
  { tagList ::
      Lude.Maybe (Lude.NonEmpty Tag),
    resourceARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TagInfoForResource' with the minimum fields required to make a request.
--
-- * 'resourceARN' -
-- * 'tagList' -
mkTagInfoForResource ::
  TagInfoForResource
mkTagInfoForResource =
  TagInfoForResource'
    { tagList = Lude.Nothing,
      resourceARN = Lude.Nothing
    }

-- |
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tifrTagList :: Lens.Lens' TagInfoForResource (Lude.Maybe (Lude.NonEmpty Tag))
tifrTagList = Lens.lens (tagList :: TagInfoForResource -> Lude.Maybe (Lude.NonEmpty Tag)) (\s a -> s {tagList = a} :: TagInfoForResource)
{-# DEPRECATED tifrTagList "Use generic-lens or generic-optics with 'tagList' instead." #-}

-- |
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tifrResourceARN :: Lens.Lens' TagInfoForResource (Lude.Maybe Lude.Text)
tifrResourceARN = Lens.lens (resourceARN :: TagInfoForResource -> Lude.Maybe Lude.Text) (\s a -> s {resourceARN = a} :: TagInfoForResource)
{-# DEPRECATED tifrResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

instance Lude.FromJSON TagInfoForResource where
  parseJSON =
    Lude.withObject
      "TagInfoForResource"
      ( \x ->
          TagInfoForResource'
            Lude.<$> (x Lude..:? "TagList") Lude.<*> (x Lude..:? "ResourceARN")
      )
