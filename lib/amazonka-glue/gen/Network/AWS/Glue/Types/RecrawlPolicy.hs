{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.RecrawlPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.RecrawlPolicy
  ( RecrawlPolicy (..),

    -- * Smart constructor
    mkRecrawlPolicy,

    -- * Lenses
    rpRecrawlBehavior,
  )
where

import Network.AWS.Glue.Types.RecrawlBehavior
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | When crawling an Amazon S3 data source after the first crawl is complete, specifies whether to crawl the entire dataset again or to crawl only folders that were added since the last crawler run. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/incremental-crawls.html Incremental Crawls in AWS Glue> in the developer guide.
--
-- /See:/ 'mkRecrawlPolicy' smart constructor.
newtype RecrawlPolicy = RecrawlPolicy'
  { recrawlBehavior ::
      Lude.Maybe RecrawlBehavior
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RecrawlPolicy' with the minimum fields required to make a request.
--
-- * 'recrawlBehavior' - Specifies whether to crawl the entire dataset again or to crawl only folders that were added since the last crawler run.
--
-- A value of @CRAWL_EVERYTHING@ specifies crawling the entire dataset again.
-- A value of @CRAWL_NEW_FOLDERS_ONLY@ specifies crawling only folders that were added since the last crawler run.
mkRecrawlPolicy ::
  RecrawlPolicy
mkRecrawlPolicy = RecrawlPolicy' {recrawlBehavior = Lude.Nothing}

-- | Specifies whether to crawl the entire dataset again or to crawl only folders that were added since the last crawler run.
--
-- A value of @CRAWL_EVERYTHING@ specifies crawling the entire dataset again.
-- A value of @CRAWL_NEW_FOLDERS_ONLY@ specifies crawling only folders that were added since the last crawler run.
--
-- /Note:/ Consider using 'recrawlBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpRecrawlBehavior :: Lens.Lens' RecrawlPolicy (Lude.Maybe RecrawlBehavior)
rpRecrawlBehavior = Lens.lens (recrawlBehavior :: RecrawlPolicy -> Lude.Maybe RecrawlBehavior) (\s a -> s {recrawlBehavior = a} :: RecrawlPolicy)
{-# DEPRECATED rpRecrawlBehavior "Use generic-lens or generic-optics with 'recrawlBehavior' instead." #-}

instance Lude.FromJSON RecrawlPolicy where
  parseJSON =
    Lude.withObject
      "RecrawlPolicy"
      (\x -> RecrawlPolicy' Lude.<$> (x Lude..:? "RecrawlBehavior"))

instance Lude.ToJSON RecrawlPolicy where
  toJSON RecrawlPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [("RecrawlBehavior" Lude..=) Lude.<$> recrawlBehavior]
      )
