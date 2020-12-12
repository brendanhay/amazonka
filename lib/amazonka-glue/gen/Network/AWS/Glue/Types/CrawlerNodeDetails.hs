{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CrawlerNodeDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CrawlerNodeDetails
  ( CrawlerNodeDetails (..),

    -- * Smart constructor
    mkCrawlerNodeDetails,

    -- * Lenses
    cndCrawls,
  )
where

import Network.AWS.Glue.Types.Crawl
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The details of a Crawler node present in the workflow.
--
-- /See:/ 'mkCrawlerNodeDetails' smart constructor.
newtype CrawlerNodeDetails = CrawlerNodeDetails'
  { crawls ::
      Lude.Maybe [Crawl]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CrawlerNodeDetails' with the minimum fields required to make a request.
--
-- * 'crawls' - A list of crawls represented by the crawl node.
mkCrawlerNodeDetails ::
  CrawlerNodeDetails
mkCrawlerNodeDetails = CrawlerNodeDetails' {crawls = Lude.Nothing}

-- | A list of crawls represented by the crawl node.
--
-- /Note:/ Consider using 'crawls' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cndCrawls :: Lens.Lens' CrawlerNodeDetails (Lude.Maybe [Crawl])
cndCrawls = Lens.lens (crawls :: CrawlerNodeDetails -> Lude.Maybe [Crawl]) (\s a -> s {crawls = a} :: CrawlerNodeDetails)
{-# DEPRECATED cndCrawls "Use generic-lens or generic-optics with 'crawls' instead." #-}

instance Lude.FromJSON CrawlerNodeDetails where
  parseJSON =
    Lude.withObject
      "CrawlerNodeDetails"
      ( \x ->
          CrawlerNodeDetails'
            Lude.<$> (x Lude..:? "Crawls" Lude..!= Lude.mempty)
      )
