{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.S3Target
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.S3Target
  ( S3Target (..),

    -- * Smart constructor
    mkS3Target,

    -- * Lenses
    stPath,
    stConnectionName,
    stExclusions,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies a data store in Amazon Simple Storage Service (Amazon S3).
--
-- /See:/ 'mkS3Target' smart constructor.
data S3Target = S3Target'
  { path :: Lude.Maybe Lude.Text,
    connectionName :: Lude.Maybe Lude.Text,
    exclusions :: Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'S3Target' with the minimum fields required to make a request.
--
-- * 'connectionName' - The name of a connection which allows a job or crawler to access data in Amazon S3 within an Amazon Virtual Private Cloud environment (Amazon VPC).
-- * 'exclusions' - A list of glob patterns used to exclude from the crawl. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/add-crawler.html Catalog Tables with a Crawler> .
-- * 'path' - The path to the Amazon S3 target.
mkS3Target ::
  S3Target
mkS3Target =
  S3Target'
    { path = Lude.Nothing,
      connectionName = Lude.Nothing,
      exclusions = Lude.Nothing
    }

-- | The path to the Amazon S3 target.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stPath :: Lens.Lens' S3Target (Lude.Maybe Lude.Text)
stPath = Lens.lens (path :: S3Target -> Lude.Maybe Lude.Text) (\s a -> s {path = a} :: S3Target)
{-# DEPRECATED stPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The name of a connection which allows a job or crawler to access data in Amazon S3 within an Amazon Virtual Private Cloud environment (Amazon VPC).
--
-- /Note:/ Consider using 'connectionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stConnectionName :: Lens.Lens' S3Target (Lude.Maybe Lude.Text)
stConnectionName = Lens.lens (connectionName :: S3Target -> Lude.Maybe Lude.Text) (\s a -> s {connectionName = a} :: S3Target)
{-# DEPRECATED stConnectionName "Use generic-lens or generic-optics with 'connectionName' instead." #-}

-- | A list of glob patterns used to exclude from the crawl. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/add-crawler.html Catalog Tables with a Crawler> .
--
-- /Note:/ Consider using 'exclusions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stExclusions :: Lens.Lens' S3Target (Lude.Maybe [Lude.Text])
stExclusions = Lens.lens (exclusions :: S3Target -> Lude.Maybe [Lude.Text]) (\s a -> s {exclusions = a} :: S3Target)
{-# DEPRECATED stExclusions "Use generic-lens or generic-optics with 'exclusions' instead." #-}

instance Lude.FromJSON S3Target where
  parseJSON =
    Lude.withObject
      "S3Target"
      ( \x ->
          S3Target'
            Lude.<$> (x Lude..:? "Path")
            Lude.<*> (x Lude..:? "ConnectionName")
            Lude.<*> (x Lude..:? "Exclusions" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON S3Target where
  toJSON S3Target' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Path" Lude..=) Lude.<$> path,
            ("ConnectionName" Lude..=) Lude.<$> connectionName,
            ("Exclusions" Lude..=) Lude.<$> exclusions
          ]
      )
