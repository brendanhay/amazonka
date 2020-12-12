{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.JdbcTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.JdbcTarget
  ( JdbcTarget (..),

    -- * Smart constructor
    mkJdbcTarget,

    -- * Lenses
    jtPath,
    jtConnectionName,
    jtExclusions,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies a JDBC data store to crawl.
--
-- /See:/ 'mkJdbcTarget' smart constructor.
data JdbcTarget = JdbcTarget'
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

-- | Creates a value of 'JdbcTarget' with the minimum fields required to make a request.
--
-- * 'connectionName' - The name of the connection to use to connect to the JDBC target.
-- * 'exclusions' - A list of glob patterns used to exclude from the crawl. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/add-crawler.html Catalog Tables with a Crawler> .
-- * 'path' - The path of the JDBC target.
mkJdbcTarget ::
  JdbcTarget
mkJdbcTarget =
  JdbcTarget'
    { path = Lude.Nothing,
      connectionName = Lude.Nothing,
      exclusions = Lude.Nothing
    }

-- | The path of the JDBC target.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtPath :: Lens.Lens' JdbcTarget (Lude.Maybe Lude.Text)
jtPath = Lens.lens (path :: JdbcTarget -> Lude.Maybe Lude.Text) (\s a -> s {path = a} :: JdbcTarget)
{-# DEPRECATED jtPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The name of the connection to use to connect to the JDBC target.
--
-- /Note:/ Consider using 'connectionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtConnectionName :: Lens.Lens' JdbcTarget (Lude.Maybe Lude.Text)
jtConnectionName = Lens.lens (connectionName :: JdbcTarget -> Lude.Maybe Lude.Text) (\s a -> s {connectionName = a} :: JdbcTarget)
{-# DEPRECATED jtConnectionName "Use generic-lens or generic-optics with 'connectionName' instead." #-}

-- | A list of glob patterns used to exclude from the crawl. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/add-crawler.html Catalog Tables with a Crawler> .
--
-- /Note:/ Consider using 'exclusions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtExclusions :: Lens.Lens' JdbcTarget (Lude.Maybe [Lude.Text])
jtExclusions = Lens.lens (exclusions :: JdbcTarget -> Lude.Maybe [Lude.Text]) (\s a -> s {exclusions = a} :: JdbcTarget)
{-# DEPRECATED jtExclusions "Use generic-lens or generic-optics with 'exclusions' instead." #-}

instance Lude.FromJSON JdbcTarget where
  parseJSON =
    Lude.withObject
      "JdbcTarget"
      ( \x ->
          JdbcTarget'
            Lude.<$> (x Lude..:? "Path")
            Lude.<*> (x Lude..:? "ConnectionName")
            Lude.<*> (x Lude..:? "Exclusions" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON JdbcTarget where
  toJSON JdbcTarget' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Path" Lude..=) Lude.<$> path,
            ("ConnectionName" Lude..=) Lude.<$> connectionName,
            ("Exclusions" Lude..=) Lude.<$> exclusions
          ]
      )
