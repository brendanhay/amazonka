{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.MongoDBTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.MongoDBTarget
  ( MongoDBTarget (..),

    -- * Smart constructor
    mkMongoDBTarget,

    -- * Lenses
    mdtPath,
    mdtConnectionName,
    mdtScanAll,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies an Amazon DocumentDB or MongoDB data store to crawl.
--
-- /See:/ 'mkMongoDBTarget' smart constructor.
data MongoDBTarget = MongoDBTarget'
  { -- | The path of the Amazon DocumentDB or MongoDB target (database/collection).
    path :: Lude.Maybe Lude.Text,
    -- | The name of the connection to use to connect to the Amazon DocumentDB or MongoDB target.
    connectionName :: Lude.Maybe Lude.Text,
    -- | Indicates whether to scan all the records, or to sample rows from the table. Scanning all the records can take a long time when the table is not a high throughput table.
    --
    -- A value of @true@ means to scan all records, while a value of @false@ means to sample the records. If no value is specified, the value defaults to @true@ .
    scanAll :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MongoDBTarget' with the minimum fields required to make a request.
--
-- * 'path' - The path of the Amazon DocumentDB or MongoDB target (database/collection).
-- * 'connectionName' - The name of the connection to use to connect to the Amazon DocumentDB or MongoDB target.
-- * 'scanAll' - Indicates whether to scan all the records, or to sample rows from the table. Scanning all the records can take a long time when the table is not a high throughput table.
--
-- A value of @true@ means to scan all records, while a value of @false@ means to sample the records. If no value is specified, the value defaults to @true@ .
mkMongoDBTarget ::
  MongoDBTarget
mkMongoDBTarget =
  MongoDBTarget'
    { path = Lude.Nothing,
      connectionName = Lude.Nothing,
      scanAll = Lude.Nothing
    }

-- | The path of the Amazon DocumentDB or MongoDB target (database/collection).
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdtPath :: Lens.Lens' MongoDBTarget (Lude.Maybe Lude.Text)
mdtPath = Lens.lens (path :: MongoDBTarget -> Lude.Maybe Lude.Text) (\s a -> s {path = a} :: MongoDBTarget)
{-# DEPRECATED mdtPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The name of the connection to use to connect to the Amazon DocumentDB or MongoDB target.
--
-- /Note:/ Consider using 'connectionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdtConnectionName :: Lens.Lens' MongoDBTarget (Lude.Maybe Lude.Text)
mdtConnectionName = Lens.lens (connectionName :: MongoDBTarget -> Lude.Maybe Lude.Text) (\s a -> s {connectionName = a} :: MongoDBTarget)
{-# DEPRECATED mdtConnectionName "Use generic-lens or generic-optics with 'connectionName' instead." #-}

-- | Indicates whether to scan all the records, or to sample rows from the table. Scanning all the records can take a long time when the table is not a high throughput table.
--
-- A value of @true@ means to scan all records, while a value of @false@ means to sample the records. If no value is specified, the value defaults to @true@ .
--
-- /Note:/ Consider using 'scanAll' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdtScanAll :: Lens.Lens' MongoDBTarget (Lude.Maybe Lude.Bool)
mdtScanAll = Lens.lens (scanAll :: MongoDBTarget -> Lude.Maybe Lude.Bool) (\s a -> s {scanAll = a} :: MongoDBTarget)
{-# DEPRECATED mdtScanAll "Use generic-lens or generic-optics with 'scanAll' instead." #-}

instance Lude.FromJSON MongoDBTarget where
  parseJSON =
    Lude.withObject
      "MongoDBTarget"
      ( \x ->
          MongoDBTarget'
            Lude.<$> (x Lude..:? "Path")
            Lude.<*> (x Lude..:? "ConnectionName")
            Lude.<*> (x Lude..:? "ScanAll")
      )

instance Lude.ToJSON MongoDBTarget where
  toJSON MongoDBTarget' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Path" Lude..=) Lude.<$> path,
            ("ConnectionName" Lude..=) Lude.<$> connectionName,
            ("ScanAll" Lude..=) Lude.<$> scanAll
          ]
      )
