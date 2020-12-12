{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.RedshiftDatabase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.RedshiftDatabase
  ( RedshiftDatabase (..),

    -- * Smart constructor
    mkRedshiftDatabase,

    -- * Lenses
    rdDatabaseName,
    rdClusterIdentifier,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the database details required to connect to an Amazon Redshift database.
--
-- /See:/ 'mkRedshiftDatabase' smart constructor.
data RedshiftDatabase = RedshiftDatabase'
  { databaseName ::
      Lude.Text,
    clusterIdentifier :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RedshiftDatabase' with the minimum fields required to make a request.
--
-- * 'clusterIdentifier' - Undocumented field.
-- * 'databaseName' - Undocumented field.
mkRedshiftDatabase ::
  -- | 'databaseName'
  Lude.Text ->
  -- | 'clusterIdentifier'
  Lude.Text ->
  RedshiftDatabase
mkRedshiftDatabase pDatabaseName_ pClusterIdentifier_ =
  RedshiftDatabase'
    { databaseName = pDatabaseName_,
      clusterIdentifier = pClusterIdentifier_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdDatabaseName :: Lens.Lens' RedshiftDatabase Lude.Text
rdDatabaseName = Lens.lens (databaseName :: RedshiftDatabase -> Lude.Text) (\s a -> s {databaseName = a} :: RedshiftDatabase)
{-# DEPRECATED rdDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdClusterIdentifier :: Lens.Lens' RedshiftDatabase Lude.Text
rdClusterIdentifier = Lens.lens (clusterIdentifier :: RedshiftDatabase -> Lude.Text) (\s a -> s {clusterIdentifier = a} :: RedshiftDatabase)
{-# DEPRECATED rdClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

instance Lude.FromJSON RedshiftDatabase where
  parseJSON =
    Lude.withObject
      "RedshiftDatabase"
      ( \x ->
          RedshiftDatabase'
            Lude.<$> (x Lude..: "DatabaseName")
            Lude.<*> (x Lude..: "ClusterIdentifier")
      )

instance Lude.ToJSON RedshiftDatabase where
  toJSON RedshiftDatabase' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DatabaseName" Lude..= databaseName),
            Lude.Just ("ClusterIdentifier" Lude..= clusterIdentifier)
          ]
      )
