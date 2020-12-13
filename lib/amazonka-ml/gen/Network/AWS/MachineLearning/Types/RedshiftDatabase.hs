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
    rClusterIdentifier,
    rDatabaseName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the database details required to connect to an Amazon Redshift database.
--
-- /See:/ 'mkRedshiftDatabase' smart constructor.
data RedshiftDatabase = RedshiftDatabase'
  { clusterIdentifier :: Lude.Text,
    databaseName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RedshiftDatabase' with the minimum fields required to make a request.
--
-- * 'clusterIdentifier' -
-- * 'databaseName' -
mkRedshiftDatabase ::
  -- | 'clusterIdentifier'
  Lude.Text ->
  -- | 'databaseName'
  Lude.Text ->
  RedshiftDatabase
mkRedshiftDatabase pClusterIdentifier_ pDatabaseName_ =
  RedshiftDatabase'
    { clusterIdentifier = pClusterIdentifier_,
      databaseName = pDatabaseName_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rClusterIdentifier :: Lens.Lens' RedshiftDatabase Lude.Text
rClusterIdentifier = Lens.lens (clusterIdentifier :: RedshiftDatabase -> Lude.Text) (\s a -> s {clusterIdentifier = a} :: RedshiftDatabase)
{-# DEPRECATED rClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDatabaseName :: Lens.Lens' RedshiftDatabase Lude.Text
rDatabaseName = Lens.lens (databaseName :: RedshiftDatabase -> Lude.Text) (\s a -> s {databaseName = a} :: RedshiftDatabase)
{-# DEPRECATED rDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

instance Lude.FromJSON RedshiftDatabase where
  parseJSON =
    Lude.withObject
      "RedshiftDatabase"
      ( \x ->
          RedshiftDatabase'
            Lude.<$> (x Lude..: "ClusterIdentifier")
            Lude.<*> (x Lude..: "DatabaseName")
      )

instance Lude.ToJSON RedshiftDatabase where
  toJSON RedshiftDatabase' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ClusterIdentifier" Lude..= clusterIdentifier),
            Lude.Just ("DatabaseName" Lude..= databaseName)
          ]
      )
