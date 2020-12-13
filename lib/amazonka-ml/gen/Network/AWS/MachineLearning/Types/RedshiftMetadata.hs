{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.RedshiftMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.RedshiftMetadata
  ( RedshiftMetadata (..),

    -- * Smart constructor
    mkRedshiftMetadata,

    -- * Lenses
    rSelectSqlQuery,
    rRedshiftDatabase,
    rDatabaseUserName,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types.RedshiftDatabase
import qualified Network.AWS.Prelude as Lude

-- | Describes the @DataSource@ details specific to Amazon Redshift.
--
-- /See:/ 'mkRedshiftMetadata' smart constructor.
data RedshiftMetadata = RedshiftMetadata'
  { -- | The SQL query that is specified during 'CreateDataSourceFromRedshift' . Returns only if @Verbose@ is true in GetDataSourceInput.
    selectSqlQuery :: Lude.Maybe Lude.Text,
    redshiftDatabase :: Lude.Maybe RedshiftDatabase,
    databaseUserName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RedshiftMetadata' with the minimum fields required to make a request.
--
-- * 'selectSqlQuery' - The SQL query that is specified during 'CreateDataSourceFromRedshift' . Returns only if @Verbose@ is true in GetDataSourceInput.
-- * 'redshiftDatabase' -
-- * 'databaseUserName' -
mkRedshiftMetadata ::
  RedshiftMetadata
mkRedshiftMetadata =
  RedshiftMetadata'
    { selectSqlQuery = Lude.Nothing,
      redshiftDatabase = Lude.Nothing,
      databaseUserName = Lude.Nothing
    }

-- | The SQL query that is specified during 'CreateDataSourceFromRedshift' . Returns only if @Verbose@ is true in GetDataSourceInput.
--
-- /Note:/ Consider using 'selectSqlQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rSelectSqlQuery :: Lens.Lens' RedshiftMetadata (Lude.Maybe Lude.Text)
rSelectSqlQuery = Lens.lens (selectSqlQuery :: RedshiftMetadata -> Lude.Maybe Lude.Text) (\s a -> s {selectSqlQuery = a} :: RedshiftMetadata)
{-# DEPRECATED rSelectSqlQuery "Use generic-lens or generic-optics with 'selectSqlQuery' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'redshiftDatabase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rRedshiftDatabase :: Lens.Lens' RedshiftMetadata (Lude.Maybe RedshiftDatabase)
rRedshiftDatabase = Lens.lens (redshiftDatabase :: RedshiftMetadata -> Lude.Maybe RedshiftDatabase) (\s a -> s {redshiftDatabase = a} :: RedshiftMetadata)
{-# DEPRECATED rRedshiftDatabase "Use generic-lens or generic-optics with 'redshiftDatabase' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'databaseUserName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDatabaseUserName :: Lens.Lens' RedshiftMetadata (Lude.Maybe Lude.Text)
rDatabaseUserName = Lens.lens (databaseUserName :: RedshiftMetadata -> Lude.Maybe Lude.Text) (\s a -> s {databaseUserName = a} :: RedshiftMetadata)
{-# DEPRECATED rDatabaseUserName "Use generic-lens or generic-optics with 'databaseUserName' instead." #-}

instance Lude.FromJSON RedshiftMetadata where
  parseJSON =
    Lude.withObject
      "RedshiftMetadata"
      ( \x ->
          RedshiftMetadata'
            Lude.<$> (x Lude..:? "SelectSqlQuery")
            Lude.<*> (x Lude..:? "RedshiftDatabase")
            Lude.<*> (x Lude..:? "DatabaseUserName")
      )
