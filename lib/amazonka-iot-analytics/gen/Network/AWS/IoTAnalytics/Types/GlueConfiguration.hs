{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.GlueConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.GlueConfiguration
  ( GlueConfiguration (..),

    -- * Smart constructor
    mkGlueConfiguration,

    -- * Lenses
    gcTableName,
    gcDatabaseName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configuration information for coordination with AWS Glue, a fully managed extract, transform and load (ETL) service.
--
-- /See:/ 'mkGlueConfiguration' smart constructor.
data GlueConfiguration = GlueConfiguration'
  { tableName :: Lude.Text,
    databaseName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GlueConfiguration' with the minimum fields required to make a request.
--
-- * 'databaseName' - The name of the database in your AWS Glue Data Catalog in which the table is located. An AWS Glue Data Catalog database contains metadata tables.
-- * 'tableName' - The name of the table in your AWS Glue Data Catalog that is used to perform the ETL operations. An AWS Glue Data Catalog table contains partitioned data and descriptions of data sources and targets.
mkGlueConfiguration ::
  -- | 'tableName'
  Lude.Text ->
  -- | 'databaseName'
  Lude.Text ->
  GlueConfiguration
mkGlueConfiguration pTableName_ pDatabaseName_ =
  GlueConfiguration'
    { tableName = pTableName_,
      databaseName = pDatabaseName_
    }

-- | The name of the table in your AWS Glue Data Catalog that is used to perform the ETL operations. An AWS Glue Data Catalog table contains partitioned data and descriptions of data sources and targets.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcTableName :: Lens.Lens' GlueConfiguration Lude.Text
gcTableName = Lens.lens (tableName :: GlueConfiguration -> Lude.Text) (\s a -> s {tableName = a} :: GlueConfiguration)
{-# DEPRECATED gcTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | The name of the database in your AWS Glue Data Catalog in which the table is located. An AWS Glue Data Catalog database contains metadata tables.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcDatabaseName :: Lens.Lens' GlueConfiguration Lude.Text
gcDatabaseName = Lens.lens (databaseName :: GlueConfiguration -> Lude.Text) (\s a -> s {databaseName = a} :: GlueConfiguration)
{-# DEPRECATED gcDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

instance Lude.FromJSON GlueConfiguration where
  parseJSON =
    Lude.withObject
      "GlueConfiguration"
      ( \x ->
          GlueConfiguration'
            Lude.<$> (x Lude..: "tableName") Lude.<*> (x Lude..: "databaseName")
      )

instance Lude.ToJSON GlueConfiguration where
  toJSON GlueConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("tableName" Lude..= tableName),
            Lude.Just ("databaseName" Lude..= databaseName)
          ]
      )
