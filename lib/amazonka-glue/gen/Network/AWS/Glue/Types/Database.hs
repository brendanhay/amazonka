-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Database
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Database
  ( Database (..),

    -- * Smart constructor
    mkDatabase,

    -- * Lenses
    dLocationURI,
    dCatalogId,
    dTargetDatabase,
    dParameters,
    dDescription,
    dCreateTime,
    dCreateTableDefaultPermissions,
    dName,
  )
where

import Network.AWS.Glue.Types.DatabaseIdentifier
import Network.AWS.Glue.Types.PrincipalPermissions
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The @Database@ object represents a logical grouping of tables that might reside in a Hive metastore or an RDBMS.
--
-- /See:/ 'mkDatabase' smart constructor.
data Database = Database'
  { locationURI :: Lude.Maybe Lude.Text,
    catalogId :: Lude.Maybe Lude.Text,
    targetDatabase :: Lude.Maybe DatabaseIdentifier,
    parameters :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    description :: Lude.Maybe Lude.Text,
    createTime :: Lude.Maybe Lude.Timestamp,
    createTableDefaultPermissions :: Lude.Maybe [PrincipalPermissions],
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Database' with the minimum fields required to make a request.
--
-- * 'catalogId' - The ID of the Data Catalog in which the database resides.
-- * 'createTableDefaultPermissions' - Creates a set of default permissions on the table for principals.
-- * 'createTime' - The time at which the metadata database was created in the catalog.
-- * 'description' - A description of the database.
-- * 'locationURI' - The location of the database (for example, an HDFS path).
-- * 'name' - The name of the database. For Hive compatibility, this is folded to lowercase when it is stored.
-- * 'parameters' - These key-value pairs define parameters and properties of the database.
-- * 'targetDatabase' - A @DatabaseIdentifier@ structure that describes a target database for resource linking.
mkDatabase ::
  -- | 'name'
  Lude.Text ->
  Database
mkDatabase pName_ =
  Database'
    { locationURI = Lude.Nothing,
      catalogId = Lude.Nothing,
      targetDatabase = Lude.Nothing,
      parameters = Lude.Nothing,
      description = Lude.Nothing,
      createTime = Lude.Nothing,
      createTableDefaultPermissions = Lude.Nothing,
      name = pName_
    }

-- | The location of the database (for example, an HDFS path).
--
-- /Note:/ Consider using 'locationURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dLocationURI :: Lens.Lens' Database (Lude.Maybe Lude.Text)
dLocationURI = Lens.lens (locationURI :: Database -> Lude.Maybe Lude.Text) (\s a -> s {locationURI = a} :: Database)
{-# DEPRECATED dLocationURI "Use generic-lens or generic-optics with 'locationURI' instead." #-}

-- | The ID of the Data Catalog in which the database resides.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCatalogId :: Lens.Lens' Database (Lude.Maybe Lude.Text)
dCatalogId = Lens.lens (catalogId :: Database -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: Database)
{-# DEPRECATED dCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | A @DatabaseIdentifier@ structure that describes a target database for resource linking.
--
-- /Note:/ Consider using 'targetDatabase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTargetDatabase :: Lens.Lens' Database (Lude.Maybe DatabaseIdentifier)
dTargetDatabase = Lens.lens (targetDatabase :: Database -> Lude.Maybe DatabaseIdentifier) (\s a -> s {targetDatabase = a} :: Database)
{-# DEPRECATED dTargetDatabase "Use generic-lens or generic-optics with 'targetDatabase' instead." #-}

-- | These key-value pairs define parameters and properties of the database.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dParameters :: Lens.Lens' Database (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
dParameters = Lens.lens (parameters :: Database -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {parameters = a} :: Database)
{-# DEPRECATED dParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | A description of the database.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDescription :: Lens.Lens' Database (Lude.Maybe Lude.Text)
dDescription = Lens.lens (description :: Database -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Database)
{-# DEPRECATED dDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The time at which the metadata database was created in the catalog.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCreateTime :: Lens.Lens' Database (Lude.Maybe Lude.Timestamp)
dCreateTime = Lens.lens (createTime :: Database -> Lude.Maybe Lude.Timestamp) (\s a -> s {createTime = a} :: Database)
{-# DEPRECATED dCreateTime "Use generic-lens or generic-optics with 'createTime' instead." #-}

-- | Creates a set of default permissions on the table for principals.
--
-- /Note:/ Consider using 'createTableDefaultPermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCreateTableDefaultPermissions :: Lens.Lens' Database (Lude.Maybe [PrincipalPermissions])
dCreateTableDefaultPermissions = Lens.lens (createTableDefaultPermissions :: Database -> Lude.Maybe [PrincipalPermissions]) (\s a -> s {createTableDefaultPermissions = a} :: Database)
{-# DEPRECATED dCreateTableDefaultPermissions "Use generic-lens or generic-optics with 'createTableDefaultPermissions' instead." #-}

-- | The name of the database. For Hive compatibility, this is folded to lowercase when it is stored.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dName :: Lens.Lens' Database Lude.Text
dName = Lens.lens (name :: Database -> Lude.Text) (\s a -> s {name = a} :: Database)
{-# DEPRECATED dName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON Database where
  parseJSON =
    Lude.withObject
      "Database"
      ( \x ->
          Database'
            Lude.<$> (x Lude..:? "LocationUri")
            Lude.<*> (x Lude..:? "CatalogId")
            Lude.<*> (x Lude..:? "TargetDatabase")
            Lude.<*> (x Lude..:? "Parameters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "CreateTime")
            Lude.<*> (x Lude..:? "CreateTableDefaultPermissions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "Name")
      )
