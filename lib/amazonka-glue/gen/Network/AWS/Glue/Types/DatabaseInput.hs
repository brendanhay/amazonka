{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.DatabaseInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.DatabaseInput
  ( DatabaseInput (..),

    -- * Smart constructor
    mkDatabaseInput,

    -- * Lenses
    diLocationURI,
    diTargetDatabase,
    diParameters,
    diDescription,
    diCreateTableDefaultPermissions,
    diName,
  )
where

import Network.AWS.Glue.Types.DatabaseIdentifier
import Network.AWS.Glue.Types.PrincipalPermissions
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The structure used to create or update a database.
--
-- /See:/ 'mkDatabaseInput' smart constructor.
data DatabaseInput = DatabaseInput'
  { locationURI ::
      Lude.Maybe Lude.Text,
    targetDatabase :: Lude.Maybe DatabaseIdentifier,
    parameters :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    description :: Lude.Maybe Lude.Text,
    createTableDefaultPermissions ::
      Lude.Maybe [PrincipalPermissions],
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

-- | Creates a value of 'DatabaseInput' with the minimum fields required to make a request.
--
-- * 'createTableDefaultPermissions' - Creates a set of default permissions on the table for principals.
-- * 'description' - A description of the database.
-- * 'locationURI' - The location of the database (for example, an HDFS path).
-- * 'name' - The name of the database. For Hive compatibility, this is folded to lowercase when it is stored.
-- * 'parameters' - These key-value pairs define parameters and properties of the database.
--
-- These key-value pairs define parameters and properties of the database.
-- * 'targetDatabase' - A @DatabaseIdentifier@ structure that describes a target database for resource linking.
mkDatabaseInput ::
  -- | 'name'
  Lude.Text ->
  DatabaseInput
mkDatabaseInput pName_ =
  DatabaseInput'
    { locationURI = Lude.Nothing,
      targetDatabase = Lude.Nothing,
      parameters = Lude.Nothing,
      description = Lude.Nothing,
      createTableDefaultPermissions = Lude.Nothing,
      name = pName_
    }

-- | The location of the database (for example, an HDFS path).
--
-- /Note:/ Consider using 'locationURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diLocationURI :: Lens.Lens' DatabaseInput (Lude.Maybe Lude.Text)
diLocationURI = Lens.lens (locationURI :: DatabaseInput -> Lude.Maybe Lude.Text) (\s a -> s {locationURI = a} :: DatabaseInput)
{-# DEPRECATED diLocationURI "Use generic-lens or generic-optics with 'locationURI' instead." #-}

-- | A @DatabaseIdentifier@ structure that describes a target database for resource linking.
--
-- /Note:/ Consider using 'targetDatabase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diTargetDatabase :: Lens.Lens' DatabaseInput (Lude.Maybe DatabaseIdentifier)
diTargetDatabase = Lens.lens (targetDatabase :: DatabaseInput -> Lude.Maybe DatabaseIdentifier) (\s a -> s {targetDatabase = a} :: DatabaseInput)
{-# DEPRECATED diTargetDatabase "Use generic-lens or generic-optics with 'targetDatabase' instead." #-}

-- | These key-value pairs define parameters and properties of the database.
--
-- These key-value pairs define parameters and properties of the database.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diParameters :: Lens.Lens' DatabaseInput (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
diParameters = Lens.lens (parameters :: DatabaseInput -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {parameters = a} :: DatabaseInput)
{-# DEPRECATED diParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | A description of the database.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDescription :: Lens.Lens' DatabaseInput (Lude.Maybe Lude.Text)
diDescription = Lens.lens (description :: DatabaseInput -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: DatabaseInput)
{-# DEPRECATED diDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Creates a set of default permissions on the table for principals.
--
-- /Note:/ Consider using 'createTableDefaultPermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diCreateTableDefaultPermissions :: Lens.Lens' DatabaseInput (Lude.Maybe [PrincipalPermissions])
diCreateTableDefaultPermissions = Lens.lens (createTableDefaultPermissions :: DatabaseInput -> Lude.Maybe [PrincipalPermissions]) (\s a -> s {createTableDefaultPermissions = a} :: DatabaseInput)
{-# DEPRECATED diCreateTableDefaultPermissions "Use generic-lens or generic-optics with 'createTableDefaultPermissions' instead." #-}

-- | The name of the database. For Hive compatibility, this is folded to lowercase when it is stored.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diName :: Lens.Lens' DatabaseInput Lude.Text
diName = Lens.lens (name :: DatabaseInput -> Lude.Text) (\s a -> s {name = a} :: DatabaseInput)
{-# DEPRECATED diName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.ToJSON DatabaseInput where
  toJSON DatabaseInput' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("LocationUri" Lude..=) Lude.<$> locationURI,
            ("TargetDatabase" Lude..=) Lude.<$> targetDatabase,
            ("Parameters" Lude..=) Lude.<$> parameters,
            ("Description" Lude..=) Lude.<$> description,
            ("CreateTableDefaultPermissions" Lude..=)
              Lude.<$> createTableDefaultPermissions,
            Lude.Just ("Name" Lude..= name)
          ]
      )
