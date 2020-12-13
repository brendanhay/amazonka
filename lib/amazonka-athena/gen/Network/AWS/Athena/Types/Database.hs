{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.Database
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.Database
  ( Database (..),

    -- * Smart constructor
    mkDatabase,

    -- * Lenses
    dName,
    dParameters,
    dDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains metadata information for a database in a data catalog.
--
-- /See:/ 'mkDatabase' smart constructor.
data Database = Database'
  { -- | The name of the database.
    name :: Lude.Text,
    -- | A set of custom key/value pairs.
    parameters :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | An optional description of the database.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Database' with the minimum fields required to make a request.
--
-- * 'name' - The name of the database.
-- * 'parameters' - A set of custom key/value pairs.
-- * 'description' - An optional description of the database.
mkDatabase ::
  -- | 'name'
  Lude.Text ->
  Database
mkDatabase pName_ =
  Database'
    { name = pName_,
      parameters = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The name of the database.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dName :: Lens.Lens' Database Lude.Text
dName = Lens.lens (name :: Database -> Lude.Text) (\s a -> s {name = a} :: Database)
{-# DEPRECATED dName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A set of custom key/value pairs.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dParameters :: Lens.Lens' Database (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
dParameters = Lens.lens (parameters :: Database -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {parameters = a} :: Database)
{-# DEPRECATED dParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | An optional description of the database.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDescription :: Lens.Lens' Database (Lude.Maybe Lude.Text)
dDescription = Lens.lens (description :: Database -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Database)
{-# DEPRECATED dDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON Database where
  parseJSON =
    Lude.withObject
      "Database"
      ( \x ->
          Database'
            Lude.<$> (x Lude..: "Name")
            Lude.<*> (x Lude..:? "Parameters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Description")
      )
