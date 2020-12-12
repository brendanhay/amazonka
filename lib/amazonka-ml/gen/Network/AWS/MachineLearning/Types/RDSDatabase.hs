{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.RDSDatabase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.RDSDatabase
  ( RDSDatabase (..),

    -- * Smart constructor
    mkRDSDatabase,

    -- * Lenses
    rdsdInstanceIdentifier,
    rdsdDatabaseName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The database details of an Amazon RDS database.
--
-- /See:/ 'mkRDSDatabase' smart constructor.
data RDSDatabase = RDSDatabase'
  { instanceIdentifier :: Lude.Text,
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

-- | Creates a value of 'RDSDatabase' with the minimum fields required to make a request.
--
-- * 'databaseName' - Undocumented field.
-- * 'instanceIdentifier' - The ID of an RDS DB instance.
mkRDSDatabase ::
  -- | 'instanceIdentifier'
  Lude.Text ->
  -- | 'databaseName'
  Lude.Text ->
  RDSDatabase
mkRDSDatabase pInstanceIdentifier_ pDatabaseName_ =
  RDSDatabase'
    { instanceIdentifier = pInstanceIdentifier_,
      databaseName = pDatabaseName_
    }

-- | The ID of an RDS DB instance.
--
-- /Note:/ Consider using 'instanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsdInstanceIdentifier :: Lens.Lens' RDSDatabase Lude.Text
rdsdInstanceIdentifier = Lens.lens (instanceIdentifier :: RDSDatabase -> Lude.Text) (\s a -> s {instanceIdentifier = a} :: RDSDatabase)
{-# DEPRECATED rdsdInstanceIdentifier "Use generic-lens or generic-optics with 'instanceIdentifier' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsdDatabaseName :: Lens.Lens' RDSDatabase Lude.Text
rdsdDatabaseName = Lens.lens (databaseName :: RDSDatabase -> Lude.Text) (\s a -> s {databaseName = a} :: RDSDatabase)
{-# DEPRECATED rdsdDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

instance Lude.FromJSON RDSDatabase where
  parseJSON =
    Lude.withObject
      "RDSDatabase"
      ( \x ->
          RDSDatabase'
            Lude.<$> (x Lude..: "InstanceIdentifier")
            Lude.<*> (x Lude..: "DatabaseName")
      )

instance Lude.ToJSON RDSDatabase where
  toJSON RDSDatabase' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("InstanceIdentifier" Lude..= instanceIdentifier),
            Lude.Just ("DatabaseName" Lude..= databaseName)
          ]
      )
