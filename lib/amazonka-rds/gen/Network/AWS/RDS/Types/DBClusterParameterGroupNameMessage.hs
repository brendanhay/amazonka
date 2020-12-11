-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBClusterParameterGroupNameMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBClusterParameterGroupNameMessage
  ( DBClusterParameterGroupNameMessage (..),

    -- * Smart constructor
    mkDBClusterParameterGroupNameMessage,

    -- * Lenses
    dcpgnmDBClusterParameterGroupName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- |
--
-- /See:/ 'mkDBClusterParameterGroupNameMessage' smart constructor.
newtype DBClusterParameterGroupNameMessage = DBClusterParameterGroupNameMessage'
  { dbClusterParameterGroupName ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DBClusterParameterGroupNameMessage' with the minimum fields required to make a request.
--
-- * 'dbClusterParameterGroupName' - The name of the DB cluster parameter group.
--
-- Constraints:
--
--     * Must be 1 to 255 letters or numbers.
--
--
--     * First character must be a letter
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens
mkDBClusterParameterGroupNameMessage ::
  DBClusterParameterGroupNameMessage
mkDBClusterParameterGroupNameMessage =
  DBClusterParameterGroupNameMessage'
    { dbClusterParameterGroupName =
        Lude.Nothing
    }

-- | The name of the DB cluster parameter group.
--
-- Constraints:
--
--     * Must be 1 to 255 letters or numbers.
--
--
--     * First character must be a letter
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens
--
--
--
-- /Note:/ Consider using 'dbClusterParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpgnmDBClusterParameterGroupName :: Lens.Lens' DBClusterParameterGroupNameMessage (Lude.Maybe Lude.Text)
dcpgnmDBClusterParameterGroupName = Lens.lens (dbClusterParameterGroupName :: DBClusterParameterGroupNameMessage -> Lude.Maybe Lude.Text) (\s a -> s {dbClusterParameterGroupName = a} :: DBClusterParameterGroupNameMessage)
{-# DEPRECATED dcpgnmDBClusterParameterGroupName "Use generic-lens or generic-optics with 'dbClusterParameterGroupName' instead." #-}

instance Lude.FromXML DBClusterParameterGroupNameMessage where
  parseXML x =
    DBClusterParameterGroupNameMessage'
      Lude.<$> (x Lude..@? "DBClusterParameterGroupName")
