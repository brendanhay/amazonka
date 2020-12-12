{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBParameterGroupNameMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBParameterGroupNameMessage
  ( DBParameterGroupNameMessage (..),

    -- * Smart constructor
    mkDBParameterGroupNameMessage,

    -- * Lenses
    dpgnmDBParameterGroupName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the result of a successful invocation of the @ModifyDBParameterGroup@ or @ResetDBParameterGroup@ action.
--
-- /See:/ 'mkDBParameterGroupNameMessage' smart constructor.
newtype DBParameterGroupNameMessage = DBParameterGroupNameMessage'
  { dbParameterGroupName ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DBParameterGroupNameMessage' with the minimum fields required to make a request.
--
-- * 'dbParameterGroupName' - The name of the DB parameter group.
mkDBParameterGroupNameMessage ::
  DBParameterGroupNameMessage
mkDBParameterGroupNameMessage =
  DBParameterGroupNameMessage' {dbParameterGroupName = Lude.Nothing}

-- | The name of the DB parameter group.
--
-- /Note:/ Consider using 'dbParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgnmDBParameterGroupName :: Lens.Lens' DBParameterGroupNameMessage (Lude.Maybe Lude.Text)
dpgnmDBParameterGroupName = Lens.lens (dbParameterGroupName :: DBParameterGroupNameMessage -> Lude.Maybe Lude.Text) (\s a -> s {dbParameterGroupName = a} :: DBParameterGroupNameMessage)
{-# DEPRECATED dpgnmDBParameterGroupName "Use generic-lens or generic-optics with 'dbParameterGroupName' instead." #-}

instance Lude.FromXML DBParameterGroupNameMessage where
  parseXML x =
    DBParameterGroupNameMessage'
      Lude.<$> (x Lude..@? "DBParameterGroupName")
