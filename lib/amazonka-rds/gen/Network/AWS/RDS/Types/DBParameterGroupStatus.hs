{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBParameterGroupStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBParameterGroupStatus
  ( DBParameterGroupStatus (..),

    -- * Smart constructor
    mkDBParameterGroupStatus,

    -- * Lenses
    dpgsDBParameterGroupName,
    dpgsParameterApplyStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The status of the DB parameter group.
--
-- This data type is used as a response element in the following actions:
--
--     * @CreateDBInstance@
--
--
--     * @CreateDBInstanceReadReplica@
--
--
--     * @DeleteDBInstance@
--
--
--     * @ModifyDBInstance@
--
--
--     * @RebootDBInstance@
--
--
--     * @RestoreDBInstanceFromDBSnapshot@
--
--
--
-- /See:/ 'mkDBParameterGroupStatus' smart constructor.
data DBParameterGroupStatus = DBParameterGroupStatus'
  { dbParameterGroupName ::
      Lude.Maybe Lude.Text,
    parameterApplyStatus :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DBParameterGroupStatus' with the minimum fields required to make a request.
--
-- * 'dbParameterGroupName' - The name of the DB parameter group.
-- * 'parameterApplyStatus' - The status of parameter updates.
mkDBParameterGroupStatus ::
  DBParameterGroupStatus
mkDBParameterGroupStatus =
  DBParameterGroupStatus'
    { dbParameterGroupName = Lude.Nothing,
      parameterApplyStatus = Lude.Nothing
    }

-- | The name of the DB parameter group.
--
-- /Note:/ Consider using 'dbParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgsDBParameterGroupName :: Lens.Lens' DBParameterGroupStatus (Lude.Maybe Lude.Text)
dpgsDBParameterGroupName = Lens.lens (dbParameterGroupName :: DBParameterGroupStatus -> Lude.Maybe Lude.Text) (\s a -> s {dbParameterGroupName = a} :: DBParameterGroupStatus)
{-# DEPRECATED dpgsDBParameterGroupName "Use generic-lens or generic-optics with 'dbParameterGroupName' instead." #-}

-- | The status of parameter updates.
--
-- /Note:/ Consider using 'parameterApplyStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgsParameterApplyStatus :: Lens.Lens' DBParameterGroupStatus (Lude.Maybe Lude.Text)
dpgsParameterApplyStatus = Lens.lens (parameterApplyStatus :: DBParameterGroupStatus -> Lude.Maybe Lude.Text) (\s a -> s {parameterApplyStatus = a} :: DBParameterGroupStatus)
{-# DEPRECATED dpgsParameterApplyStatus "Use generic-lens or generic-optics with 'parameterApplyStatus' instead." #-}

instance Lude.FromXML DBParameterGroupStatus where
  parseXML x =
    DBParameterGroupStatus'
      Lude.<$> (x Lude..@? "DBParameterGroupName")
      Lude.<*> (x Lude..@? "ParameterApplyStatus")
