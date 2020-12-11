-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBClusterParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBClusterParameterGroup
  ( DBClusterParameterGroup (..),

    -- * Smart constructor
    mkDBClusterParameterGroup,

    -- * Lenses
    dcpgDBClusterParameterGroupARN,
    dcpgDBParameterGroupFamily,
    dcpgDBClusterParameterGroupName,
    dcpgDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the details of an Amazon RDS DB cluster parameter group.
--
-- This data type is used as a response element in the @DescribeDBClusterParameterGroups@ action.
--
-- /See:/ 'mkDBClusterParameterGroup' smart constructor.
data DBClusterParameterGroup = DBClusterParameterGroup'
  { dbClusterParameterGroupARN ::
      Lude.Maybe Lude.Text,
    dbParameterGroupFamily ::
      Lude.Maybe Lude.Text,
    dbClusterParameterGroupName ::
      Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DBClusterParameterGroup' with the minimum fields required to make a request.
--
-- * 'dbClusterParameterGroupARN' - The Amazon Resource Name (ARN) for the DB cluster parameter group.
-- * 'dbClusterParameterGroupName' - The name of the DB cluster parameter group.
-- * 'dbParameterGroupFamily' - The name of the DB parameter group family that this DB cluster parameter group is compatible with.
-- * 'description' - Provides the customer-specified description for this DB cluster parameter group.
mkDBClusterParameterGroup ::
  DBClusterParameterGroup
mkDBClusterParameterGroup =
  DBClusterParameterGroup'
    { dbClusterParameterGroupARN =
        Lude.Nothing,
      dbParameterGroupFamily = Lude.Nothing,
      dbClusterParameterGroupName = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) for the DB cluster parameter group.
--
-- /Note:/ Consider using 'dbClusterParameterGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpgDBClusterParameterGroupARN :: Lens.Lens' DBClusterParameterGroup (Lude.Maybe Lude.Text)
dcpgDBClusterParameterGroupARN = Lens.lens (dbClusterParameterGroupARN :: DBClusterParameterGroup -> Lude.Maybe Lude.Text) (\s a -> s {dbClusterParameterGroupARN = a} :: DBClusterParameterGroup)
{-# DEPRECATED dcpgDBClusterParameterGroupARN "Use generic-lens or generic-optics with 'dbClusterParameterGroupARN' instead." #-}

-- | The name of the DB parameter group family that this DB cluster parameter group is compatible with.
--
-- /Note:/ Consider using 'dbParameterGroupFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpgDBParameterGroupFamily :: Lens.Lens' DBClusterParameterGroup (Lude.Maybe Lude.Text)
dcpgDBParameterGroupFamily = Lens.lens (dbParameterGroupFamily :: DBClusterParameterGroup -> Lude.Maybe Lude.Text) (\s a -> s {dbParameterGroupFamily = a} :: DBClusterParameterGroup)
{-# DEPRECATED dcpgDBParameterGroupFamily "Use generic-lens or generic-optics with 'dbParameterGroupFamily' instead." #-}

-- | The name of the DB cluster parameter group.
--
-- /Note:/ Consider using 'dbClusterParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpgDBClusterParameterGroupName :: Lens.Lens' DBClusterParameterGroup (Lude.Maybe Lude.Text)
dcpgDBClusterParameterGroupName = Lens.lens (dbClusterParameterGroupName :: DBClusterParameterGroup -> Lude.Maybe Lude.Text) (\s a -> s {dbClusterParameterGroupName = a} :: DBClusterParameterGroup)
{-# DEPRECATED dcpgDBClusterParameterGroupName "Use generic-lens or generic-optics with 'dbClusterParameterGroupName' instead." #-}

-- | Provides the customer-specified description for this DB cluster parameter group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpgDescription :: Lens.Lens' DBClusterParameterGroup (Lude.Maybe Lude.Text)
dcpgDescription = Lens.lens (description :: DBClusterParameterGroup -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: DBClusterParameterGroup)
{-# DEPRECATED dcpgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromXML DBClusterParameterGroup where
  parseXML x =
    DBClusterParameterGroup'
      Lude.<$> (x Lude..@? "DBClusterParameterGroupArn")
      Lude.<*> (x Lude..@? "DBParameterGroupFamily")
      Lude.<*> (x Lude..@? "DBClusterParameterGroupName")
      Lude.<*> (x Lude..@? "Description")
