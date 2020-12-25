{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    dbcpgDBClusterParameterGroupArn,
    dbcpgDBClusterParameterGroupName,
    dbcpgDBParameterGroupFamily,
    dbcpgDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.DBClusterParameterGroupArn as Types
import qualified Network.AWS.RDS.Types.DBClusterParameterGroupName as Types
import qualified Network.AWS.RDS.Types.DBParameterGroupFamily as Types
import qualified Network.AWS.RDS.Types.Description as Types

-- | Contains the details of an Amazon RDS DB cluster parameter group.
--
-- This data type is used as a response element in the @DescribeDBClusterParameterGroups@ action.
--
-- /See:/ 'mkDBClusterParameterGroup' smart constructor.
data DBClusterParameterGroup = DBClusterParameterGroup'
  { -- | The Amazon Resource Name (ARN) for the DB cluster parameter group.
    dBClusterParameterGroupArn :: Core.Maybe Types.DBClusterParameterGroupArn,
    -- | The name of the DB cluster parameter group.
    dBClusterParameterGroupName :: Core.Maybe Types.DBClusterParameterGroupName,
    -- | The name of the DB parameter group family that this DB cluster parameter group is compatible with.
    dBParameterGroupFamily :: Core.Maybe Types.DBParameterGroupFamily,
    -- | Provides the customer-specified description for this DB cluster parameter group.
    description :: Core.Maybe Types.Description
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DBClusterParameterGroup' value with any optional fields omitted.
mkDBClusterParameterGroup ::
  DBClusterParameterGroup
mkDBClusterParameterGroup =
  DBClusterParameterGroup'
    { dBClusterParameterGroupArn =
        Core.Nothing,
      dBClusterParameterGroupName = Core.Nothing,
      dBParameterGroupFamily = Core.Nothing,
      description = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) for the DB cluster parameter group.
--
-- /Note:/ Consider using 'dBClusterParameterGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcpgDBClusterParameterGroupArn :: Lens.Lens' DBClusterParameterGroup (Core.Maybe Types.DBClusterParameterGroupArn)
dbcpgDBClusterParameterGroupArn = Lens.field @"dBClusterParameterGroupArn"
{-# DEPRECATED dbcpgDBClusterParameterGroupArn "Use generic-lens or generic-optics with 'dBClusterParameterGroupArn' instead." #-}

-- | The name of the DB cluster parameter group.
--
-- /Note:/ Consider using 'dBClusterParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcpgDBClusterParameterGroupName :: Lens.Lens' DBClusterParameterGroup (Core.Maybe Types.DBClusterParameterGroupName)
dbcpgDBClusterParameterGroupName = Lens.field @"dBClusterParameterGroupName"
{-# DEPRECATED dbcpgDBClusterParameterGroupName "Use generic-lens or generic-optics with 'dBClusterParameterGroupName' instead." #-}

-- | The name of the DB parameter group family that this DB cluster parameter group is compatible with.
--
-- /Note:/ Consider using 'dBParameterGroupFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcpgDBParameterGroupFamily :: Lens.Lens' DBClusterParameterGroup (Core.Maybe Types.DBParameterGroupFamily)
dbcpgDBParameterGroupFamily = Lens.field @"dBParameterGroupFamily"
{-# DEPRECATED dbcpgDBParameterGroupFamily "Use generic-lens or generic-optics with 'dBParameterGroupFamily' instead." #-}

-- | Provides the customer-specified description for this DB cluster parameter group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcpgDescription :: Lens.Lens' DBClusterParameterGroup (Core.Maybe Types.Description)
dbcpgDescription = Lens.field @"description"
{-# DEPRECATED dbcpgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Core.FromXML DBClusterParameterGroup where
  parseXML x =
    DBClusterParameterGroup'
      Core.<$> (x Core..@? "DBClusterParameterGroupArn")
      Core.<*> (x Core..@? "DBClusterParameterGroupName")
      Core.<*> (x Core..@? "DBParameterGroupFamily")
      Core.<*> (x Core..@? "Description")
