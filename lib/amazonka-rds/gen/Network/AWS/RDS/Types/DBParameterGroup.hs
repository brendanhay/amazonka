{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.DBParameterGroup
  ( DBParameterGroup (..)
  -- * Smart constructor
  , mkDBParameterGroup
  -- * Lenses
  , dbpgDBParameterGroupArn
  , dbpgDBParameterGroupFamily
  , dbpgDBParameterGroupName
  , dbpgDescription
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the details of an Amazon RDS DB parameter group. 
--
-- This data type is used as a response element in the @DescribeDBParameterGroups@ action. 
--
-- /See:/ 'mkDBParameterGroup' smart constructor.
data DBParameterGroup = DBParameterGroup'
  { dBParameterGroupArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) for the DB parameter group.
  , dBParameterGroupFamily :: Core.Maybe Core.Text
    -- ^ The name of the DB parameter group family that this DB parameter group is compatible with.
  , dBParameterGroupName :: Core.Maybe Core.Text
    -- ^ The name of the DB parameter group.
  , description :: Core.Maybe Core.Text
    -- ^ Provides the customer-specified description for this DB parameter group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DBParameterGroup' value with any optional fields omitted.
mkDBParameterGroup
    :: DBParameterGroup
mkDBParameterGroup
  = DBParameterGroup'{dBParameterGroupArn = Core.Nothing,
                      dBParameterGroupFamily = Core.Nothing,
                      dBParameterGroupName = Core.Nothing, description = Core.Nothing}

-- | The Amazon Resource Name (ARN) for the DB parameter group.
--
-- /Note:/ Consider using 'dBParameterGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpgDBParameterGroupArn :: Lens.Lens' DBParameterGroup (Core.Maybe Core.Text)
dbpgDBParameterGroupArn = Lens.field @"dBParameterGroupArn"
{-# INLINEABLE dbpgDBParameterGroupArn #-}
{-# DEPRECATED dBParameterGroupArn "Use generic-lens or generic-optics with 'dBParameterGroupArn' instead"  #-}

-- | The name of the DB parameter group family that this DB parameter group is compatible with.
--
-- /Note:/ Consider using 'dBParameterGroupFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpgDBParameterGroupFamily :: Lens.Lens' DBParameterGroup (Core.Maybe Core.Text)
dbpgDBParameterGroupFamily = Lens.field @"dBParameterGroupFamily"
{-# INLINEABLE dbpgDBParameterGroupFamily #-}
{-# DEPRECATED dBParameterGroupFamily "Use generic-lens or generic-optics with 'dBParameterGroupFamily' instead"  #-}

-- | The name of the DB parameter group.
--
-- /Note:/ Consider using 'dBParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpgDBParameterGroupName :: Lens.Lens' DBParameterGroup (Core.Maybe Core.Text)
dbpgDBParameterGroupName = Lens.field @"dBParameterGroupName"
{-# INLINEABLE dbpgDBParameterGroupName #-}
{-# DEPRECATED dBParameterGroupName "Use generic-lens or generic-optics with 'dBParameterGroupName' instead"  #-}

-- | Provides the customer-specified description for this DB parameter group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpgDescription :: Lens.Lens' DBParameterGroup (Core.Maybe Core.Text)
dbpgDescription = Lens.field @"description"
{-# INLINEABLE dbpgDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

instance Core.FromXML DBParameterGroup where
        parseXML x
          = DBParameterGroup' Core.<$>
              (x Core..@? "DBParameterGroupArn") Core.<*>
                x Core..@? "DBParameterGroupFamily"
                Core.<*> x Core..@? "DBParameterGroupName"
                Core.<*> x Core..@? "Description"
