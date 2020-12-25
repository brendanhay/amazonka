{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBSecurityGroupMembership
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBSecurityGroupMembership
  ( DBSecurityGroupMembership (..),

    -- * Smart constructor
    mkDBSecurityGroupMembership,

    -- * Lenses
    dbsgmDBSecurityGroupName,
    dbsgmStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.DBSecurityGroupName as Types
import qualified Network.AWS.RDS.Types.Status as Types

-- | This data type is used as a response element in the following actions:
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
--     * @RestoreDBInstanceToPointInTime@
--
--
--
-- /See:/ 'mkDBSecurityGroupMembership' smart constructor.
data DBSecurityGroupMembership = DBSecurityGroupMembership'
  { -- | The name of the DB security group.
    dBSecurityGroupName :: Core.Maybe Types.DBSecurityGroupName,
    -- | The status of the DB security group.
    status :: Core.Maybe Types.Status
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DBSecurityGroupMembership' value with any optional fields omitted.
mkDBSecurityGroupMembership ::
  DBSecurityGroupMembership
mkDBSecurityGroupMembership =
  DBSecurityGroupMembership'
    { dBSecurityGroupName = Core.Nothing,
      status = Core.Nothing
    }

-- | The name of the DB security group.
--
-- /Note:/ Consider using 'dBSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsgmDBSecurityGroupName :: Lens.Lens' DBSecurityGroupMembership (Core.Maybe Types.DBSecurityGroupName)
dbsgmDBSecurityGroupName = Lens.field @"dBSecurityGroupName"
{-# DEPRECATED dbsgmDBSecurityGroupName "Use generic-lens or generic-optics with 'dBSecurityGroupName' instead." #-}

-- | The status of the DB security group.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsgmStatus :: Lens.Lens' DBSecurityGroupMembership (Core.Maybe Types.Status)
dbsgmStatus = Lens.field @"status"
{-# DEPRECATED dbsgmStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromXML DBSecurityGroupMembership where
  parseXML x =
    DBSecurityGroupMembership'
      Core.<$> (x Core..@? "DBSecurityGroupName") Core.<*> (x Core..@? "Status")
