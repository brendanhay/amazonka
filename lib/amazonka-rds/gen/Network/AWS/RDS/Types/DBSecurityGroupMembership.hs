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
    dsgmStatus,
    dsgmDBSecurityGroupName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

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
  { -- | The status of the DB security group.
    status :: Lude.Maybe Lude.Text,
    -- | The name of the DB security group.
    dbSecurityGroupName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DBSecurityGroupMembership' with the minimum fields required to make a request.
--
-- * 'status' - The status of the DB security group.
-- * 'dbSecurityGroupName' - The name of the DB security group.
mkDBSecurityGroupMembership ::
  DBSecurityGroupMembership
mkDBSecurityGroupMembership =
  DBSecurityGroupMembership'
    { status = Lude.Nothing,
      dbSecurityGroupName = Lude.Nothing
    }

-- | The status of the DB security group.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgmStatus :: Lens.Lens' DBSecurityGroupMembership (Lude.Maybe Lude.Text)
dsgmStatus = Lens.lens (status :: DBSecurityGroupMembership -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: DBSecurityGroupMembership)
{-# DEPRECATED dsgmStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The name of the DB security group.
--
-- /Note:/ Consider using 'dbSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgmDBSecurityGroupName :: Lens.Lens' DBSecurityGroupMembership (Lude.Maybe Lude.Text)
dsgmDBSecurityGroupName = Lens.lens (dbSecurityGroupName :: DBSecurityGroupMembership -> Lude.Maybe Lude.Text) (\s a -> s {dbSecurityGroupName = a} :: DBSecurityGroupMembership)
{-# DEPRECATED dsgmDBSecurityGroupName "Use generic-lens or generic-optics with 'dbSecurityGroupName' instead." #-}

instance Lude.FromXML DBSecurityGroupMembership where
  parseXML x =
    DBSecurityGroupMembership'
      Lude.<$> (x Lude..@? "Status") Lude.<*> (x Lude..@? "DBSecurityGroupName")
