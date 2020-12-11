-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.RoleLastUsed
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.RoleLastUsed
  ( RoleLastUsed (..),

    -- * Smart constructor
    mkRoleLastUsed,

    -- * Lenses
    rluLastUsedDate,
    rluRegion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the last time that an IAM role was used. This includes the date and time and the Region in which the role was last used. Activity is only reported for the trailing 400 days. This period can be shorter if your Region began supporting these features within the last year. The role might have been used more than 400 days ago. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#access-advisor_tracking-period Regions Where Data Is Tracked> in the /IAM User Guide/ .
--
-- This data type is returned as a response element in the 'GetRole' and 'GetAccountAuthorizationDetails' operations.
--
-- /See:/ 'mkRoleLastUsed' smart constructor.
data RoleLastUsed = RoleLastUsed'
  { lastUsedDate ::
      Lude.Maybe Lude.ISO8601,
    region :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RoleLastUsed' with the minimum fields required to make a request.
--
-- * 'lastUsedDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> that the role was last used.
--
-- This field is null if the role has not been used within the IAM tracking period. For more information about the tracking period, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#access-advisor_tracking-period Regions Where Data Is Tracked> in the /IAM User Guide/ .
-- * 'region' - The name of the AWS Region in which the role was last used.
mkRoleLastUsed ::
  RoleLastUsed
mkRoleLastUsed =
  RoleLastUsed' {lastUsedDate = Lude.Nothing, region = Lude.Nothing}

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> that the role was last used.
--
-- This field is null if the role has not been used within the IAM tracking period. For more information about the tracking period, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#access-advisor_tracking-period Regions Where Data Is Tracked> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'lastUsedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rluLastUsedDate :: Lens.Lens' RoleLastUsed (Lude.Maybe Lude.ISO8601)
rluLastUsedDate = Lens.lens (lastUsedDate :: RoleLastUsed -> Lude.Maybe Lude.ISO8601) (\s a -> s {lastUsedDate = a} :: RoleLastUsed)
{-# DEPRECATED rluLastUsedDate "Use generic-lens or generic-optics with 'lastUsedDate' instead." #-}

-- | The name of the AWS Region in which the role was last used.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rluRegion :: Lens.Lens' RoleLastUsed (Lude.Maybe Lude.Text)
rluRegion = Lens.lens (region :: RoleLastUsed -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: RoleLastUsed)
{-# DEPRECATED rluRegion "Use generic-lens or generic-optics with 'region' instead." #-}

instance Lude.FromXML RoleLastUsed where
  parseXML x =
    RoleLastUsed'
      Lude.<$> (x Lude..@? "LastUsedDate") Lude.<*> (x Lude..@? "Region")
