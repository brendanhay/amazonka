-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.AccountQuota
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.AccountQuota
  ( AccountQuota (..),

    -- * Smart constructor
    mkAccountQuota,

    -- * Lenses
    aqMax,
    aqUsed,
    aqAccountQuotaName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a quota for an AWS account, for example, the number of replication instances allowed.
--
-- /See:/ 'mkAccountQuota' smart constructor.
data AccountQuota = AccountQuota'
  { max :: Lude.Maybe Lude.Integer,
    used :: Lude.Maybe Lude.Integer,
    accountQuotaName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AccountQuota' with the minimum fields required to make a request.
--
-- * 'accountQuotaName' - The name of the AWS DMS quota for this AWS account.
-- * 'max' - The maximum allowed value for the quota.
-- * 'used' - The amount currently used toward the quota maximum.
mkAccountQuota ::
  AccountQuota
mkAccountQuota =
  AccountQuota'
    { max = Lude.Nothing,
      used = Lude.Nothing,
      accountQuotaName = Lude.Nothing
    }

-- | The maximum allowed value for the quota.
--
-- /Note:/ Consider using 'max' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aqMax :: Lens.Lens' AccountQuota (Lude.Maybe Lude.Integer)
aqMax = Lens.lens (max :: AccountQuota -> Lude.Maybe Lude.Integer) (\s a -> s {max = a} :: AccountQuota)
{-# DEPRECATED aqMax "Use generic-lens or generic-optics with 'max' instead." #-}

-- | The amount currently used toward the quota maximum.
--
-- /Note:/ Consider using 'used' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aqUsed :: Lens.Lens' AccountQuota (Lude.Maybe Lude.Integer)
aqUsed = Lens.lens (used :: AccountQuota -> Lude.Maybe Lude.Integer) (\s a -> s {used = a} :: AccountQuota)
{-# DEPRECATED aqUsed "Use generic-lens or generic-optics with 'used' instead." #-}

-- | The name of the AWS DMS quota for this AWS account.
--
-- /Note:/ Consider using 'accountQuotaName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aqAccountQuotaName :: Lens.Lens' AccountQuota (Lude.Maybe Lude.Text)
aqAccountQuotaName = Lens.lens (accountQuotaName :: AccountQuota -> Lude.Maybe Lude.Text) (\s a -> s {accountQuotaName = a} :: AccountQuota)
{-# DEPRECATED aqAccountQuotaName "Use generic-lens or generic-optics with 'accountQuotaName' instead." #-}

instance Lude.FromJSON AccountQuota where
  parseJSON =
    Lude.withObject
      "AccountQuota"
      ( \x ->
          AccountQuota'
            Lude.<$> (x Lude..:? "Max")
            Lude.<*> (x Lude..:? "Used")
            Lude.<*> (x Lude..:? "AccountQuotaName")
      )
