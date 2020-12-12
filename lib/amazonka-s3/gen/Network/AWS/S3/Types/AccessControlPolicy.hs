{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.AccessControlPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.AccessControlPolicy
  ( AccessControlPolicy (..),

    -- * Smart constructor
    mkAccessControlPolicy,

    -- * Lenses
    acpGrants,
    acpOwner,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Grant
import Network.AWS.S3.Types.Owner

-- | Contains the elements that set the ACL permissions for an object per grantee.
--
-- /See:/ 'mkAccessControlPolicy' smart constructor.
data AccessControlPolicy = AccessControlPolicy'
  { grants ::
      Lude.Maybe [Grant],
    owner :: Lude.Maybe Owner
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AccessControlPolicy' with the minimum fields required to make a request.
--
-- * 'grants' - A list of grants.
-- * 'owner' - Container for the bucket owner's display name and ID.
mkAccessControlPolicy ::
  AccessControlPolicy
mkAccessControlPolicy =
  AccessControlPolicy' {grants = Lude.Nothing, owner = Lude.Nothing}

-- | A list of grants.
--
-- /Note:/ Consider using 'grants' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acpGrants :: Lens.Lens' AccessControlPolicy (Lude.Maybe [Grant])
acpGrants = Lens.lens (grants :: AccessControlPolicy -> Lude.Maybe [Grant]) (\s a -> s {grants = a} :: AccessControlPolicy)
{-# DEPRECATED acpGrants "Use generic-lens or generic-optics with 'grants' instead." #-}

-- | Container for the bucket owner's display name and ID.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acpOwner :: Lens.Lens' AccessControlPolicy (Lude.Maybe Owner)
acpOwner = Lens.lens (owner :: AccessControlPolicy -> Lude.Maybe Owner) (\s a -> s {owner = a} :: AccessControlPolicy)
{-# DEPRECATED acpOwner "Use generic-lens or generic-optics with 'owner' instead." #-}

instance Lude.ToXML AccessControlPolicy where
  toXML AccessControlPolicy' {..} =
    Lude.mconcat
      [ "AccessControlList"
          Lude.@= Lude.toXML (Lude.toXMLList "Grant" Lude.<$> grants),
        "Owner" Lude.@= owner
      ]
