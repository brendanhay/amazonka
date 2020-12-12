{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.IPRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.IPRange
  ( IPRange (..),

    -- * Smart constructor
    mkIPRange,

    -- * Lenses
    iprDescription,
    iprCidrIP,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an IPv4 range.
--
-- /See:/ 'mkIPRange' smart constructor.
data IPRange = IPRange'
  { description :: Lude.Maybe Lude.Text,
    cidrIP :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IPRange' with the minimum fields required to make a request.
--
-- * 'cidrIP' - The IPv4 CIDR range. You can either specify a CIDR range or a source security group, not both. To specify a single IPv4 address, use the /32 prefix length.
-- * 'description' - A description for the security group rule that references this IPv4 address range.
--
-- Constraints: Up to 255 characters in length. Allowed characters are a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=&;{}!$*
mkIPRange ::
  -- | 'cidrIP'
  Lude.Text ->
  IPRange
mkIPRange pCidrIP_ =
  IPRange' {description = Lude.Nothing, cidrIP = pCidrIP_}

-- | A description for the security group rule that references this IPv4 address range.
--
-- Constraints: Up to 255 characters in length. Allowed characters are a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=&;{}!$*
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iprDescription :: Lens.Lens' IPRange (Lude.Maybe Lude.Text)
iprDescription = Lens.lens (description :: IPRange -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: IPRange)
{-# DEPRECATED iprDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The IPv4 CIDR range. You can either specify a CIDR range or a source security group, not both. To specify a single IPv4 address, use the /32 prefix length.
--
-- /Note:/ Consider using 'cidrIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iprCidrIP :: Lens.Lens' IPRange Lude.Text
iprCidrIP = Lens.lens (cidrIP :: IPRange -> Lude.Text) (\s a -> s {cidrIP = a} :: IPRange)
{-# DEPRECATED iprCidrIP "Use generic-lens or generic-optics with 'cidrIP' instead." #-}

instance Lude.FromXML IPRange where
  parseXML x =
    IPRange'
      Lude.<$> (x Lude..@? "description") Lude.<*> (x Lude..@ "cidrIp")

instance Lude.ToQuery IPRange where
  toQuery IPRange' {..} =
    Lude.mconcat
      ["Description" Lude.=: description, "CidrIp" Lude.=: cidrIP]
