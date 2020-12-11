-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.IPRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.IPRange
  ( IPRange (..),

    -- * Smart constructor
    mkIPRange,

    -- * Lenses
    irStatus,
    irCIdRIP,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | This data type is used as a response element in the @DescribeDBSecurityGroups@ action.
--
-- /See:/ 'mkIPRange' smart constructor.
data IPRange = IPRange'
  { status :: Lude.Maybe Lude.Text,
    cIdRIP :: Lude.Maybe Lude.Text
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
-- * 'cIdRIP' - Specifies the IP range.
-- * 'status' - Specifies the status of the IP range. Status can be "authorizing", "authorized", "revoking", and "revoked".
mkIPRange ::
  IPRange
mkIPRange = IPRange' {status = Lude.Nothing, cIdRIP = Lude.Nothing}

-- | Specifies the status of the IP range. Status can be "authorizing", "authorized", "revoking", and "revoked".
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irStatus :: Lens.Lens' IPRange (Lude.Maybe Lude.Text)
irStatus = Lens.lens (status :: IPRange -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: IPRange)
{-# DEPRECATED irStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Specifies the IP range.
--
-- /Note:/ Consider using 'cIdRIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irCIdRIP :: Lens.Lens' IPRange (Lude.Maybe Lude.Text)
irCIdRIP = Lens.lens (cIdRIP :: IPRange -> Lude.Maybe Lude.Text) (\s a -> s {cIdRIP = a} :: IPRange)
{-# DEPRECATED irCIdRIP "Use generic-lens or generic-optics with 'cIdRIP' instead." #-}

instance Lude.FromXML IPRange where
  parseXML x =
    IPRange'
      Lude.<$> (x Lude..@? "Status") Lude.<*> (x Lude..@? "CIDRIP")
