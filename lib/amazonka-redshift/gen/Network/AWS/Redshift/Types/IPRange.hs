-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.IPRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.IPRange
  ( IPRange (..),

    -- * Smart constructor
    mkIPRange,

    -- * Lenses
    irStatus,
    irCIdRIP,
    irTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.Tag

-- | Describes an IP range used in a security group.
--
-- /See:/ 'mkIPRange' smart constructor.
data IPRange = IPRange'
  { status :: Lude.Maybe Lude.Text,
    cIdRIP :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag]
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
-- * 'cIdRIP' - The IP range in Classless Inter-Domain Routing (CIDR) notation.
-- * 'status' - The status of the IP range, for example, "authorized".
-- * 'tags' - The list of tags for the IP range.
mkIPRange ::
  IPRange
mkIPRange =
  IPRange'
    { status = Lude.Nothing,
      cIdRIP = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The status of the IP range, for example, "authorized".
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irStatus :: Lens.Lens' IPRange (Lude.Maybe Lude.Text)
irStatus = Lens.lens (status :: IPRange -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: IPRange)
{-# DEPRECATED irStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The IP range in Classless Inter-Domain Routing (CIDR) notation.
--
-- /Note:/ Consider using 'cIdRIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irCIdRIP :: Lens.Lens' IPRange (Lude.Maybe Lude.Text)
irCIdRIP = Lens.lens (cIdRIP :: IPRange -> Lude.Maybe Lude.Text) (\s a -> s {cIdRIP = a} :: IPRange)
{-# DEPRECATED irCIdRIP "Use generic-lens or generic-optics with 'cIdRIP' instead." #-}

-- | The list of tags for the IP range.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irTags :: Lens.Lens' IPRange (Lude.Maybe [Tag])
irTags = Lens.lens (tags :: IPRange -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: IPRange)
{-# DEPRECATED irTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML IPRange where
  parseXML x =
    IPRange'
      Lude.<$> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "CIDRIP")
      Lude.<*> ( x Lude..@? "Tags" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "Tag")
               )
