-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.ReceiptFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.ReceiptFilter
  ( ReceiptFilter (..),

    -- * Smart constructor
    mkReceiptFilter,

    -- * Lenses
    rfName,
    rfIPFilter,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SES.Types.ReceiptIPFilter

-- | A receipt IP address filter enables you to specify whether to accept or reject mail originating from an IP address or range of IP addresses.
--
-- For information about setting up IP address filters, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-ip-filters.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkReceiptFilter' smart constructor.
data ReceiptFilter = ReceiptFilter'
  { name :: Lude.Text,
    ipFilter :: ReceiptIPFilter
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReceiptFilter' with the minimum fields required to make a request.
--
-- * 'ipFilter' - A structure that provides the IP addresses to block or allow, and whether to block or allow incoming mail from them.
-- * 'name' - The name of the IP address filter. The name must:
--
--
--     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).
--
--
--     * Start and end with a letter or number.
--
--
--     * Contain less than 64 characters.
mkReceiptFilter ::
  -- | 'name'
  Lude.Text ->
  -- | 'ipFilter'
  ReceiptIPFilter ->
  ReceiptFilter
mkReceiptFilter pName_ pIPFilter_ =
  ReceiptFilter' {name = pName_, ipFilter = pIPFilter_}

-- | The name of the IP address filter. The name must:
--
--
--     * This value can only contain ASCII letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).
--
--
--     * Start and end with a letter or number.
--
--
--     * Contain less than 64 characters.
--
--
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfName :: Lens.Lens' ReceiptFilter Lude.Text
rfName = Lens.lens (name :: ReceiptFilter -> Lude.Text) (\s a -> s {name = a} :: ReceiptFilter)
{-# DEPRECATED rfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A structure that provides the IP addresses to block or allow, and whether to block or allow incoming mail from them.
--
-- /Note:/ Consider using 'ipFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfIPFilter :: Lens.Lens' ReceiptFilter ReceiptIPFilter
rfIPFilter = Lens.lens (ipFilter :: ReceiptFilter -> ReceiptIPFilter) (\s a -> s {ipFilter = a} :: ReceiptFilter)
{-# DEPRECATED rfIPFilter "Use generic-lens or generic-optics with 'ipFilter' instead." #-}

instance Lude.FromXML ReceiptFilter where
  parseXML x =
    ReceiptFilter'
      Lude.<$> (x Lude..@ "Name") Lude.<*> (x Lude..@ "IpFilter")

instance Lude.ToQuery ReceiptFilter where
  toQuery ReceiptFilter' {..} =
    Lude.mconcat ["Name" Lude.=: name, "IpFilter" Lude.=: ipFilter]
