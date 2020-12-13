{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.DNSRequestAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.DNSRequestAction
  ( DNSRequestAction (..),

    -- * Smart constructor
    mkDNSRequestAction,

    -- * Lenses
    draDomain,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the DNS_REQUEST action described in this finding.
--
-- /See:/ 'mkDNSRequestAction' smart constructor.
newtype DNSRequestAction = DNSRequestAction'
  { -- | The domain information for the API request.
    domain :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DNSRequestAction' with the minimum fields required to make a request.
--
-- * 'domain' - The domain information for the API request.
mkDNSRequestAction ::
  DNSRequestAction
mkDNSRequestAction = DNSRequestAction' {domain = Lude.Nothing}

-- | The domain information for the API request.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
draDomain :: Lens.Lens' DNSRequestAction (Lude.Maybe Lude.Text)
draDomain = Lens.lens (domain :: DNSRequestAction -> Lude.Maybe Lude.Text) (\s a -> s {domain = a} :: DNSRequestAction)
{-# DEPRECATED draDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

instance Lude.FromJSON DNSRequestAction where
  parseJSON =
    Lude.withObject
      "DNSRequestAction"
      (\x -> DNSRequestAction' Lude.<$> (x Lude..:? "domain"))
