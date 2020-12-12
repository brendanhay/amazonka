{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.IPRoute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.IPRoute
  ( IPRoute (..),

    -- * Smart constructor
    mkIPRoute,

    -- * Lenses
    irCidrIP,
    irDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | IP address block. This is often the address block of the DNS server used for your on-premises domain.
--
-- /See:/ 'mkIPRoute' smart constructor.
data IPRoute = IPRoute'
  { cidrIP :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IPRoute' with the minimum fields required to make a request.
--
-- * 'cidrIP' - IP address block using CIDR format, for example 10.0.0.0/24. This is often the address block of the DNS server used for your on-premises domain. For a single IP address use a CIDR address block with /32. For example 10.0.0.0/32.
-- * 'description' - Description of the address block.
mkIPRoute ::
  IPRoute
mkIPRoute =
  IPRoute' {cidrIP = Lude.Nothing, description = Lude.Nothing}

-- | IP address block using CIDR format, for example 10.0.0.0/24. This is often the address block of the DNS server used for your on-premises domain. For a single IP address use a CIDR address block with /32. For example 10.0.0.0/32.
--
-- /Note:/ Consider using 'cidrIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irCidrIP :: Lens.Lens' IPRoute (Lude.Maybe Lude.Text)
irCidrIP = Lens.lens (cidrIP :: IPRoute -> Lude.Maybe Lude.Text) (\s a -> s {cidrIP = a} :: IPRoute)
{-# DEPRECATED irCidrIP "Use generic-lens or generic-optics with 'cidrIP' instead." #-}

-- | Description of the address block.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irDescription :: Lens.Lens' IPRoute (Lude.Maybe Lude.Text)
irDescription = Lens.lens (description :: IPRoute -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: IPRoute)
{-# DEPRECATED irDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.ToJSON IPRoute where
  toJSON IPRoute' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CidrIp" Lude..=) Lude.<$> cidrIP,
            ("Description" Lude..=) Lude.<$> description
          ]
      )
