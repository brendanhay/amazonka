-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.ServiceEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.ServiceEndpoint
  ( ServiceEndpoint (..),

    -- * Smart constructor
    mkServiceEndpoint,

    -- * Lenses
    seEndpoint,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The endpoint to which service requests can be submitted.
--
-- /See:/ 'mkServiceEndpoint' smart constructor.
newtype ServiceEndpoint = ServiceEndpoint'
  { endpoint ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ServiceEndpoint' with the minimum fields required to make a request.
--
-- * 'endpoint' - Undocumented field.
mkServiceEndpoint ::
  ServiceEndpoint
mkServiceEndpoint = ServiceEndpoint' {endpoint = Lude.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seEndpoint :: Lens.Lens' ServiceEndpoint (Lude.Maybe Lude.Text)
seEndpoint = Lens.lens (endpoint :: ServiceEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {endpoint = a} :: ServiceEndpoint)
{-# DEPRECATED seEndpoint "Use generic-lens or generic-optics with 'endpoint' instead." #-}

instance Lude.FromXML ServiceEndpoint where
  parseXML x = ServiceEndpoint' Lude.<$> (x Lude..@? "Endpoint")
