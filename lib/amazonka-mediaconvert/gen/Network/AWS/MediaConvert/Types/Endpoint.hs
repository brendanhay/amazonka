{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Endpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Endpoint
  ( Endpoint (..),

    -- * Smart constructor
    mkEndpoint,

    -- * Lenses
    eURL,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an account-specific API endpoint.
--
-- /See:/ 'mkEndpoint' smart constructor.
newtype Endpoint = Endpoint'
  { -- | URL of endpoint
    url :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Endpoint' with the minimum fields required to make a request.
--
-- * 'url' - URL of endpoint
mkEndpoint ::
  Endpoint
mkEndpoint = Endpoint' {url = Lude.Nothing}

-- | URL of endpoint
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eURL :: Lens.Lens' Endpoint (Lude.Maybe Lude.Text)
eURL = Lens.lens (url :: Endpoint -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: Endpoint)
{-# DEPRECATED eURL "Use generic-lens or generic-optics with 'url' instead." #-}

instance Lude.FromJSON Endpoint where
  parseJSON =
    Lude.withObject
      "Endpoint"
      (\x -> Endpoint' Lude.<$> (x Lude..:? "url"))
