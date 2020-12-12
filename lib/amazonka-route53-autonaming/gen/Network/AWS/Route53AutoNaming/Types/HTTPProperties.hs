{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.HTTPProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.HTTPProperties
  ( HTTPProperties (..),

    -- * Smart constructor
    mkHTTPProperties,

    -- * Lenses
    httppHTTPName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A complex type that contains the name of an HTTP namespace.
--
-- /See:/ 'mkHTTPProperties' smart constructor.
newtype HTTPProperties = HTTPProperties'
  { hTTPName ::
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

-- | Creates a value of 'HTTPProperties' with the minimum fields required to make a request.
--
-- * 'hTTPName' - The name of an HTTP namespace.
mkHTTPProperties ::
  HTTPProperties
mkHTTPProperties = HTTPProperties' {hTTPName = Lude.Nothing}

-- | The name of an HTTP namespace.
--
-- /Note:/ Consider using 'hTTPName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httppHTTPName :: Lens.Lens' HTTPProperties (Lude.Maybe Lude.Text)
httppHTTPName = Lens.lens (hTTPName :: HTTPProperties -> Lude.Maybe Lude.Text) (\s a -> s {hTTPName = a} :: HTTPProperties)
{-# DEPRECATED httppHTTPName "Use generic-lens or generic-optics with 'hTTPName' instead." #-}

instance Lude.FromJSON HTTPProperties where
  parseJSON =
    Lude.withObject
      "HTTPProperties"
      (\x -> HTTPProperties' Lude.<$> (x Lude..:? "HttpName"))
