{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.HTTPEndpointDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.HTTPEndpointDescription
  ( HTTPEndpointDescription (..),

    -- * Smart constructor
    mkHTTPEndpointDescription,

    -- * Lenses
    httpedURL,
    httpedName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the HTTP endpoint selected as the destination.
--
-- /See:/ 'mkHTTPEndpointDescription' smart constructor.
data HTTPEndpointDescription = HTTPEndpointDescription'
  { -- | The URL of the HTTP endpoint selected as the destination.
    url :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The name of the HTTP endpoint selected as the destination.
    name :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HTTPEndpointDescription' with the minimum fields required to make a request.
--
-- * 'url' - The URL of the HTTP endpoint selected as the destination.
-- * 'name' - The name of the HTTP endpoint selected as the destination.
mkHTTPEndpointDescription ::
  HTTPEndpointDescription
mkHTTPEndpointDescription =
  HTTPEndpointDescription' {url = Lude.Nothing, name = Lude.Nothing}

-- | The URL of the HTTP endpoint selected as the destination.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpedURL :: Lens.Lens' HTTPEndpointDescription (Lude.Maybe (Lude.Sensitive Lude.Text))
httpedURL = Lens.lens (url :: HTTPEndpointDescription -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {url = a} :: HTTPEndpointDescription)
{-# DEPRECATED httpedURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The name of the HTTP endpoint selected as the destination.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpedName :: Lens.Lens' HTTPEndpointDescription (Lude.Maybe Lude.Text)
httpedName = Lens.lens (name :: HTTPEndpointDescription -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: HTTPEndpointDescription)
{-# DEPRECATED httpedName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON HTTPEndpointDescription where
  parseJSON =
    Lude.withObject
      "HTTPEndpointDescription"
      ( \x ->
          HTTPEndpointDescription'
            Lude.<$> (x Lude..:? "Url") Lude.<*> (x Lude..:? "Name")
      )
