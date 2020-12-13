{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.HTTPHeader
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.HTTPHeader
  ( HTTPHeader (..),

    -- * Smart constructor
    mkHTTPHeader,

    -- * Lenses
    httphHeaderValue,
    httphHeaderName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The HTTP header.
--
-- /See:/ 'mkHTTPHeader' smart constructor.
data HTTPHeader = HTTPHeader'
  { -- | The header value.
    headerValue :: Lude.Maybe Lude.Text,
    -- | The header name
    headerName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HTTPHeader' with the minimum fields required to make a request.
--
-- * 'headerValue' - The header value.
-- * 'headerName' - The header name
mkHTTPHeader ::
  HTTPHeader
mkHTTPHeader =
  HTTPHeader'
    { headerValue = Lude.Nothing,
      headerName = Lude.Nothing
    }

-- | The header value.
--
-- /Note:/ Consider using 'headerValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httphHeaderValue :: Lens.Lens' HTTPHeader (Lude.Maybe Lude.Text)
httphHeaderValue = Lens.lens (headerValue :: HTTPHeader -> Lude.Maybe Lude.Text) (\s a -> s {headerValue = a} :: HTTPHeader)
{-# DEPRECATED httphHeaderValue "Use generic-lens or generic-optics with 'headerValue' instead." #-}

-- | The header name
--
-- /Note:/ Consider using 'headerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httphHeaderName :: Lens.Lens' HTTPHeader (Lude.Maybe Lude.Text)
httphHeaderName = Lens.lens (headerName :: HTTPHeader -> Lude.Maybe Lude.Text) (\s a -> s {headerName = a} :: HTTPHeader)
{-# DEPRECATED httphHeaderName "Use generic-lens or generic-optics with 'headerName' instead." #-}

instance Lude.ToJSON HTTPHeader where
  toJSON HTTPHeader' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("headerValue" Lude..=) Lude.<$> headerValue,
            ("headerName" Lude..=) Lude.<$> headerName
          ]
      )
