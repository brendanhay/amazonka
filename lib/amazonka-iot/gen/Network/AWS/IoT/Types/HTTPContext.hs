{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.HTTPContext
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.HTTPContext
  ( HTTPContext (..),

    -- * Smart constructor
    mkHTTPContext,

    -- * Lenses
    httpcHeaders,
    httpcQueryString,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the HTTP context to use for the test authorizer request.
--
-- /See:/ 'mkHTTPContext' smart constructor.
data HTTPContext = HTTPContext'
  { -- | The header keys and values in an HTTP authorization request.
    headers :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The query string keys and values in an HTTP authorization request.
    queryString :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HTTPContext' with the minimum fields required to make a request.
--
-- * 'headers' - The header keys and values in an HTTP authorization request.
-- * 'queryString' - The query string keys and values in an HTTP authorization request.
mkHTTPContext ::
  HTTPContext
mkHTTPContext =
  HTTPContext' {headers = Lude.Nothing, queryString = Lude.Nothing}

-- | The header keys and values in an HTTP authorization request.
--
-- /Note:/ Consider using 'headers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpcHeaders :: Lens.Lens' HTTPContext (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
httpcHeaders = Lens.lens (headers :: HTTPContext -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {headers = a} :: HTTPContext)
{-# DEPRECATED httpcHeaders "Use generic-lens or generic-optics with 'headers' instead." #-}

-- | The query string keys and values in an HTTP authorization request.
--
-- /Note:/ Consider using 'queryString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpcQueryString :: Lens.Lens' HTTPContext (Lude.Maybe Lude.Text)
httpcQueryString = Lens.lens (queryString :: HTTPContext -> Lude.Maybe Lude.Text) (\s a -> s {queryString = a} :: HTTPContext)
{-# DEPRECATED httpcQueryString "Use generic-lens or generic-optics with 'queryString' instead." #-}

instance Lude.ToJSON HTTPContext where
  toJSON HTTPContext' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("headers" Lude..=) Lude.<$> headers,
            ("queryString" Lude..=) Lude.<$> queryString
          ]
      )
