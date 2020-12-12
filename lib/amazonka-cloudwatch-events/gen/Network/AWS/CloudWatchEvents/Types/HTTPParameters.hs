{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.HTTPParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.HTTPParameters
  ( HTTPParameters (..),

    -- * Smart constructor
    mkHTTPParameters,

    -- * Lenses
    httppPathParameterValues,
    httppQueryStringParameters,
    httppHeaderParameters,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | These are custom parameter to be used when the target is an API Gateway REST APIs.
--
-- /See:/ 'mkHTTPParameters' smart constructor.
data HTTPParameters = HTTPParameters'
  { pathParameterValues ::
      Lude.Maybe [Lude.Text],
    queryStringParameters ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    headerParameters ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HTTPParameters' with the minimum fields required to make a request.
--
-- * 'headerParameters' - The headers that need to be sent as part of request invoking the API Gateway REST API.
-- * 'pathParameterValues' - The path parameter values to be used to populate API Gateway REST API path wildcards ("*").
-- * 'queryStringParameters' - The query string keys/values that need to be sent as part of request invoking the API Gateway REST API.
mkHTTPParameters ::
  HTTPParameters
mkHTTPParameters =
  HTTPParameters'
    { pathParameterValues = Lude.Nothing,
      queryStringParameters = Lude.Nothing,
      headerParameters = Lude.Nothing
    }

-- | The path parameter values to be used to populate API Gateway REST API path wildcards ("*").
--
-- /Note:/ Consider using 'pathParameterValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httppPathParameterValues :: Lens.Lens' HTTPParameters (Lude.Maybe [Lude.Text])
httppPathParameterValues = Lens.lens (pathParameterValues :: HTTPParameters -> Lude.Maybe [Lude.Text]) (\s a -> s {pathParameterValues = a} :: HTTPParameters)
{-# DEPRECATED httppPathParameterValues "Use generic-lens or generic-optics with 'pathParameterValues' instead." #-}

-- | The query string keys/values that need to be sent as part of request invoking the API Gateway REST API.
--
-- /Note:/ Consider using 'queryStringParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httppQueryStringParameters :: Lens.Lens' HTTPParameters (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
httppQueryStringParameters = Lens.lens (queryStringParameters :: HTTPParameters -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {queryStringParameters = a} :: HTTPParameters)
{-# DEPRECATED httppQueryStringParameters "Use generic-lens or generic-optics with 'queryStringParameters' instead." #-}

-- | The headers that need to be sent as part of request invoking the API Gateway REST API.
--
-- /Note:/ Consider using 'headerParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httppHeaderParameters :: Lens.Lens' HTTPParameters (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
httppHeaderParameters = Lens.lens (headerParameters :: HTTPParameters -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {headerParameters = a} :: HTTPParameters)
{-# DEPRECATED httppHeaderParameters "Use generic-lens or generic-optics with 'headerParameters' instead." #-}

instance Lude.FromJSON HTTPParameters where
  parseJSON =
    Lude.withObject
      "HTTPParameters"
      ( \x ->
          HTTPParameters'
            Lude.<$> (x Lude..:? "PathParameterValues" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "QueryStringParameters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "HeaderParameters" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON HTTPParameters where
  toJSON HTTPParameters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PathParameterValues" Lude..=) Lude.<$> pathParameterValues,
            ("QueryStringParameters" Lude..=) Lude.<$> queryStringParameters,
            ("HeaderParameters" Lude..=) Lude.<$> headerParameters
          ]
      )
