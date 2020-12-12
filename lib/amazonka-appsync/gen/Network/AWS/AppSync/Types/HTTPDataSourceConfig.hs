{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.HTTPDataSourceConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.HTTPDataSourceConfig
  ( HTTPDataSourceConfig (..),

    -- * Smart constructor
    mkHTTPDataSourceConfig,

    -- * Lenses
    httpdscAuthorizationConfig,
    httpdscEndpoint,
  )
where

import Network.AWS.AppSync.Types.AuthorizationConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an HTTP data source configuration.
--
-- /See:/ 'mkHTTPDataSourceConfig' smart constructor.
data HTTPDataSourceConfig = HTTPDataSourceConfig'
  { authorizationConfig ::
      Lude.Maybe AuthorizationConfig,
    endpoint :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HTTPDataSourceConfig' with the minimum fields required to make a request.
--
-- * 'authorizationConfig' - The authorization config in case the HTTP endpoint requires authorization.
-- * 'endpoint' - The HTTP URL endpoint. You can either specify the domain name or IP, and port combination, and the URL scheme must be HTTP or HTTPS. If the port is not specified, AWS AppSync uses the default port 80 for the HTTP endpoint and port 443 for HTTPS endpoints.
mkHTTPDataSourceConfig ::
  HTTPDataSourceConfig
mkHTTPDataSourceConfig =
  HTTPDataSourceConfig'
    { authorizationConfig = Lude.Nothing,
      endpoint = Lude.Nothing
    }

-- | The authorization config in case the HTTP endpoint requires authorization.
--
-- /Note:/ Consider using 'authorizationConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpdscAuthorizationConfig :: Lens.Lens' HTTPDataSourceConfig (Lude.Maybe AuthorizationConfig)
httpdscAuthorizationConfig = Lens.lens (authorizationConfig :: HTTPDataSourceConfig -> Lude.Maybe AuthorizationConfig) (\s a -> s {authorizationConfig = a} :: HTTPDataSourceConfig)
{-# DEPRECATED httpdscAuthorizationConfig "Use generic-lens or generic-optics with 'authorizationConfig' instead." #-}

-- | The HTTP URL endpoint. You can either specify the domain name or IP, and port combination, and the URL scheme must be HTTP or HTTPS. If the port is not specified, AWS AppSync uses the default port 80 for the HTTP endpoint and port 443 for HTTPS endpoints.
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpdscEndpoint :: Lens.Lens' HTTPDataSourceConfig (Lude.Maybe Lude.Text)
httpdscEndpoint = Lens.lens (endpoint :: HTTPDataSourceConfig -> Lude.Maybe Lude.Text) (\s a -> s {endpoint = a} :: HTTPDataSourceConfig)
{-# DEPRECATED httpdscEndpoint "Use generic-lens or generic-optics with 'endpoint' instead." #-}

instance Lude.FromJSON HTTPDataSourceConfig where
  parseJSON =
    Lude.withObject
      "HTTPDataSourceConfig"
      ( \x ->
          HTTPDataSourceConfig'
            Lude.<$> (x Lude..:? "authorizationConfig")
            Lude.<*> (x Lude..:? "endpoint")
      )

instance Lude.ToJSON HTTPDataSourceConfig where
  toJSON HTTPDataSourceConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("authorizationConfig" Lude..=) Lude.<$> authorizationConfig,
            ("endpoint" Lude..=) Lude.<$> endpoint
          ]
      )
