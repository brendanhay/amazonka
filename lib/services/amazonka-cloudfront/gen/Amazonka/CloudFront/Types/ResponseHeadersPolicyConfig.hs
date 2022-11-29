{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudFront.Types.ResponseHeadersPolicyConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.ResponseHeadersPolicyConfig where

import Amazonka.CloudFront.Types.ResponseHeadersPolicyCorsConfig
import Amazonka.CloudFront.Types.ResponseHeadersPolicyCustomHeadersConfig
import Amazonka.CloudFront.Types.ResponseHeadersPolicySecurityHeadersConfig
import Amazonka.CloudFront.Types.ResponseHeadersPolicyServerTimingHeadersConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A response headers policy configuration.
--
-- A response headers policy configuration contains metadata about the
-- response headers policy, and configurations for sets of HTTP response
-- headers and their values. CloudFront adds the headers in the policy to
-- HTTP responses that it sends for requests that match a cache behavior
-- associated with the policy.
--
-- /See:/ 'newResponseHeadersPolicyConfig' smart constructor.
data ResponseHeadersPolicyConfig = ResponseHeadersPolicyConfig'
  { -- | A configuration for enabling the @Server-Timing@ header in HTTP
    -- responses sent from CloudFront.
    serverTimingHeadersConfig :: Prelude.Maybe ResponseHeadersPolicyServerTimingHeadersConfig,
    -- | A comment to describe the response headers policy.
    --
    -- The comment cannot be longer than 128 characters.
    comment :: Prelude.Maybe Prelude.Text,
    -- | A configuration for a set of security-related HTTP response headers.
    securityHeadersConfig :: Prelude.Maybe ResponseHeadersPolicySecurityHeadersConfig,
    -- | A configuration for a set of HTTP response headers that are used for
    -- cross-origin resource sharing (CORS).
    corsConfig :: Prelude.Maybe ResponseHeadersPolicyCorsConfig,
    -- | A configuration for a set of custom HTTP response headers.
    customHeadersConfig :: Prelude.Maybe ResponseHeadersPolicyCustomHeadersConfig,
    -- | A name to identify the response headers policy.
    --
    -- The name must be unique for response headers policies in this Amazon Web
    -- Services account.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResponseHeadersPolicyConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverTimingHeadersConfig', 'responseHeadersPolicyConfig_serverTimingHeadersConfig' - A configuration for enabling the @Server-Timing@ header in HTTP
-- responses sent from CloudFront.
--
-- 'comment', 'responseHeadersPolicyConfig_comment' - A comment to describe the response headers policy.
--
-- The comment cannot be longer than 128 characters.
--
-- 'securityHeadersConfig', 'responseHeadersPolicyConfig_securityHeadersConfig' - A configuration for a set of security-related HTTP response headers.
--
-- 'corsConfig', 'responseHeadersPolicyConfig_corsConfig' - A configuration for a set of HTTP response headers that are used for
-- cross-origin resource sharing (CORS).
--
-- 'customHeadersConfig', 'responseHeadersPolicyConfig_customHeadersConfig' - A configuration for a set of custom HTTP response headers.
--
-- 'name', 'responseHeadersPolicyConfig_name' - A name to identify the response headers policy.
--
-- The name must be unique for response headers policies in this Amazon Web
-- Services account.
newResponseHeadersPolicyConfig ::
  -- | 'name'
  Prelude.Text ->
  ResponseHeadersPolicyConfig
newResponseHeadersPolicyConfig pName_ =
  ResponseHeadersPolicyConfig'
    { serverTimingHeadersConfig =
        Prelude.Nothing,
      comment = Prelude.Nothing,
      securityHeadersConfig = Prelude.Nothing,
      corsConfig = Prelude.Nothing,
      customHeadersConfig = Prelude.Nothing,
      name = pName_
    }

-- | A configuration for enabling the @Server-Timing@ header in HTTP
-- responses sent from CloudFront.
responseHeadersPolicyConfig_serverTimingHeadersConfig :: Lens.Lens' ResponseHeadersPolicyConfig (Prelude.Maybe ResponseHeadersPolicyServerTimingHeadersConfig)
responseHeadersPolicyConfig_serverTimingHeadersConfig = Lens.lens (\ResponseHeadersPolicyConfig' {serverTimingHeadersConfig} -> serverTimingHeadersConfig) (\s@ResponseHeadersPolicyConfig' {} a -> s {serverTimingHeadersConfig = a} :: ResponseHeadersPolicyConfig)

-- | A comment to describe the response headers policy.
--
-- The comment cannot be longer than 128 characters.
responseHeadersPolicyConfig_comment :: Lens.Lens' ResponseHeadersPolicyConfig (Prelude.Maybe Prelude.Text)
responseHeadersPolicyConfig_comment = Lens.lens (\ResponseHeadersPolicyConfig' {comment} -> comment) (\s@ResponseHeadersPolicyConfig' {} a -> s {comment = a} :: ResponseHeadersPolicyConfig)

-- | A configuration for a set of security-related HTTP response headers.
responseHeadersPolicyConfig_securityHeadersConfig :: Lens.Lens' ResponseHeadersPolicyConfig (Prelude.Maybe ResponseHeadersPolicySecurityHeadersConfig)
responseHeadersPolicyConfig_securityHeadersConfig = Lens.lens (\ResponseHeadersPolicyConfig' {securityHeadersConfig} -> securityHeadersConfig) (\s@ResponseHeadersPolicyConfig' {} a -> s {securityHeadersConfig = a} :: ResponseHeadersPolicyConfig)

-- | A configuration for a set of HTTP response headers that are used for
-- cross-origin resource sharing (CORS).
responseHeadersPolicyConfig_corsConfig :: Lens.Lens' ResponseHeadersPolicyConfig (Prelude.Maybe ResponseHeadersPolicyCorsConfig)
responseHeadersPolicyConfig_corsConfig = Lens.lens (\ResponseHeadersPolicyConfig' {corsConfig} -> corsConfig) (\s@ResponseHeadersPolicyConfig' {} a -> s {corsConfig = a} :: ResponseHeadersPolicyConfig)

-- | A configuration for a set of custom HTTP response headers.
responseHeadersPolicyConfig_customHeadersConfig :: Lens.Lens' ResponseHeadersPolicyConfig (Prelude.Maybe ResponseHeadersPolicyCustomHeadersConfig)
responseHeadersPolicyConfig_customHeadersConfig = Lens.lens (\ResponseHeadersPolicyConfig' {customHeadersConfig} -> customHeadersConfig) (\s@ResponseHeadersPolicyConfig' {} a -> s {customHeadersConfig = a} :: ResponseHeadersPolicyConfig)

-- | A name to identify the response headers policy.
--
-- The name must be unique for response headers policies in this Amazon Web
-- Services account.
responseHeadersPolicyConfig_name :: Lens.Lens' ResponseHeadersPolicyConfig Prelude.Text
responseHeadersPolicyConfig_name = Lens.lens (\ResponseHeadersPolicyConfig' {name} -> name) (\s@ResponseHeadersPolicyConfig' {} a -> s {name = a} :: ResponseHeadersPolicyConfig)

instance Core.FromXML ResponseHeadersPolicyConfig where
  parseXML x =
    ResponseHeadersPolicyConfig'
      Prelude.<$> (x Core..@? "ServerTimingHeadersConfig")
      Prelude.<*> (x Core..@? "Comment")
      Prelude.<*> (x Core..@? "SecurityHeadersConfig")
      Prelude.<*> (x Core..@? "CorsConfig")
      Prelude.<*> (x Core..@? "CustomHeadersConfig")
      Prelude.<*> (x Core..@ "Name")

instance Prelude.Hashable ResponseHeadersPolicyConfig where
  hashWithSalt _salt ResponseHeadersPolicyConfig' {..} =
    _salt
      `Prelude.hashWithSalt` serverTimingHeadersConfig
      `Prelude.hashWithSalt` comment
      `Prelude.hashWithSalt` securityHeadersConfig
      `Prelude.hashWithSalt` corsConfig
      `Prelude.hashWithSalt` customHeadersConfig
      `Prelude.hashWithSalt` name

instance Prelude.NFData ResponseHeadersPolicyConfig where
  rnf ResponseHeadersPolicyConfig' {..} =
    Prelude.rnf serverTimingHeadersConfig
      `Prelude.seq` Prelude.rnf comment
      `Prelude.seq` Prelude.rnf securityHeadersConfig
      `Prelude.seq` Prelude.rnf corsConfig
      `Prelude.seq` Prelude.rnf customHeadersConfig
      `Prelude.seq` Prelude.rnf name

instance Core.ToXML ResponseHeadersPolicyConfig where
  toXML ResponseHeadersPolicyConfig' {..} =
    Prelude.mconcat
      [ "ServerTimingHeadersConfig"
          Core.@= serverTimingHeadersConfig,
        "Comment" Core.@= comment,
        "SecurityHeadersConfig"
          Core.@= securityHeadersConfig,
        "CorsConfig" Core.@= corsConfig,
        "CustomHeadersConfig" Core.@= customHeadersConfig,
        "Name" Core.@= name
      ]
