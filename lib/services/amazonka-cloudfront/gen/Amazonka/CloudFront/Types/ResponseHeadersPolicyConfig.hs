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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.ResponseHeadersPolicyConfig where

import Amazonka.CloudFront.Types.ResponseHeadersPolicyCorsConfig
import Amazonka.CloudFront.Types.ResponseHeadersPolicyCustomHeadersConfig
import Amazonka.CloudFront.Types.ResponseHeadersPolicyRemoveHeadersConfig
import Amazonka.CloudFront.Types.ResponseHeadersPolicySecurityHeadersConfig
import Amazonka.CloudFront.Types.ResponseHeadersPolicyServerTimingHeadersConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A response headers policy configuration.
--
-- A response headers policy configuration contains metadata about the
-- response headers policy, and configurations for sets of HTTP response
-- headers.
--
-- /See:/ 'newResponseHeadersPolicyConfig' smart constructor.
data ResponseHeadersPolicyConfig = ResponseHeadersPolicyConfig'
  { -- | A comment to describe the response headers policy.
    --
    -- The comment cannot be longer than 128 characters.
    comment :: Prelude.Maybe Prelude.Text,
    -- | A configuration for a set of HTTP response headers that are used for
    -- cross-origin resource sharing (CORS).
    corsConfig :: Prelude.Maybe ResponseHeadersPolicyCorsConfig,
    -- | A configuration for a set of custom HTTP response headers.
    customHeadersConfig :: Prelude.Maybe ResponseHeadersPolicyCustomHeadersConfig,
    -- | A configuration for a set of HTTP headers to remove from the HTTP
    -- response.
    removeHeadersConfig :: Prelude.Maybe ResponseHeadersPolicyRemoveHeadersConfig,
    -- | A configuration for a set of security-related HTTP response headers.
    securityHeadersConfig :: Prelude.Maybe ResponseHeadersPolicySecurityHeadersConfig,
    -- | A configuration for enabling the @Server-Timing@ header in HTTP
    -- responses sent from CloudFront.
    serverTimingHeadersConfig :: Prelude.Maybe ResponseHeadersPolicyServerTimingHeadersConfig,
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
-- 'comment', 'responseHeadersPolicyConfig_comment' - A comment to describe the response headers policy.
--
-- The comment cannot be longer than 128 characters.
--
-- 'corsConfig', 'responseHeadersPolicyConfig_corsConfig' - A configuration for a set of HTTP response headers that are used for
-- cross-origin resource sharing (CORS).
--
-- 'customHeadersConfig', 'responseHeadersPolicyConfig_customHeadersConfig' - A configuration for a set of custom HTTP response headers.
--
-- 'removeHeadersConfig', 'responseHeadersPolicyConfig_removeHeadersConfig' - A configuration for a set of HTTP headers to remove from the HTTP
-- response.
--
-- 'securityHeadersConfig', 'responseHeadersPolicyConfig_securityHeadersConfig' - A configuration for a set of security-related HTTP response headers.
--
-- 'serverTimingHeadersConfig', 'responseHeadersPolicyConfig_serverTimingHeadersConfig' - A configuration for enabling the @Server-Timing@ header in HTTP
-- responses sent from CloudFront.
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
    { comment =
        Prelude.Nothing,
      corsConfig = Prelude.Nothing,
      customHeadersConfig = Prelude.Nothing,
      removeHeadersConfig = Prelude.Nothing,
      securityHeadersConfig = Prelude.Nothing,
      serverTimingHeadersConfig = Prelude.Nothing,
      name = pName_
    }

-- | A comment to describe the response headers policy.
--
-- The comment cannot be longer than 128 characters.
responseHeadersPolicyConfig_comment :: Lens.Lens' ResponseHeadersPolicyConfig (Prelude.Maybe Prelude.Text)
responseHeadersPolicyConfig_comment = Lens.lens (\ResponseHeadersPolicyConfig' {comment} -> comment) (\s@ResponseHeadersPolicyConfig' {} a -> s {comment = a} :: ResponseHeadersPolicyConfig)

-- | A configuration for a set of HTTP response headers that are used for
-- cross-origin resource sharing (CORS).
responseHeadersPolicyConfig_corsConfig :: Lens.Lens' ResponseHeadersPolicyConfig (Prelude.Maybe ResponseHeadersPolicyCorsConfig)
responseHeadersPolicyConfig_corsConfig = Lens.lens (\ResponseHeadersPolicyConfig' {corsConfig} -> corsConfig) (\s@ResponseHeadersPolicyConfig' {} a -> s {corsConfig = a} :: ResponseHeadersPolicyConfig)

-- | A configuration for a set of custom HTTP response headers.
responseHeadersPolicyConfig_customHeadersConfig :: Lens.Lens' ResponseHeadersPolicyConfig (Prelude.Maybe ResponseHeadersPolicyCustomHeadersConfig)
responseHeadersPolicyConfig_customHeadersConfig = Lens.lens (\ResponseHeadersPolicyConfig' {customHeadersConfig} -> customHeadersConfig) (\s@ResponseHeadersPolicyConfig' {} a -> s {customHeadersConfig = a} :: ResponseHeadersPolicyConfig)

-- | A configuration for a set of HTTP headers to remove from the HTTP
-- response.
responseHeadersPolicyConfig_removeHeadersConfig :: Lens.Lens' ResponseHeadersPolicyConfig (Prelude.Maybe ResponseHeadersPolicyRemoveHeadersConfig)
responseHeadersPolicyConfig_removeHeadersConfig = Lens.lens (\ResponseHeadersPolicyConfig' {removeHeadersConfig} -> removeHeadersConfig) (\s@ResponseHeadersPolicyConfig' {} a -> s {removeHeadersConfig = a} :: ResponseHeadersPolicyConfig)

-- | A configuration for a set of security-related HTTP response headers.
responseHeadersPolicyConfig_securityHeadersConfig :: Lens.Lens' ResponseHeadersPolicyConfig (Prelude.Maybe ResponseHeadersPolicySecurityHeadersConfig)
responseHeadersPolicyConfig_securityHeadersConfig = Lens.lens (\ResponseHeadersPolicyConfig' {securityHeadersConfig} -> securityHeadersConfig) (\s@ResponseHeadersPolicyConfig' {} a -> s {securityHeadersConfig = a} :: ResponseHeadersPolicyConfig)

-- | A configuration for enabling the @Server-Timing@ header in HTTP
-- responses sent from CloudFront.
responseHeadersPolicyConfig_serverTimingHeadersConfig :: Lens.Lens' ResponseHeadersPolicyConfig (Prelude.Maybe ResponseHeadersPolicyServerTimingHeadersConfig)
responseHeadersPolicyConfig_serverTimingHeadersConfig = Lens.lens (\ResponseHeadersPolicyConfig' {serverTimingHeadersConfig} -> serverTimingHeadersConfig) (\s@ResponseHeadersPolicyConfig' {} a -> s {serverTimingHeadersConfig = a} :: ResponseHeadersPolicyConfig)

-- | A name to identify the response headers policy.
--
-- The name must be unique for response headers policies in this Amazon Web
-- Services account.
responseHeadersPolicyConfig_name :: Lens.Lens' ResponseHeadersPolicyConfig Prelude.Text
responseHeadersPolicyConfig_name = Lens.lens (\ResponseHeadersPolicyConfig' {name} -> name) (\s@ResponseHeadersPolicyConfig' {} a -> s {name = a} :: ResponseHeadersPolicyConfig)

instance Data.FromXML ResponseHeadersPolicyConfig where
  parseXML x =
    ResponseHeadersPolicyConfig'
      Prelude.<$> (x Data..@? "Comment")
      Prelude.<*> (x Data..@? "CorsConfig")
      Prelude.<*> (x Data..@? "CustomHeadersConfig")
      Prelude.<*> (x Data..@? "RemoveHeadersConfig")
      Prelude.<*> (x Data..@? "SecurityHeadersConfig")
      Prelude.<*> (x Data..@? "ServerTimingHeadersConfig")
      Prelude.<*> (x Data..@ "Name")

instance Prelude.Hashable ResponseHeadersPolicyConfig where
  hashWithSalt _salt ResponseHeadersPolicyConfig' {..} =
    _salt
      `Prelude.hashWithSalt` comment
      `Prelude.hashWithSalt` corsConfig
      `Prelude.hashWithSalt` customHeadersConfig
      `Prelude.hashWithSalt` removeHeadersConfig
      `Prelude.hashWithSalt` securityHeadersConfig
      `Prelude.hashWithSalt` serverTimingHeadersConfig
      `Prelude.hashWithSalt` name

instance Prelude.NFData ResponseHeadersPolicyConfig where
  rnf ResponseHeadersPolicyConfig' {..} =
    Prelude.rnf comment
      `Prelude.seq` Prelude.rnf corsConfig
      `Prelude.seq` Prelude.rnf customHeadersConfig
      `Prelude.seq` Prelude.rnf removeHeadersConfig
      `Prelude.seq` Prelude.rnf securityHeadersConfig
      `Prelude.seq` Prelude.rnf serverTimingHeadersConfig
      `Prelude.seq` Prelude.rnf name

instance Data.ToXML ResponseHeadersPolicyConfig where
  toXML ResponseHeadersPolicyConfig' {..} =
    Prelude.mconcat
      [ "Comment" Data.@= comment,
        "CorsConfig" Data.@= corsConfig,
        "CustomHeadersConfig" Data.@= customHeadersConfig,
        "RemoveHeadersConfig" Data.@= removeHeadersConfig,
        "SecurityHeadersConfig"
          Data.@= securityHeadersConfig,
        "ServerTimingHeadersConfig"
          Data.@= serverTimingHeadersConfig,
        "Name" Data.@= name
      ]
