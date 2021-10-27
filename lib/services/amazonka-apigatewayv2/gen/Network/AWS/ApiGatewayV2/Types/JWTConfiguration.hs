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
-- Module      : Network.AWS.ApiGatewayV2.Types.JWTConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApiGatewayV2.Types.JWTConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the configuration of a JWT authorizer. Required for the JWT
-- authorizer type. Supported only for HTTP APIs.
--
-- /See:/ 'newJWTConfiguration' smart constructor.
data JWTConfiguration = JWTConfiguration'
  { -- | A list of the intended recipients of the JWT. A valid JWT must provide
    -- an aud that matches at least one entry in this list. See
    -- <https://tools.ietf.org/html/rfc7519#section-4.1.3 RFC 7519>. Supported
    -- only for HTTP APIs.
    audience :: Prelude.Maybe [Prelude.Text],
    -- | The base domain of the identity provider that issues JSON Web Tokens.
    -- For example, an Amazon Cognito user pool has the following format:
    -- https:\/\/cognito-idp.{region}.amazonaws.com\/{userPoolId} . Required
    -- for the JWT authorizer type. Supported only for HTTP APIs.
    issuer :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JWTConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'audience', 'jWTConfiguration_audience' - A list of the intended recipients of the JWT. A valid JWT must provide
-- an aud that matches at least one entry in this list. See
-- <https://tools.ietf.org/html/rfc7519#section-4.1.3 RFC 7519>. Supported
-- only for HTTP APIs.
--
-- 'issuer', 'jWTConfiguration_issuer' - The base domain of the identity provider that issues JSON Web Tokens.
-- For example, an Amazon Cognito user pool has the following format:
-- https:\/\/cognito-idp.{region}.amazonaws.com\/{userPoolId} . Required
-- for the JWT authorizer type. Supported only for HTTP APIs.
newJWTConfiguration ::
  JWTConfiguration
newJWTConfiguration =
  JWTConfiguration'
    { audience = Prelude.Nothing,
      issuer = Prelude.Nothing
    }

-- | A list of the intended recipients of the JWT. A valid JWT must provide
-- an aud that matches at least one entry in this list. See
-- <https://tools.ietf.org/html/rfc7519#section-4.1.3 RFC 7519>. Supported
-- only for HTTP APIs.
jWTConfiguration_audience :: Lens.Lens' JWTConfiguration (Prelude.Maybe [Prelude.Text])
jWTConfiguration_audience = Lens.lens (\JWTConfiguration' {audience} -> audience) (\s@JWTConfiguration' {} a -> s {audience = a} :: JWTConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The base domain of the identity provider that issues JSON Web Tokens.
-- For example, an Amazon Cognito user pool has the following format:
-- https:\/\/cognito-idp.{region}.amazonaws.com\/{userPoolId} . Required
-- for the JWT authorizer type. Supported only for HTTP APIs.
jWTConfiguration_issuer :: Lens.Lens' JWTConfiguration (Prelude.Maybe Prelude.Text)
jWTConfiguration_issuer = Lens.lens (\JWTConfiguration' {issuer} -> issuer) (\s@JWTConfiguration' {} a -> s {issuer = a} :: JWTConfiguration)

instance Core.FromJSON JWTConfiguration where
  parseJSON =
    Core.withObject
      "JWTConfiguration"
      ( \x ->
          JWTConfiguration'
            Prelude.<$> (x Core..:? "audience" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "issuer")
      )

instance Prelude.Hashable JWTConfiguration

instance Prelude.NFData JWTConfiguration

instance Core.ToJSON JWTConfiguration where
  toJSON JWTConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("audience" Core..=) Prelude.<$> audience,
            ("issuer" Core..=) Prelude.<$> issuer
          ]
      )
