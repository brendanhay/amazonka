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
-- Module      : Amazonka.SecurityHub.Types.AwsS3BucketWebsiteConfigurationRoutingRuleRedirect
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsS3BucketWebsiteConfigurationRoutingRuleRedirect where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The rules to redirect the request if the condition in @Condition@ is
-- met.
--
-- /See:/ 'newAwsS3BucketWebsiteConfigurationRoutingRuleRedirect' smart constructor.
data AwsS3BucketWebsiteConfigurationRoutingRuleRedirect = AwsS3BucketWebsiteConfigurationRoutingRuleRedirect'
  { -- | The host name to use in the redirect request.
    hostname :: Prelude.Maybe Prelude.Text,
    -- | The object key prefix to use in the redirect request.
    --
    -- Cannot be provided if @ReplaceKeyWith@ is present.
    replaceKeyPrefixWith :: Prelude.Maybe Prelude.Text,
    -- | The specific object key to use in the redirect request.
    --
    -- Cannot be provided if @ReplaceKeyPrefixWith@ is present.
    replaceKeyWith :: Prelude.Maybe Prelude.Text,
    -- | The protocol to use to redirect the request. By default, uses the
    -- protocol from the original request.
    protocol :: Prelude.Maybe Prelude.Text,
    -- | The HTTP redirect code to use in the response.
    httpRedirectCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsS3BucketWebsiteConfigurationRoutingRuleRedirect' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostname', 'awsS3BucketWebsiteConfigurationRoutingRuleRedirect_hostname' - The host name to use in the redirect request.
--
-- 'replaceKeyPrefixWith', 'awsS3BucketWebsiteConfigurationRoutingRuleRedirect_replaceKeyPrefixWith' - The object key prefix to use in the redirect request.
--
-- Cannot be provided if @ReplaceKeyWith@ is present.
--
-- 'replaceKeyWith', 'awsS3BucketWebsiteConfigurationRoutingRuleRedirect_replaceKeyWith' - The specific object key to use in the redirect request.
--
-- Cannot be provided if @ReplaceKeyPrefixWith@ is present.
--
-- 'protocol', 'awsS3BucketWebsiteConfigurationRoutingRuleRedirect_protocol' - The protocol to use to redirect the request. By default, uses the
-- protocol from the original request.
--
-- 'httpRedirectCode', 'awsS3BucketWebsiteConfigurationRoutingRuleRedirect_httpRedirectCode' - The HTTP redirect code to use in the response.
newAwsS3BucketWebsiteConfigurationRoutingRuleRedirect ::
  AwsS3BucketWebsiteConfigurationRoutingRuleRedirect
newAwsS3BucketWebsiteConfigurationRoutingRuleRedirect =
  AwsS3BucketWebsiteConfigurationRoutingRuleRedirect'
    { hostname =
        Prelude.Nothing,
      replaceKeyPrefixWith =
        Prelude.Nothing,
      replaceKeyWith =
        Prelude.Nothing,
      protocol =
        Prelude.Nothing,
      httpRedirectCode =
        Prelude.Nothing
    }

-- | The host name to use in the redirect request.
awsS3BucketWebsiteConfigurationRoutingRuleRedirect_hostname :: Lens.Lens' AwsS3BucketWebsiteConfigurationRoutingRuleRedirect (Prelude.Maybe Prelude.Text)
awsS3BucketWebsiteConfigurationRoutingRuleRedirect_hostname = Lens.lens (\AwsS3BucketWebsiteConfigurationRoutingRuleRedirect' {hostname} -> hostname) (\s@AwsS3BucketWebsiteConfigurationRoutingRuleRedirect' {} a -> s {hostname = a} :: AwsS3BucketWebsiteConfigurationRoutingRuleRedirect)

-- | The object key prefix to use in the redirect request.
--
-- Cannot be provided if @ReplaceKeyWith@ is present.
awsS3BucketWebsiteConfigurationRoutingRuleRedirect_replaceKeyPrefixWith :: Lens.Lens' AwsS3BucketWebsiteConfigurationRoutingRuleRedirect (Prelude.Maybe Prelude.Text)
awsS3BucketWebsiteConfigurationRoutingRuleRedirect_replaceKeyPrefixWith = Lens.lens (\AwsS3BucketWebsiteConfigurationRoutingRuleRedirect' {replaceKeyPrefixWith} -> replaceKeyPrefixWith) (\s@AwsS3BucketWebsiteConfigurationRoutingRuleRedirect' {} a -> s {replaceKeyPrefixWith = a} :: AwsS3BucketWebsiteConfigurationRoutingRuleRedirect)

-- | The specific object key to use in the redirect request.
--
-- Cannot be provided if @ReplaceKeyPrefixWith@ is present.
awsS3BucketWebsiteConfigurationRoutingRuleRedirect_replaceKeyWith :: Lens.Lens' AwsS3BucketWebsiteConfigurationRoutingRuleRedirect (Prelude.Maybe Prelude.Text)
awsS3BucketWebsiteConfigurationRoutingRuleRedirect_replaceKeyWith = Lens.lens (\AwsS3BucketWebsiteConfigurationRoutingRuleRedirect' {replaceKeyWith} -> replaceKeyWith) (\s@AwsS3BucketWebsiteConfigurationRoutingRuleRedirect' {} a -> s {replaceKeyWith = a} :: AwsS3BucketWebsiteConfigurationRoutingRuleRedirect)

-- | The protocol to use to redirect the request. By default, uses the
-- protocol from the original request.
awsS3BucketWebsiteConfigurationRoutingRuleRedirect_protocol :: Lens.Lens' AwsS3BucketWebsiteConfigurationRoutingRuleRedirect (Prelude.Maybe Prelude.Text)
awsS3BucketWebsiteConfigurationRoutingRuleRedirect_protocol = Lens.lens (\AwsS3BucketWebsiteConfigurationRoutingRuleRedirect' {protocol} -> protocol) (\s@AwsS3BucketWebsiteConfigurationRoutingRuleRedirect' {} a -> s {protocol = a} :: AwsS3BucketWebsiteConfigurationRoutingRuleRedirect)

-- | The HTTP redirect code to use in the response.
awsS3BucketWebsiteConfigurationRoutingRuleRedirect_httpRedirectCode :: Lens.Lens' AwsS3BucketWebsiteConfigurationRoutingRuleRedirect (Prelude.Maybe Prelude.Text)
awsS3BucketWebsiteConfigurationRoutingRuleRedirect_httpRedirectCode = Lens.lens (\AwsS3BucketWebsiteConfigurationRoutingRuleRedirect' {httpRedirectCode} -> httpRedirectCode) (\s@AwsS3BucketWebsiteConfigurationRoutingRuleRedirect' {} a -> s {httpRedirectCode = a} :: AwsS3BucketWebsiteConfigurationRoutingRuleRedirect)

instance
  Core.FromJSON
    AwsS3BucketWebsiteConfigurationRoutingRuleRedirect
  where
  parseJSON =
    Core.withObject
      "AwsS3BucketWebsiteConfigurationRoutingRuleRedirect"
      ( \x ->
          AwsS3BucketWebsiteConfigurationRoutingRuleRedirect'
            Prelude.<$> (x Core..:? "Hostname")
              Prelude.<*> (x Core..:? "ReplaceKeyPrefixWith")
              Prelude.<*> (x Core..:? "ReplaceKeyWith")
              Prelude.<*> (x Core..:? "Protocol")
              Prelude.<*> (x Core..:? "HttpRedirectCode")
      )

instance
  Prelude.Hashable
    AwsS3BucketWebsiteConfigurationRoutingRuleRedirect
  where
  hashWithSalt
    _salt
    AwsS3BucketWebsiteConfigurationRoutingRuleRedirect' {..} =
      _salt `Prelude.hashWithSalt` hostname
        `Prelude.hashWithSalt` replaceKeyPrefixWith
        `Prelude.hashWithSalt` replaceKeyWith
        `Prelude.hashWithSalt` protocol
        `Prelude.hashWithSalt` httpRedirectCode

instance
  Prelude.NFData
    AwsS3BucketWebsiteConfigurationRoutingRuleRedirect
  where
  rnf
    AwsS3BucketWebsiteConfigurationRoutingRuleRedirect' {..} =
      Prelude.rnf hostname
        `Prelude.seq` Prelude.rnf replaceKeyPrefixWith
        `Prelude.seq` Prelude.rnf replaceKeyWith
        `Prelude.seq` Prelude.rnf protocol
        `Prelude.seq` Prelude.rnf httpRedirectCode

instance
  Core.ToJSON
    AwsS3BucketWebsiteConfigurationRoutingRuleRedirect
  where
  toJSON
    AwsS3BucketWebsiteConfigurationRoutingRuleRedirect' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("Hostname" Core..=) Prelude.<$> hostname,
              ("ReplaceKeyPrefixWith" Core..=)
                Prelude.<$> replaceKeyPrefixWith,
              ("ReplaceKeyWith" Core..=)
                Prelude.<$> replaceKeyWith,
              ("Protocol" Core..=) Prelude.<$> protocol,
              ("HttpRedirectCode" Core..=)
                Prelude.<$> httpRedirectCode
            ]
        )
