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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The rules to redirect the request if the condition in @Condition@ is
-- met.
--
-- /See:/ 'newAwsS3BucketWebsiteConfigurationRoutingRuleRedirect' smart constructor.
data AwsS3BucketWebsiteConfigurationRoutingRuleRedirect = AwsS3BucketWebsiteConfigurationRoutingRuleRedirect'
  { -- | The host name to use in the redirect request.
    hostname :: Prelude.Maybe Prelude.Text,
    -- | The HTTP redirect code to use in the response.
    httpRedirectCode :: Prelude.Maybe Prelude.Text,
    -- | The protocol to use to redirect the request. By default, uses the
    -- protocol from the original request.
    protocol :: Prelude.Maybe Prelude.Text,
    -- | The object key prefix to use in the redirect request.
    --
    -- Cannot be provided if @ReplaceKeyWith@ is present.
    replaceKeyPrefixWith :: Prelude.Maybe Prelude.Text,
    -- | The specific object key to use in the redirect request.
    --
    -- Cannot be provided if @ReplaceKeyPrefixWith@ is present.
    replaceKeyWith :: Prelude.Maybe Prelude.Text
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
-- 'httpRedirectCode', 'awsS3BucketWebsiteConfigurationRoutingRuleRedirect_httpRedirectCode' - The HTTP redirect code to use in the response.
--
-- 'protocol', 'awsS3BucketWebsiteConfigurationRoutingRuleRedirect_protocol' - The protocol to use to redirect the request. By default, uses the
-- protocol from the original request.
--
-- 'replaceKeyPrefixWith', 'awsS3BucketWebsiteConfigurationRoutingRuleRedirect_replaceKeyPrefixWith' - The object key prefix to use in the redirect request.
--
-- Cannot be provided if @ReplaceKeyWith@ is present.
--
-- 'replaceKeyWith', 'awsS3BucketWebsiteConfigurationRoutingRuleRedirect_replaceKeyWith' - The specific object key to use in the redirect request.
--
-- Cannot be provided if @ReplaceKeyPrefixWith@ is present.
newAwsS3BucketWebsiteConfigurationRoutingRuleRedirect ::
  AwsS3BucketWebsiteConfigurationRoutingRuleRedirect
newAwsS3BucketWebsiteConfigurationRoutingRuleRedirect =
  AwsS3BucketWebsiteConfigurationRoutingRuleRedirect'
    { hostname =
        Prelude.Nothing,
      httpRedirectCode =
        Prelude.Nothing,
      protocol =
        Prelude.Nothing,
      replaceKeyPrefixWith =
        Prelude.Nothing,
      replaceKeyWith =
        Prelude.Nothing
    }

-- | The host name to use in the redirect request.
awsS3BucketWebsiteConfigurationRoutingRuleRedirect_hostname :: Lens.Lens' AwsS3BucketWebsiteConfigurationRoutingRuleRedirect (Prelude.Maybe Prelude.Text)
awsS3BucketWebsiteConfigurationRoutingRuleRedirect_hostname = Lens.lens (\AwsS3BucketWebsiteConfigurationRoutingRuleRedirect' {hostname} -> hostname) (\s@AwsS3BucketWebsiteConfigurationRoutingRuleRedirect' {} a -> s {hostname = a} :: AwsS3BucketWebsiteConfigurationRoutingRuleRedirect)

-- | The HTTP redirect code to use in the response.
awsS3BucketWebsiteConfigurationRoutingRuleRedirect_httpRedirectCode :: Lens.Lens' AwsS3BucketWebsiteConfigurationRoutingRuleRedirect (Prelude.Maybe Prelude.Text)
awsS3BucketWebsiteConfigurationRoutingRuleRedirect_httpRedirectCode = Lens.lens (\AwsS3BucketWebsiteConfigurationRoutingRuleRedirect' {httpRedirectCode} -> httpRedirectCode) (\s@AwsS3BucketWebsiteConfigurationRoutingRuleRedirect' {} a -> s {httpRedirectCode = a} :: AwsS3BucketWebsiteConfigurationRoutingRuleRedirect)

-- | The protocol to use to redirect the request. By default, uses the
-- protocol from the original request.
awsS3BucketWebsiteConfigurationRoutingRuleRedirect_protocol :: Lens.Lens' AwsS3BucketWebsiteConfigurationRoutingRuleRedirect (Prelude.Maybe Prelude.Text)
awsS3BucketWebsiteConfigurationRoutingRuleRedirect_protocol = Lens.lens (\AwsS3BucketWebsiteConfigurationRoutingRuleRedirect' {protocol} -> protocol) (\s@AwsS3BucketWebsiteConfigurationRoutingRuleRedirect' {} a -> s {protocol = a} :: AwsS3BucketWebsiteConfigurationRoutingRuleRedirect)

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

instance
  Data.FromJSON
    AwsS3BucketWebsiteConfigurationRoutingRuleRedirect
  where
  parseJSON =
    Data.withObject
      "AwsS3BucketWebsiteConfigurationRoutingRuleRedirect"
      ( \x ->
          AwsS3BucketWebsiteConfigurationRoutingRuleRedirect'
            Prelude.<$> (x Data..:? "Hostname")
              Prelude.<*> (x Data..:? "HttpRedirectCode")
              Prelude.<*> (x Data..:? "Protocol")
              Prelude.<*> (x Data..:? "ReplaceKeyPrefixWith")
              Prelude.<*> (x Data..:? "ReplaceKeyWith")
      )

instance
  Prelude.Hashable
    AwsS3BucketWebsiteConfigurationRoutingRuleRedirect
  where
  hashWithSalt
    _salt
    AwsS3BucketWebsiteConfigurationRoutingRuleRedirect' {..} =
      _salt `Prelude.hashWithSalt` hostname
        `Prelude.hashWithSalt` httpRedirectCode
        `Prelude.hashWithSalt` protocol
        `Prelude.hashWithSalt` replaceKeyPrefixWith
        `Prelude.hashWithSalt` replaceKeyWith

instance
  Prelude.NFData
    AwsS3BucketWebsiteConfigurationRoutingRuleRedirect
  where
  rnf
    AwsS3BucketWebsiteConfigurationRoutingRuleRedirect' {..} =
      Prelude.rnf hostname
        `Prelude.seq` Prelude.rnf httpRedirectCode
        `Prelude.seq` Prelude.rnf protocol
        `Prelude.seq` Prelude.rnf replaceKeyPrefixWith
        `Prelude.seq` Prelude.rnf replaceKeyWith

instance
  Data.ToJSON
    AwsS3BucketWebsiteConfigurationRoutingRuleRedirect
  where
  toJSON
    AwsS3BucketWebsiteConfigurationRoutingRuleRedirect' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Hostname" Data..=) Prelude.<$> hostname,
              ("HttpRedirectCode" Data..=)
                Prelude.<$> httpRedirectCode,
              ("Protocol" Data..=) Prelude.<$> protocol,
              ("ReplaceKeyPrefixWith" Data..=)
                Prelude.<$> replaceKeyPrefixWith,
              ("ReplaceKeyWith" Data..=)
                Prelude.<$> replaceKeyWith
            ]
        )
