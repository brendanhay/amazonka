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
-- Module      : Amazonka.SecurityHub.Types.AwsS3BucketWebsiteConfigurationRoutingRuleCondition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsS3BucketWebsiteConfigurationRoutingRuleCondition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The condition that must be met in order to apply the routing rule.
--
-- /See:/ 'newAwsS3BucketWebsiteConfigurationRoutingRuleCondition' smart constructor.
data AwsS3BucketWebsiteConfigurationRoutingRuleCondition = AwsS3BucketWebsiteConfigurationRoutingRuleCondition'
  { -- | Indicates to redirect the request if the HTTP error code matches this
    -- value.
    httpErrorCodeReturnedEquals :: Prelude.Maybe Prelude.Text,
    -- | Indicates to redirect the request if the key prefix matches this value.
    keyPrefixEquals :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsS3BucketWebsiteConfigurationRoutingRuleCondition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpErrorCodeReturnedEquals', 'awsS3BucketWebsiteConfigurationRoutingRuleCondition_httpErrorCodeReturnedEquals' - Indicates to redirect the request if the HTTP error code matches this
-- value.
--
-- 'keyPrefixEquals', 'awsS3BucketWebsiteConfigurationRoutingRuleCondition_keyPrefixEquals' - Indicates to redirect the request if the key prefix matches this value.
newAwsS3BucketWebsiteConfigurationRoutingRuleCondition ::
  AwsS3BucketWebsiteConfigurationRoutingRuleCondition
newAwsS3BucketWebsiteConfigurationRoutingRuleCondition =
  AwsS3BucketWebsiteConfigurationRoutingRuleCondition'
    { httpErrorCodeReturnedEquals =
        Prelude.Nothing,
      keyPrefixEquals =
        Prelude.Nothing
    }

-- | Indicates to redirect the request if the HTTP error code matches this
-- value.
awsS3BucketWebsiteConfigurationRoutingRuleCondition_httpErrorCodeReturnedEquals :: Lens.Lens' AwsS3BucketWebsiteConfigurationRoutingRuleCondition (Prelude.Maybe Prelude.Text)
awsS3BucketWebsiteConfigurationRoutingRuleCondition_httpErrorCodeReturnedEquals = Lens.lens (\AwsS3BucketWebsiteConfigurationRoutingRuleCondition' {httpErrorCodeReturnedEquals} -> httpErrorCodeReturnedEquals) (\s@AwsS3BucketWebsiteConfigurationRoutingRuleCondition' {} a -> s {httpErrorCodeReturnedEquals = a} :: AwsS3BucketWebsiteConfigurationRoutingRuleCondition)

-- | Indicates to redirect the request if the key prefix matches this value.
awsS3BucketWebsiteConfigurationRoutingRuleCondition_keyPrefixEquals :: Lens.Lens' AwsS3BucketWebsiteConfigurationRoutingRuleCondition (Prelude.Maybe Prelude.Text)
awsS3BucketWebsiteConfigurationRoutingRuleCondition_keyPrefixEquals = Lens.lens (\AwsS3BucketWebsiteConfigurationRoutingRuleCondition' {keyPrefixEquals} -> keyPrefixEquals) (\s@AwsS3BucketWebsiteConfigurationRoutingRuleCondition' {} a -> s {keyPrefixEquals = a} :: AwsS3BucketWebsiteConfigurationRoutingRuleCondition)

instance
  Data.FromJSON
    AwsS3BucketWebsiteConfigurationRoutingRuleCondition
  where
  parseJSON =
    Data.withObject
      "AwsS3BucketWebsiteConfigurationRoutingRuleCondition"
      ( \x ->
          AwsS3BucketWebsiteConfigurationRoutingRuleCondition'
            Prelude.<$> (x Data..:? "HttpErrorCodeReturnedEquals")
            Prelude.<*> (x Data..:? "KeyPrefixEquals")
      )

instance
  Prelude.Hashable
    AwsS3BucketWebsiteConfigurationRoutingRuleCondition
  where
  hashWithSalt
    _salt
    AwsS3BucketWebsiteConfigurationRoutingRuleCondition' {..} =
      _salt
        `Prelude.hashWithSalt` httpErrorCodeReturnedEquals
        `Prelude.hashWithSalt` keyPrefixEquals

instance
  Prelude.NFData
    AwsS3BucketWebsiteConfigurationRoutingRuleCondition
  where
  rnf
    AwsS3BucketWebsiteConfigurationRoutingRuleCondition' {..} =
      Prelude.rnf httpErrorCodeReturnedEquals
        `Prelude.seq` Prelude.rnf keyPrefixEquals

instance
  Data.ToJSON
    AwsS3BucketWebsiteConfigurationRoutingRuleCondition
  where
  toJSON
    AwsS3BucketWebsiteConfigurationRoutingRuleCondition' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("HttpErrorCodeReturnedEquals" Data..=)
                Prelude.<$> httpErrorCodeReturnedEquals,
              ("KeyPrefixEquals" Data..=)
                Prelude.<$> keyPrefixEquals
            ]
        )
