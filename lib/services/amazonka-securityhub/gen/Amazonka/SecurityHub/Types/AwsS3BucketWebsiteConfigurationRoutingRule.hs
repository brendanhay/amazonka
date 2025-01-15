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
-- Module      : Amazonka.SecurityHub.Types.AwsS3BucketWebsiteConfigurationRoutingRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsS3BucketWebsiteConfigurationRoutingRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsS3BucketWebsiteConfigurationRoutingRuleCondition
import Amazonka.SecurityHub.Types.AwsS3BucketWebsiteConfigurationRoutingRuleRedirect

-- | A rule for redirecting requests to the website.
--
-- /See:/ 'newAwsS3BucketWebsiteConfigurationRoutingRule' smart constructor.
data AwsS3BucketWebsiteConfigurationRoutingRule = AwsS3BucketWebsiteConfigurationRoutingRule'
  { -- | Provides the condition that must be met in order to apply the routing
    -- rule.
    condition :: Prelude.Maybe AwsS3BucketWebsiteConfigurationRoutingRuleCondition,
    -- | Provides the rules to redirect the request if the condition in
    -- @Condition@ is met.
    redirect :: Prelude.Maybe AwsS3BucketWebsiteConfigurationRoutingRuleRedirect
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsS3BucketWebsiteConfigurationRoutingRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'condition', 'awsS3BucketWebsiteConfigurationRoutingRule_condition' - Provides the condition that must be met in order to apply the routing
-- rule.
--
-- 'redirect', 'awsS3BucketWebsiteConfigurationRoutingRule_redirect' - Provides the rules to redirect the request if the condition in
-- @Condition@ is met.
newAwsS3BucketWebsiteConfigurationRoutingRule ::
  AwsS3BucketWebsiteConfigurationRoutingRule
newAwsS3BucketWebsiteConfigurationRoutingRule =
  AwsS3BucketWebsiteConfigurationRoutingRule'
    { condition =
        Prelude.Nothing,
      redirect = Prelude.Nothing
    }

-- | Provides the condition that must be met in order to apply the routing
-- rule.
awsS3BucketWebsiteConfigurationRoutingRule_condition :: Lens.Lens' AwsS3BucketWebsiteConfigurationRoutingRule (Prelude.Maybe AwsS3BucketWebsiteConfigurationRoutingRuleCondition)
awsS3BucketWebsiteConfigurationRoutingRule_condition = Lens.lens (\AwsS3BucketWebsiteConfigurationRoutingRule' {condition} -> condition) (\s@AwsS3BucketWebsiteConfigurationRoutingRule' {} a -> s {condition = a} :: AwsS3BucketWebsiteConfigurationRoutingRule)

-- | Provides the rules to redirect the request if the condition in
-- @Condition@ is met.
awsS3BucketWebsiteConfigurationRoutingRule_redirect :: Lens.Lens' AwsS3BucketWebsiteConfigurationRoutingRule (Prelude.Maybe AwsS3BucketWebsiteConfigurationRoutingRuleRedirect)
awsS3BucketWebsiteConfigurationRoutingRule_redirect = Lens.lens (\AwsS3BucketWebsiteConfigurationRoutingRule' {redirect} -> redirect) (\s@AwsS3BucketWebsiteConfigurationRoutingRule' {} a -> s {redirect = a} :: AwsS3BucketWebsiteConfigurationRoutingRule)

instance
  Data.FromJSON
    AwsS3BucketWebsiteConfigurationRoutingRule
  where
  parseJSON =
    Data.withObject
      "AwsS3BucketWebsiteConfigurationRoutingRule"
      ( \x ->
          AwsS3BucketWebsiteConfigurationRoutingRule'
            Prelude.<$> (x Data..:? "Condition")
            Prelude.<*> (x Data..:? "Redirect")
      )

instance
  Prelude.Hashable
    AwsS3BucketWebsiteConfigurationRoutingRule
  where
  hashWithSalt
    _salt
    AwsS3BucketWebsiteConfigurationRoutingRule' {..} =
      _salt
        `Prelude.hashWithSalt` condition
        `Prelude.hashWithSalt` redirect

instance
  Prelude.NFData
    AwsS3BucketWebsiteConfigurationRoutingRule
  where
  rnf AwsS3BucketWebsiteConfigurationRoutingRule' {..} =
    Prelude.rnf condition `Prelude.seq`
      Prelude.rnf redirect

instance
  Data.ToJSON
    AwsS3BucketWebsiteConfigurationRoutingRule
  where
  toJSON
    AwsS3BucketWebsiteConfigurationRoutingRule' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Condition" Data..=) Prelude.<$> condition,
              ("Redirect" Data..=) Prelude.<$> redirect
            ]
        )
