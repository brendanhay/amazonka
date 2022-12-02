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
-- Module      : Amazonka.AuditManager.Types.SourceKeyword
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.SourceKeyword where

import Amazonka.AuditManager.Types.KeywordInputType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The keyword to search for in CloudTrail logs, Config rules, Security Hub
-- checks, and Amazon Web Services API names.
--
-- To learn more about the supported keywords that you can use when mapping
-- a control data source, see the following pages in the /Audit Manager
-- User Guide/:
--
-- -   <https://docs.aws.amazon.com/audit-manager/latest/userguide/control-data-sources-ash.html Config rules supported by Audit Manager>
--
-- -   <https://docs.aws.amazon.com/audit-manager/latest/userguide/control-data-sources-config.html Security Hub controls supported by Audit Manager>
--
-- -   <https://docs.aws.amazon.com/audit-manager/latest/userguide/control-data-sources-api.html API calls supported by Audit Manager>
--
-- -   <https://docs.aws.amazon.com/audit-manager/latest/userguide/control-data-sources-cloudtrail.html CloudTrail event names supported by Audit Manager>
--
-- /See:/ 'newSourceKeyword' smart constructor.
data SourceKeyword = SourceKeyword'
  { -- | The value of the keyword that\'s used when mapping a control data
    -- source. For example, this can be a CloudTrail event name, a rule name
    -- for Config, a Security Hub control, or the name of an Amazon Web
    -- Services API call.
    --
    -- If you’re mapping a data source to a rule in Config, the @keywordValue@
    -- that you specify depends on the type of rule:
    --
    -- -   For
    --     <https://docs.aws.amazon.com/config/latest/developerguide/evaluate-config_use-managed-rules.html managed rules>,
    --     you can use the rule identifier as the @keywordValue@. You can find
    --     the rule identifier from the
    --     <https://docs.aws.amazon.com/config/latest/developerguide/managed-rules-by-aws-config.html list of Config managed rules>.
    --
    --     -   Managed rule name:
    --         <https://docs.aws.amazon.com/config/latest/developerguide/s3-bucket-acl-prohibited.html s3-bucket-acl-prohibited>
    --
    --         @keywordValue@: @S3_BUCKET_ACL_PROHIBITED@
    --
    -- -   For
    --     <https://docs.aws.amazon.com/config/latest/developerguide/evaluate-config_develop-rules.html custom rules>,
    --     you form the @keywordValue@ by adding the @Custom_@ prefix to the
    --     rule name. This prefix distinguishes the rule from a managed rule.
    --
    --     -   Custom rule name: my-custom-config-rule
    --
    --         @keywordValue@: @Custom_my-custom-config-rule@
    --
    -- -   For
    --     <https://docs.aws.amazon.com/config/latest/developerguide/service-linked-awsconfig-rules.html service-linked rules>,
    --     you form the @keywordValue@ by adding the @Custom_@ prefix to the
    --     rule name. In addition, you remove the suffix ID that appears at the
    --     end of the rule name.
    --
    --     -   Service-linked rule name:
    --         CustomRuleForAccount-conformance-pack-szsm1uv0w
    --
    --         @keywordValue@: @Custom_CustomRuleForAccount-conformance-pack@
    --
    --     -   Service-linked rule name:
    --         OrgConfigRule-s3-bucket-versioning-enabled-dbgzf8ba
    --
    --         @keywordValue@:
    --         @Custom_OrgConfigRule-s3-bucket-versioning-enabled@
    keywordValue :: Prelude.Maybe Prelude.Text,
    -- | The input method for the keyword.
    keywordInputType :: Prelude.Maybe KeywordInputType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SourceKeyword' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keywordValue', 'sourceKeyword_keywordValue' - The value of the keyword that\'s used when mapping a control data
-- source. For example, this can be a CloudTrail event name, a rule name
-- for Config, a Security Hub control, or the name of an Amazon Web
-- Services API call.
--
-- If you’re mapping a data source to a rule in Config, the @keywordValue@
-- that you specify depends on the type of rule:
--
-- -   For
--     <https://docs.aws.amazon.com/config/latest/developerguide/evaluate-config_use-managed-rules.html managed rules>,
--     you can use the rule identifier as the @keywordValue@. You can find
--     the rule identifier from the
--     <https://docs.aws.amazon.com/config/latest/developerguide/managed-rules-by-aws-config.html list of Config managed rules>.
--
--     -   Managed rule name:
--         <https://docs.aws.amazon.com/config/latest/developerguide/s3-bucket-acl-prohibited.html s3-bucket-acl-prohibited>
--
--         @keywordValue@: @S3_BUCKET_ACL_PROHIBITED@
--
-- -   For
--     <https://docs.aws.amazon.com/config/latest/developerguide/evaluate-config_develop-rules.html custom rules>,
--     you form the @keywordValue@ by adding the @Custom_@ prefix to the
--     rule name. This prefix distinguishes the rule from a managed rule.
--
--     -   Custom rule name: my-custom-config-rule
--
--         @keywordValue@: @Custom_my-custom-config-rule@
--
-- -   For
--     <https://docs.aws.amazon.com/config/latest/developerguide/service-linked-awsconfig-rules.html service-linked rules>,
--     you form the @keywordValue@ by adding the @Custom_@ prefix to the
--     rule name. In addition, you remove the suffix ID that appears at the
--     end of the rule name.
--
--     -   Service-linked rule name:
--         CustomRuleForAccount-conformance-pack-szsm1uv0w
--
--         @keywordValue@: @Custom_CustomRuleForAccount-conformance-pack@
--
--     -   Service-linked rule name:
--         OrgConfigRule-s3-bucket-versioning-enabled-dbgzf8ba
--
--         @keywordValue@:
--         @Custom_OrgConfigRule-s3-bucket-versioning-enabled@
--
-- 'keywordInputType', 'sourceKeyword_keywordInputType' - The input method for the keyword.
newSourceKeyword ::
  SourceKeyword
newSourceKeyword =
  SourceKeyword'
    { keywordValue = Prelude.Nothing,
      keywordInputType = Prelude.Nothing
    }

-- | The value of the keyword that\'s used when mapping a control data
-- source. For example, this can be a CloudTrail event name, a rule name
-- for Config, a Security Hub control, or the name of an Amazon Web
-- Services API call.
--
-- If you’re mapping a data source to a rule in Config, the @keywordValue@
-- that you specify depends on the type of rule:
--
-- -   For
--     <https://docs.aws.amazon.com/config/latest/developerguide/evaluate-config_use-managed-rules.html managed rules>,
--     you can use the rule identifier as the @keywordValue@. You can find
--     the rule identifier from the
--     <https://docs.aws.amazon.com/config/latest/developerguide/managed-rules-by-aws-config.html list of Config managed rules>.
--
--     -   Managed rule name:
--         <https://docs.aws.amazon.com/config/latest/developerguide/s3-bucket-acl-prohibited.html s3-bucket-acl-prohibited>
--
--         @keywordValue@: @S3_BUCKET_ACL_PROHIBITED@
--
-- -   For
--     <https://docs.aws.amazon.com/config/latest/developerguide/evaluate-config_develop-rules.html custom rules>,
--     you form the @keywordValue@ by adding the @Custom_@ prefix to the
--     rule name. This prefix distinguishes the rule from a managed rule.
--
--     -   Custom rule name: my-custom-config-rule
--
--         @keywordValue@: @Custom_my-custom-config-rule@
--
-- -   For
--     <https://docs.aws.amazon.com/config/latest/developerguide/service-linked-awsconfig-rules.html service-linked rules>,
--     you form the @keywordValue@ by adding the @Custom_@ prefix to the
--     rule name. In addition, you remove the suffix ID that appears at the
--     end of the rule name.
--
--     -   Service-linked rule name:
--         CustomRuleForAccount-conformance-pack-szsm1uv0w
--
--         @keywordValue@: @Custom_CustomRuleForAccount-conformance-pack@
--
--     -   Service-linked rule name:
--         OrgConfigRule-s3-bucket-versioning-enabled-dbgzf8ba
--
--         @keywordValue@:
--         @Custom_OrgConfigRule-s3-bucket-versioning-enabled@
sourceKeyword_keywordValue :: Lens.Lens' SourceKeyword (Prelude.Maybe Prelude.Text)
sourceKeyword_keywordValue = Lens.lens (\SourceKeyword' {keywordValue} -> keywordValue) (\s@SourceKeyword' {} a -> s {keywordValue = a} :: SourceKeyword)

-- | The input method for the keyword.
sourceKeyword_keywordInputType :: Lens.Lens' SourceKeyword (Prelude.Maybe KeywordInputType)
sourceKeyword_keywordInputType = Lens.lens (\SourceKeyword' {keywordInputType} -> keywordInputType) (\s@SourceKeyword' {} a -> s {keywordInputType = a} :: SourceKeyword)

instance Data.FromJSON SourceKeyword where
  parseJSON =
    Data.withObject
      "SourceKeyword"
      ( \x ->
          SourceKeyword'
            Prelude.<$> (x Data..:? "keywordValue")
            Prelude.<*> (x Data..:? "keywordInputType")
      )

instance Prelude.Hashable SourceKeyword where
  hashWithSalt _salt SourceKeyword' {..} =
    _salt `Prelude.hashWithSalt` keywordValue
      `Prelude.hashWithSalt` keywordInputType

instance Prelude.NFData SourceKeyword where
  rnf SourceKeyword' {..} =
    Prelude.rnf keywordValue
      `Prelude.seq` Prelude.rnf keywordInputType

instance Data.ToJSON SourceKeyword where
  toJSON SourceKeyword' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("keywordValue" Data..=) Prelude.<$> keywordValue,
            ("keywordInputType" Data..=)
              Prelude.<$> keywordInputType
          ]
      )
