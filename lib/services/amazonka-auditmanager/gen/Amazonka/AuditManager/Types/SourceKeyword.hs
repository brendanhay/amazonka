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
-- Copyright   : (c) 2013-2023 Brendan Hay
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

-- | A keyword that relates to the control data source.
--
-- For manual evidence, this keyword indicates if the manual evidence is a
-- file or text.
--
-- For automated evidence, this keyword identifies a specific CloudTrail
-- event, Config rule, Security Hub control, or Amazon Web Services API
-- name.
--
-- To learn more about the supported keywords that you can use when mapping
-- a control data source, see the following pages in the /Audit Manager
-- User Guide/:
--
-- -   <https://docs.aws.amazon.com/audit-manager/latest/userguide/control-data-sources-config.html Config rules supported by Audit Manager>
--
-- -   <https://docs.aws.amazon.com/audit-manager/latest/userguide/control-data-sources-ash.html Security Hub controls supported by Audit Manager>
--
-- -   <https://docs.aws.amazon.com/audit-manager/latest/userguide/control-data-sources-api.html API calls supported by Audit Manager>
--
-- -   <https://docs.aws.amazon.com/audit-manager/latest/userguide/control-data-sources-cloudtrail.html CloudTrail event names supported by Audit Manager>
--
-- /See:/ 'newSourceKeyword' smart constructor.
data SourceKeyword = SourceKeyword'
  { -- | The input method for the keyword.
    --
    -- -   @SELECT_FROM_LIST@ is used when mapping a data source for automated
    --     evidence.
    --
    --     -   When @keywordInputType@ is @SELECT_FROM_LIST@, a keyword must be
    --         selected to collect automated evidence. For example, this
    --         keyword can be a CloudTrail event name, a rule name for Config,
    --         a Security Hub control, or the name of an Amazon Web Services
    --         API call.
    --
    -- -   @UPLOAD_FILE@ and @INPUT_TEXT@ are only used when mapping a data
    --     source for manual evidence.
    --
    --     -   When @keywordInputType@ is @UPLOAD_FILE@, a file must be
    --         uploaded as manual evidence.
    --
    --     -   When @keywordInputType@ is @INPUT_TEXT@, text must be entered as
    --         manual evidence.
    keywordInputType :: Prelude.Maybe KeywordInputType,
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
    --     For some rules, the rule identifier is different from the rule name.
    --     For example, the rule name @restricted-ssh@ has the following rule
    --     identifier: @INCOMING_SSH_DISABLED@. Make sure to use the rule
    --     identifier, not the rule name.
    --
    --     Keyword example for managed rules:
    --
    --     -   Managed rule name:
    --         <https://docs.aws.amazon.com/config/latest/developerguide/s3-bucket-acl-prohibited.html s3-bucket-acl-prohibited>
    --
    --         @keywordValue@: @S3_BUCKET_ACL_PROHIBITED@
    --
    -- -   For
    --     <https://docs.aws.amazon.com/config/latest/developerguide/evaluate-config_develop-rules.html custom rules>,
    --     you form the @keywordValue@ by adding the @Custom_@ prefix to the
    --     rule name. This prefix distinguishes the custom rule from a managed
    --     rule.
    --
    --     Keyword example for custom rules:
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
    --     Keyword examples for service-linked rules:
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
    -- The @keywordValue@ is case sensitive. If you enter a value incorrectly,
    -- Audit Manager might not recognize the data source mapping. As a result,
    -- you might not successfully collect evidence from that data source as
    -- intended.
    --
    -- Keep in mind the following requirements, depending on the data source
    -- type that you\'re using.
    --
    -- 1.  For Config:
    --
    --     -   For managed rules, make sure that the @keywordValue@ is the rule
    --         identifier in @ALL_CAPS_WITH_UNDERSCORES@. For example,
    --         @CLOUDWATCH_LOG_GROUP_ENCRYPTED@. For accuracy, we recommend
    --         that you reference the list of
    --         <https://docs.aws.amazon.com/audit-manager/latest/userguide/control-data-sources-config.html supported Config managed rules>.
    --
    --     -   For custom rules, make sure that the @keywordValue@ has the
    --         @Custom_@ prefix followed by the custom rule name. The format of
    --         the custom rule name itself may vary. For accuracy, we recommend
    --         that you visit the
    --         <https://console.aws.amazon.com/config/ Config console> to
    --         verify your custom rule name.
    --
    -- 2.  For Security Hub: The format varies for Security Hub control names.
    --     For accuracy, we recommend that you reference the list of
    --     <https://docs.aws.amazon.com/audit-manager/latest/userguide/control-data-sources-ash.html supported Security Hub controls>.
    --
    -- 3.  For Amazon Web Services API calls: Make sure that the @keywordValue@
    --     is written as @serviceprefix_ActionName@. For example,
    --     @iam_ListGroups@. For accuracy, we recommend that you reference the
    --     list of
    --     <https://docs.aws.amazon.com/audit-manager/latest/userguide/control-data-sources-api.html supported API calls>.
    --
    -- 4.  For CloudTrail: Make sure that the @keywordValue@ is written as
    --     @serviceprefix_ActionName@. For example, @cloudtrail_StartLogging@.
    --     For accuracy, we recommend that you review the Amazon Web Service
    --     prefix and action names in the
    --     <https://docs.aws.amazon.com/service-authorization/latest/reference/reference_policies_actions-resources-contextkeys.html Service Authorization Reference>.
    keywordValue :: Prelude.Maybe Prelude.Text
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
-- 'keywordInputType', 'sourceKeyword_keywordInputType' - The input method for the keyword.
--
-- -   @SELECT_FROM_LIST@ is used when mapping a data source for automated
--     evidence.
--
--     -   When @keywordInputType@ is @SELECT_FROM_LIST@, a keyword must be
--         selected to collect automated evidence. For example, this
--         keyword can be a CloudTrail event name, a rule name for Config,
--         a Security Hub control, or the name of an Amazon Web Services
--         API call.
--
-- -   @UPLOAD_FILE@ and @INPUT_TEXT@ are only used when mapping a data
--     source for manual evidence.
--
--     -   When @keywordInputType@ is @UPLOAD_FILE@, a file must be
--         uploaded as manual evidence.
--
--     -   When @keywordInputType@ is @INPUT_TEXT@, text must be entered as
--         manual evidence.
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
--     For some rules, the rule identifier is different from the rule name.
--     For example, the rule name @restricted-ssh@ has the following rule
--     identifier: @INCOMING_SSH_DISABLED@. Make sure to use the rule
--     identifier, not the rule name.
--
--     Keyword example for managed rules:
--
--     -   Managed rule name:
--         <https://docs.aws.amazon.com/config/latest/developerguide/s3-bucket-acl-prohibited.html s3-bucket-acl-prohibited>
--
--         @keywordValue@: @S3_BUCKET_ACL_PROHIBITED@
--
-- -   For
--     <https://docs.aws.amazon.com/config/latest/developerguide/evaluate-config_develop-rules.html custom rules>,
--     you form the @keywordValue@ by adding the @Custom_@ prefix to the
--     rule name. This prefix distinguishes the custom rule from a managed
--     rule.
--
--     Keyword example for custom rules:
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
--     Keyword examples for service-linked rules:
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
-- The @keywordValue@ is case sensitive. If you enter a value incorrectly,
-- Audit Manager might not recognize the data source mapping. As a result,
-- you might not successfully collect evidence from that data source as
-- intended.
--
-- Keep in mind the following requirements, depending on the data source
-- type that you\'re using.
--
-- 1.  For Config:
--
--     -   For managed rules, make sure that the @keywordValue@ is the rule
--         identifier in @ALL_CAPS_WITH_UNDERSCORES@. For example,
--         @CLOUDWATCH_LOG_GROUP_ENCRYPTED@. For accuracy, we recommend
--         that you reference the list of
--         <https://docs.aws.amazon.com/audit-manager/latest/userguide/control-data-sources-config.html supported Config managed rules>.
--
--     -   For custom rules, make sure that the @keywordValue@ has the
--         @Custom_@ prefix followed by the custom rule name. The format of
--         the custom rule name itself may vary. For accuracy, we recommend
--         that you visit the
--         <https://console.aws.amazon.com/config/ Config console> to
--         verify your custom rule name.
--
-- 2.  For Security Hub: The format varies for Security Hub control names.
--     For accuracy, we recommend that you reference the list of
--     <https://docs.aws.amazon.com/audit-manager/latest/userguide/control-data-sources-ash.html supported Security Hub controls>.
--
-- 3.  For Amazon Web Services API calls: Make sure that the @keywordValue@
--     is written as @serviceprefix_ActionName@. For example,
--     @iam_ListGroups@. For accuracy, we recommend that you reference the
--     list of
--     <https://docs.aws.amazon.com/audit-manager/latest/userguide/control-data-sources-api.html supported API calls>.
--
-- 4.  For CloudTrail: Make sure that the @keywordValue@ is written as
--     @serviceprefix_ActionName@. For example, @cloudtrail_StartLogging@.
--     For accuracy, we recommend that you review the Amazon Web Service
--     prefix and action names in the
--     <https://docs.aws.amazon.com/service-authorization/latest/reference/reference_policies_actions-resources-contextkeys.html Service Authorization Reference>.
newSourceKeyword ::
  SourceKeyword
newSourceKeyword =
  SourceKeyword'
    { keywordInputType = Prelude.Nothing,
      keywordValue = Prelude.Nothing
    }

-- | The input method for the keyword.
--
-- -   @SELECT_FROM_LIST@ is used when mapping a data source for automated
--     evidence.
--
--     -   When @keywordInputType@ is @SELECT_FROM_LIST@, a keyword must be
--         selected to collect automated evidence. For example, this
--         keyword can be a CloudTrail event name, a rule name for Config,
--         a Security Hub control, or the name of an Amazon Web Services
--         API call.
--
-- -   @UPLOAD_FILE@ and @INPUT_TEXT@ are only used when mapping a data
--     source for manual evidence.
--
--     -   When @keywordInputType@ is @UPLOAD_FILE@, a file must be
--         uploaded as manual evidence.
--
--     -   When @keywordInputType@ is @INPUT_TEXT@, text must be entered as
--         manual evidence.
sourceKeyword_keywordInputType :: Lens.Lens' SourceKeyword (Prelude.Maybe KeywordInputType)
sourceKeyword_keywordInputType = Lens.lens (\SourceKeyword' {keywordInputType} -> keywordInputType) (\s@SourceKeyword' {} a -> s {keywordInputType = a} :: SourceKeyword)

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
--     For some rules, the rule identifier is different from the rule name.
--     For example, the rule name @restricted-ssh@ has the following rule
--     identifier: @INCOMING_SSH_DISABLED@. Make sure to use the rule
--     identifier, not the rule name.
--
--     Keyword example for managed rules:
--
--     -   Managed rule name:
--         <https://docs.aws.amazon.com/config/latest/developerguide/s3-bucket-acl-prohibited.html s3-bucket-acl-prohibited>
--
--         @keywordValue@: @S3_BUCKET_ACL_PROHIBITED@
--
-- -   For
--     <https://docs.aws.amazon.com/config/latest/developerguide/evaluate-config_develop-rules.html custom rules>,
--     you form the @keywordValue@ by adding the @Custom_@ prefix to the
--     rule name. This prefix distinguishes the custom rule from a managed
--     rule.
--
--     Keyword example for custom rules:
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
--     Keyword examples for service-linked rules:
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
-- The @keywordValue@ is case sensitive. If you enter a value incorrectly,
-- Audit Manager might not recognize the data source mapping. As a result,
-- you might not successfully collect evidence from that data source as
-- intended.
--
-- Keep in mind the following requirements, depending on the data source
-- type that you\'re using.
--
-- 1.  For Config:
--
--     -   For managed rules, make sure that the @keywordValue@ is the rule
--         identifier in @ALL_CAPS_WITH_UNDERSCORES@. For example,
--         @CLOUDWATCH_LOG_GROUP_ENCRYPTED@. For accuracy, we recommend
--         that you reference the list of
--         <https://docs.aws.amazon.com/audit-manager/latest/userguide/control-data-sources-config.html supported Config managed rules>.
--
--     -   For custom rules, make sure that the @keywordValue@ has the
--         @Custom_@ prefix followed by the custom rule name. The format of
--         the custom rule name itself may vary. For accuracy, we recommend
--         that you visit the
--         <https://console.aws.amazon.com/config/ Config console> to
--         verify your custom rule name.
--
-- 2.  For Security Hub: The format varies for Security Hub control names.
--     For accuracy, we recommend that you reference the list of
--     <https://docs.aws.amazon.com/audit-manager/latest/userguide/control-data-sources-ash.html supported Security Hub controls>.
--
-- 3.  For Amazon Web Services API calls: Make sure that the @keywordValue@
--     is written as @serviceprefix_ActionName@. For example,
--     @iam_ListGroups@. For accuracy, we recommend that you reference the
--     list of
--     <https://docs.aws.amazon.com/audit-manager/latest/userguide/control-data-sources-api.html supported API calls>.
--
-- 4.  For CloudTrail: Make sure that the @keywordValue@ is written as
--     @serviceprefix_ActionName@. For example, @cloudtrail_StartLogging@.
--     For accuracy, we recommend that you review the Amazon Web Service
--     prefix and action names in the
--     <https://docs.aws.amazon.com/service-authorization/latest/reference/reference_policies_actions-resources-contextkeys.html Service Authorization Reference>.
sourceKeyword_keywordValue :: Lens.Lens' SourceKeyword (Prelude.Maybe Prelude.Text)
sourceKeyword_keywordValue = Lens.lens (\SourceKeyword' {keywordValue} -> keywordValue) (\s@SourceKeyword' {} a -> s {keywordValue = a} :: SourceKeyword)

instance Data.FromJSON SourceKeyword where
  parseJSON =
    Data.withObject
      "SourceKeyword"
      ( \x ->
          SourceKeyword'
            Prelude.<$> (x Data..:? "keywordInputType")
            Prelude.<*> (x Data..:? "keywordValue")
      )

instance Prelude.Hashable SourceKeyword where
  hashWithSalt _salt SourceKeyword' {..} =
    _salt
      `Prelude.hashWithSalt` keywordInputType
      `Prelude.hashWithSalt` keywordValue

instance Prelude.NFData SourceKeyword where
  rnf SourceKeyword' {..} =
    Prelude.rnf keywordInputType
      `Prelude.seq` Prelude.rnf keywordValue

instance Data.ToJSON SourceKeyword where
  toJSON SourceKeyword' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("keywordInputType" Data..=)
              Prelude.<$> keywordInputType,
            ("keywordValue" Data..=) Prelude.<$> keywordValue
          ]
      )
