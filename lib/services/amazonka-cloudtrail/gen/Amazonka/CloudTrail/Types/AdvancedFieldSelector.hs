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
-- Module      : Amazonka.CloudTrail.Types.AdvancedFieldSelector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudTrail.Types.AdvancedFieldSelector where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A single selector statement in an advanced event selector.
--
-- /See:/ 'newAdvancedFieldSelector' smart constructor.
data AdvancedFieldSelector = AdvancedFieldSelector'
  { -- | An operator that includes events that match the last few characters of
    -- the event record field specified as the value of @Field@.
    endsWith :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | An operator that includes events that match the exact value of the event
    -- record field specified as the value of @Field@. This is the only valid
    -- operator that you can use with the @readOnly@, @eventCategory@, and
    -- @resources.type@ fields.
    equals :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | An operator that excludes events that match the last few characters of
    -- the event record field specified as the value of @Field@.
    notEndsWith :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | An operator that excludes events that match the exact value of the event
    -- record field specified as the value of @Field@.
    notEquals :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | An operator that excludes events that match the first few characters of
    -- the event record field specified as the value of @Field@.
    notStartsWith :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | An operator that includes events that match the first few characters of
    -- the event record field specified as the value of @Field@.
    startsWith :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A field in a CloudTrail event record on which to filter events to be
    -- logged. For event data stores for Config configuration items, Audit
    -- Manager evidence, or non-Amazon Web Services events, the field is used
    -- only for selecting events as filtering is not supported.
    --
    -- For CloudTrail event records, supported fields include @readOnly@,
    -- @eventCategory@, @eventSource@ (for management events), @eventName@,
    -- @resources.type@, and @resources.ARN@.
    --
    -- For event data stores for Config configuration items, Audit Manager
    -- evidence, or non-Amazon Web Services events, the only supported field is
    -- @eventCategory@.
    --
    -- -   __@readOnly@__ - Optional. Can be set to @Equals@ a value of @true@
    --     or @false@. If you do not add this field, CloudTrail logs both
    --     @read@ and @write@ events. A value of @true@ logs only @read@
    --     events. A value of @false@ logs only @write@ events.
    --
    -- -   __@eventSource@__ - For filtering management events only. This can
    --     be set only to @NotEquals@ @kms.amazonaws.com@.
    --
    -- -   __@eventName@__ - Can use any operator. You can use it to ﬁlter in
    --     or ﬁlter out any data event logged to CloudTrail, such as
    --     @PutBucket@ or @GetSnapshotBlock@. You can have multiple values for
    --     this ﬁeld, separated by commas.
    --
    -- -   __@eventCategory@__ - This is required and must be set to @Equals@.
    --
    --     -   For CloudTrail event records, the value must be @Management@ or
    --         @Data@.
    --
    --     -   For Config configuration items, the value must be
    --         @ConfigurationItem@.
    --
    --     -   For Audit Manager evidence, the value must be @Evidence@.
    --
    --     -   For non-Amazon Web Services events, the value must be
    --         @ActivityAuditLog@.
    --
    -- -   __@resources.type@__ - This ﬁeld is required for CloudTrail data
    --     events. @resources.type@ can only use the @Equals@ operator, and the
    --     value can be one of the following:
    --
    --     -   @AWS::DynamoDB::Table@
    --
    --     -   @AWS::Lambda::Function@
    --
    --     -   @AWS::S3::Object@
    --
    --     -   @AWS::CloudTrail::Channel@
    --
    --     -   @AWS::CodeWhisperer::Profile@
    --
    --     -   @AWS::Cognito::IdentityPool@
    --
    --     -   @AWS::DynamoDB::Stream@
    --
    --     -   @AWS::EC2::Snapshot@
    --
    --     -   @AWS::EMRWAL::Workspace@
    --
    --     -   @AWS::FinSpace::Environment@
    --
    --     -   @AWS::Glue::Table@
    --
    --     -   @AWS::GuardDuty::Detector@
    --
    --     -   @AWS::KendraRanking::ExecutionPlan@
    --
    --     -   @AWS::ManagedBlockchain::Node@
    --
    --     -   @AWS::SageMaker::ExperimentTrialComponent@
    --
    --     -   @AWS::SageMaker::FeatureGroup@
    --
    --     -   @AWS::S3::AccessPoint@
    --
    --     -   @AWS::S3ObjectLambda::AccessPoint@
    --
    --     -   @AWS::S3Outposts::Object@
    --
    --     You can have only one @resources.type@ ﬁeld per selector. To log
    --     data events on more than one resource type, add another selector.
    --
    -- -   __@resources.ARN@__ - You can use any operator with @resources.ARN@,
    --     but if you use @Equals@ or @NotEquals@, the value must exactly match
    --     the ARN of a valid resource of the type you\'ve speciﬁed in the
    --     template as the value of resources.type. For example, if
    --     resources.type equals @AWS::S3::Object@, the ARN must be in one of
    --     the following formats. To log all data events for all objects in a
    --     specific S3 bucket, use the @StartsWith@ operator, and include only
    --     the bucket ARN as the matching value.
    --
    --     The trailing slash is intentional; do not exclude it. Replace the
    --     text between less than and greater than symbols (\<>) with
    --     resource-specific information.
    --
    --     -   @arn:\<partition>:s3:::\<bucket_name>\/@
    --
    --     -   @arn:\<partition>:s3:::\<bucket_name>\/\<object_path>\/@
    --
    --     When resources.type equals @AWS::DynamoDB::Table@, and the operator
    --     is set to @Equals@ or @NotEquals@, the ARN must be in the following
    --     format:
    --
    --     -   @arn:\<partition>:dynamodb:\<region>:\<account_ID>:table\/\<table_name>@
    --
    --     When resources.type equals @AWS::Lambda::Function@, and the operator
    --     is set to @Equals@ or @NotEquals@, the ARN must be in the following
    --     format:
    --
    --     -   @arn:\<partition>:lambda:\<region>:\<account_ID>:function:\<function_name>@
    --
    --     When resources.type equals @AWS::CloudTrail::Channel@, and the
    --     operator is set to @Equals@ or @NotEquals@, the ARN must be in the
    --     following format:
    --
    --     -   @arn:\<partition>:cloudtrail:\<region>:\<account_ID>:channel\/\<channel_UUID>@
    --
    --     When resources.type equals @AWS::CodeWhisperer::Profile@, and the
    --     operator is set to @Equals@ or @NotEquals@, the ARN must be in the
    --     following format:
    --
    --     -   @arn:\<partition>:codewhisperer:\<region>:\<account_ID>:profile\/\<profile_ID>@
    --
    --     When resources.type equals @AWS::Cognito::IdentityPool@, and the
    --     operator is set to @Equals@ or @NotEquals@, the ARN must be in the
    --     following format:
    --
    --     -   @arn:\<partition>:cognito-identity:\<region>:\<account_ID>:identitypool\/\<identity_pool_ID>@
    --
    --     When @resources.type@ equals @AWS::DynamoDB::Stream@, and the
    --     operator is set to @Equals@ or @NotEquals@, the ARN must be in the
    --     following format:
    --
    --     -   @arn:\<partition>:dynamodb:\<region>:\<account_ID>:table\/\<table_name>\/stream\/\<date_time>@
    --
    --     When @resources.type@ equals @AWS::EC2::Snapshot@, and the operator
    --     is set to @Equals@ or @NotEquals@, the ARN must be in the following
    --     format:
    --
    --     -   @arn:\<partition>:ec2:\<region>::snapshot\/\<snapshot_ID>@
    --
    --     When @resources.type@ equals @AWS::EMRWAL::Workspace@, and the
    --     operator is set to @Equals@ or @NotEquals@, the ARN must be in the
    --     following format:
    --
    --     -   @arn:\<partition>:emrwal:\<region>::workspace\/\<workspace_name>@
    --
    --     When @resources.type@ equals @AWS::FinSpace::Environment@, and the
    --     operator is set to @Equals@ or @NotEquals@, the ARN must be in the
    --     following format:
    --
    --     -   @arn:\<partition>:finspace:\<region>:\<account_ID>:environment\/\<environment_ID>@
    --
    --     When @resources.type@ equals @AWS::Glue::Table@, and the operator is
    --     set to @Equals@ or @NotEquals@, the ARN must be in the following
    --     format:
    --
    --     -   @arn:\<partition>:glue:\<region>:\<account_ID>:table\/\<database_name>\/\<table_name>@
    --
    --     When @resources.type@ equals @AWS::GuardDuty::Detector@, and the
    --     operator is set to @Equals@ or @NotEquals@, the ARN must be in the
    --     following format:
    --
    --     -   @arn:\<partition>:guardduty:\<region>:\<account_ID>:detector\/\<detector_ID>@
    --
    --     When @resources.type@ equals @AWS::KendraRanking::ExecutionPlan@,
    --     and the operator is set to @Equals@ or @NotEquals@, the ARN must be
    --     in the following format:
    --
    --     -   @arn:\<partition>:kendra-ranking:\<region>:\<account_ID>:rescore-execution-plan\/\<rescore_execution_plan_ID>@
    --
    --     When @resources.type@ equals @AWS::ManagedBlockchain::Node@, and the
    --     operator is set to @Equals@ or @NotEquals@, the ARN must be in the
    --     following format:
    --
    --     -   @arn:\<partition>:managedblockchain:\<region>:\<account_ID>:nodes\/\<node_ID>@
    --
    --     When @resources.type@ equals
    --     @AWS::SageMaker::ExperimentTrialComponent@, and the operator is set
    --     to @Equals@ or @NotEquals@, the ARN must be in the following format:
    --
    --     -   @arn:\<partition>:sagemaker:\<region>:\<account_ID>:experiment-trial-component\/\<experiment_trial_component_name>@
    --
    --     When @resources.type@ equals @AWS::SageMaker::FeatureGroup@, and the
    --     operator is set to @Equals@ or @NotEquals@, the ARN must be in the
    --     following format:
    --
    --     -   @arn:\<partition>:sagemaker:\<region>:\<account_ID>:feature-group\/\<feature_group_name>@
    --
    --     When @resources.type@ equals @AWS::S3::AccessPoint@, and the
    --     operator is set to @Equals@ or @NotEquals@, the ARN must be in one
    --     of the following formats. To log events on all objects in an S3
    --     access point, we recommend that you use only the access point ARN,
    --     don’t include the object path, and use the @StartsWith@ or
    --     @NotStartsWith@ operators.
    --
    --     -   @arn:\<partition>:s3:\<region>:\<account_ID>:accesspoint\/\<access_point_name>@
    --
    --     -   @arn:\<partition>:s3:\<region>:\<account_ID>:accesspoint\/\<access_point_name>\/object\/\<object_path>@
    --
    --     When @resources.type@ equals @AWS::S3ObjectLambda::AccessPoint@, and
    --     the operator is set to @Equals@ or @NotEquals@, the ARN must be in
    --     the following format:
    --
    --     -   @arn:\<partition>:s3-object-lambda:\<region>:\<account_ID>:accesspoint\/\<access_point_name>@
    --
    --     When @resources.type@ equals @AWS::S3Outposts::Object@, and the
    --     operator is set to @Equals@ or @NotEquals@, the ARN must be in the
    --     following format:
    --
    --     -   @arn:\<partition>:s3-outposts:\<region>:\<account_ID>:\<object_path>@
    field :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdvancedFieldSelector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endsWith', 'advancedFieldSelector_endsWith' - An operator that includes events that match the last few characters of
-- the event record field specified as the value of @Field@.
--
-- 'equals', 'advancedFieldSelector_equals' - An operator that includes events that match the exact value of the event
-- record field specified as the value of @Field@. This is the only valid
-- operator that you can use with the @readOnly@, @eventCategory@, and
-- @resources.type@ fields.
--
-- 'notEndsWith', 'advancedFieldSelector_notEndsWith' - An operator that excludes events that match the last few characters of
-- the event record field specified as the value of @Field@.
--
-- 'notEquals', 'advancedFieldSelector_notEquals' - An operator that excludes events that match the exact value of the event
-- record field specified as the value of @Field@.
--
-- 'notStartsWith', 'advancedFieldSelector_notStartsWith' - An operator that excludes events that match the first few characters of
-- the event record field specified as the value of @Field@.
--
-- 'startsWith', 'advancedFieldSelector_startsWith' - An operator that includes events that match the first few characters of
-- the event record field specified as the value of @Field@.
--
-- 'field', 'advancedFieldSelector_field' - A field in a CloudTrail event record on which to filter events to be
-- logged. For event data stores for Config configuration items, Audit
-- Manager evidence, or non-Amazon Web Services events, the field is used
-- only for selecting events as filtering is not supported.
--
-- For CloudTrail event records, supported fields include @readOnly@,
-- @eventCategory@, @eventSource@ (for management events), @eventName@,
-- @resources.type@, and @resources.ARN@.
--
-- For event data stores for Config configuration items, Audit Manager
-- evidence, or non-Amazon Web Services events, the only supported field is
-- @eventCategory@.
--
-- -   __@readOnly@__ - Optional. Can be set to @Equals@ a value of @true@
--     or @false@. If you do not add this field, CloudTrail logs both
--     @read@ and @write@ events. A value of @true@ logs only @read@
--     events. A value of @false@ logs only @write@ events.
--
-- -   __@eventSource@__ - For filtering management events only. This can
--     be set only to @NotEquals@ @kms.amazonaws.com@.
--
-- -   __@eventName@__ - Can use any operator. You can use it to ﬁlter in
--     or ﬁlter out any data event logged to CloudTrail, such as
--     @PutBucket@ or @GetSnapshotBlock@. You can have multiple values for
--     this ﬁeld, separated by commas.
--
-- -   __@eventCategory@__ - This is required and must be set to @Equals@.
--
--     -   For CloudTrail event records, the value must be @Management@ or
--         @Data@.
--
--     -   For Config configuration items, the value must be
--         @ConfigurationItem@.
--
--     -   For Audit Manager evidence, the value must be @Evidence@.
--
--     -   For non-Amazon Web Services events, the value must be
--         @ActivityAuditLog@.
--
-- -   __@resources.type@__ - This ﬁeld is required for CloudTrail data
--     events. @resources.type@ can only use the @Equals@ operator, and the
--     value can be one of the following:
--
--     -   @AWS::DynamoDB::Table@
--
--     -   @AWS::Lambda::Function@
--
--     -   @AWS::S3::Object@
--
--     -   @AWS::CloudTrail::Channel@
--
--     -   @AWS::CodeWhisperer::Profile@
--
--     -   @AWS::Cognito::IdentityPool@
--
--     -   @AWS::DynamoDB::Stream@
--
--     -   @AWS::EC2::Snapshot@
--
--     -   @AWS::EMRWAL::Workspace@
--
--     -   @AWS::FinSpace::Environment@
--
--     -   @AWS::Glue::Table@
--
--     -   @AWS::GuardDuty::Detector@
--
--     -   @AWS::KendraRanking::ExecutionPlan@
--
--     -   @AWS::ManagedBlockchain::Node@
--
--     -   @AWS::SageMaker::ExperimentTrialComponent@
--
--     -   @AWS::SageMaker::FeatureGroup@
--
--     -   @AWS::S3::AccessPoint@
--
--     -   @AWS::S3ObjectLambda::AccessPoint@
--
--     -   @AWS::S3Outposts::Object@
--
--     You can have only one @resources.type@ ﬁeld per selector. To log
--     data events on more than one resource type, add another selector.
--
-- -   __@resources.ARN@__ - You can use any operator with @resources.ARN@,
--     but if you use @Equals@ or @NotEquals@, the value must exactly match
--     the ARN of a valid resource of the type you\'ve speciﬁed in the
--     template as the value of resources.type. For example, if
--     resources.type equals @AWS::S3::Object@, the ARN must be in one of
--     the following formats. To log all data events for all objects in a
--     specific S3 bucket, use the @StartsWith@ operator, and include only
--     the bucket ARN as the matching value.
--
--     The trailing slash is intentional; do not exclude it. Replace the
--     text between less than and greater than symbols (\<>) with
--     resource-specific information.
--
--     -   @arn:\<partition>:s3:::\<bucket_name>\/@
--
--     -   @arn:\<partition>:s3:::\<bucket_name>\/\<object_path>\/@
--
--     When resources.type equals @AWS::DynamoDB::Table@, and the operator
--     is set to @Equals@ or @NotEquals@, the ARN must be in the following
--     format:
--
--     -   @arn:\<partition>:dynamodb:\<region>:\<account_ID>:table\/\<table_name>@
--
--     When resources.type equals @AWS::Lambda::Function@, and the operator
--     is set to @Equals@ or @NotEquals@, the ARN must be in the following
--     format:
--
--     -   @arn:\<partition>:lambda:\<region>:\<account_ID>:function:\<function_name>@
--
--     When resources.type equals @AWS::CloudTrail::Channel@, and the
--     operator is set to @Equals@ or @NotEquals@, the ARN must be in the
--     following format:
--
--     -   @arn:\<partition>:cloudtrail:\<region>:\<account_ID>:channel\/\<channel_UUID>@
--
--     When resources.type equals @AWS::CodeWhisperer::Profile@, and the
--     operator is set to @Equals@ or @NotEquals@, the ARN must be in the
--     following format:
--
--     -   @arn:\<partition>:codewhisperer:\<region>:\<account_ID>:profile\/\<profile_ID>@
--
--     When resources.type equals @AWS::Cognito::IdentityPool@, and the
--     operator is set to @Equals@ or @NotEquals@, the ARN must be in the
--     following format:
--
--     -   @arn:\<partition>:cognito-identity:\<region>:\<account_ID>:identitypool\/\<identity_pool_ID>@
--
--     When @resources.type@ equals @AWS::DynamoDB::Stream@, and the
--     operator is set to @Equals@ or @NotEquals@, the ARN must be in the
--     following format:
--
--     -   @arn:\<partition>:dynamodb:\<region>:\<account_ID>:table\/\<table_name>\/stream\/\<date_time>@
--
--     When @resources.type@ equals @AWS::EC2::Snapshot@, and the operator
--     is set to @Equals@ or @NotEquals@, the ARN must be in the following
--     format:
--
--     -   @arn:\<partition>:ec2:\<region>::snapshot\/\<snapshot_ID>@
--
--     When @resources.type@ equals @AWS::EMRWAL::Workspace@, and the
--     operator is set to @Equals@ or @NotEquals@, the ARN must be in the
--     following format:
--
--     -   @arn:\<partition>:emrwal:\<region>::workspace\/\<workspace_name>@
--
--     When @resources.type@ equals @AWS::FinSpace::Environment@, and the
--     operator is set to @Equals@ or @NotEquals@, the ARN must be in the
--     following format:
--
--     -   @arn:\<partition>:finspace:\<region>:\<account_ID>:environment\/\<environment_ID>@
--
--     When @resources.type@ equals @AWS::Glue::Table@, and the operator is
--     set to @Equals@ or @NotEquals@, the ARN must be in the following
--     format:
--
--     -   @arn:\<partition>:glue:\<region>:\<account_ID>:table\/\<database_name>\/\<table_name>@
--
--     When @resources.type@ equals @AWS::GuardDuty::Detector@, and the
--     operator is set to @Equals@ or @NotEquals@, the ARN must be in the
--     following format:
--
--     -   @arn:\<partition>:guardduty:\<region>:\<account_ID>:detector\/\<detector_ID>@
--
--     When @resources.type@ equals @AWS::KendraRanking::ExecutionPlan@,
--     and the operator is set to @Equals@ or @NotEquals@, the ARN must be
--     in the following format:
--
--     -   @arn:\<partition>:kendra-ranking:\<region>:\<account_ID>:rescore-execution-plan\/\<rescore_execution_plan_ID>@
--
--     When @resources.type@ equals @AWS::ManagedBlockchain::Node@, and the
--     operator is set to @Equals@ or @NotEquals@, the ARN must be in the
--     following format:
--
--     -   @arn:\<partition>:managedblockchain:\<region>:\<account_ID>:nodes\/\<node_ID>@
--
--     When @resources.type@ equals
--     @AWS::SageMaker::ExperimentTrialComponent@, and the operator is set
--     to @Equals@ or @NotEquals@, the ARN must be in the following format:
--
--     -   @arn:\<partition>:sagemaker:\<region>:\<account_ID>:experiment-trial-component\/\<experiment_trial_component_name>@
--
--     When @resources.type@ equals @AWS::SageMaker::FeatureGroup@, and the
--     operator is set to @Equals@ or @NotEquals@, the ARN must be in the
--     following format:
--
--     -   @arn:\<partition>:sagemaker:\<region>:\<account_ID>:feature-group\/\<feature_group_name>@
--
--     When @resources.type@ equals @AWS::S3::AccessPoint@, and the
--     operator is set to @Equals@ or @NotEquals@, the ARN must be in one
--     of the following formats. To log events on all objects in an S3
--     access point, we recommend that you use only the access point ARN,
--     don’t include the object path, and use the @StartsWith@ or
--     @NotStartsWith@ operators.
--
--     -   @arn:\<partition>:s3:\<region>:\<account_ID>:accesspoint\/\<access_point_name>@
--
--     -   @arn:\<partition>:s3:\<region>:\<account_ID>:accesspoint\/\<access_point_name>\/object\/\<object_path>@
--
--     When @resources.type@ equals @AWS::S3ObjectLambda::AccessPoint@, and
--     the operator is set to @Equals@ or @NotEquals@, the ARN must be in
--     the following format:
--
--     -   @arn:\<partition>:s3-object-lambda:\<region>:\<account_ID>:accesspoint\/\<access_point_name>@
--
--     When @resources.type@ equals @AWS::S3Outposts::Object@, and the
--     operator is set to @Equals@ or @NotEquals@, the ARN must be in the
--     following format:
--
--     -   @arn:\<partition>:s3-outposts:\<region>:\<account_ID>:\<object_path>@
newAdvancedFieldSelector ::
  -- | 'field'
  Prelude.Text ->
  AdvancedFieldSelector
newAdvancedFieldSelector pField_ =
  AdvancedFieldSelector'
    { endsWith = Prelude.Nothing,
      equals = Prelude.Nothing,
      notEndsWith = Prelude.Nothing,
      notEquals = Prelude.Nothing,
      notStartsWith = Prelude.Nothing,
      startsWith = Prelude.Nothing,
      field = pField_
    }

-- | An operator that includes events that match the last few characters of
-- the event record field specified as the value of @Field@.
advancedFieldSelector_endsWith :: Lens.Lens' AdvancedFieldSelector (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
advancedFieldSelector_endsWith = Lens.lens (\AdvancedFieldSelector' {endsWith} -> endsWith) (\s@AdvancedFieldSelector' {} a -> s {endsWith = a} :: AdvancedFieldSelector) Prelude.. Lens.mapping Lens.coerced

-- | An operator that includes events that match the exact value of the event
-- record field specified as the value of @Field@. This is the only valid
-- operator that you can use with the @readOnly@, @eventCategory@, and
-- @resources.type@ fields.
advancedFieldSelector_equals :: Lens.Lens' AdvancedFieldSelector (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
advancedFieldSelector_equals = Lens.lens (\AdvancedFieldSelector' {equals} -> equals) (\s@AdvancedFieldSelector' {} a -> s {equals = a} :: AdvancedFieldSelector) Prelude.. Lens.mapping Lens.coerced

-- | An operator that excludes events that match the last few characters of
-- the event record field specified as the value of @Field@.
advancedFieldSelector_notEndsWith :: Lens.Lens' AdvancedFieldSelector (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
advancedFieldSelector_notEndsWith = Lens.lens (\AdvancedFieldSelector' {notEndsWith} -> notEndsWith) (\s@AdvancedFieldSelector' {} a -> s {notEndsWith = a} :: AdvancedFieldSelector) Prelude.. Lens.mapping Lens.coerced

-- | An operator that excludes events that match the exact value of the event
-- record field specified as the value of @Field@.
advancedFieldSelector_notEquals :: Lens.Lens' AdvancedFieldSelector (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
advancedFieldSelector_notEquals = Lens.lens (\AdvancedFieldSelector' {notEquals} -> notEquals) (\s@AdvancedFieldSelector' {} a -> s {notEquals = a} :: AdvancedFieldSelector) Prelude.. Lens.mapping Lens.coerced

-- | An operator that excludes events that match the first few characters of
-- the event record field specified as the value of @Field@.
advancedFieldSelector_notStartsWith :: Lens.Lens' AdvancedFieldSelector (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
advancedFieldSelector_notStartsWith = Lens.lens (\AdvancedFieldSelector' {notStartsWith} -> notStartsWith) (\s@AdvancedFieldSelector' {} a -> s {notStartsWith = a} :: AdvancedFieldSelector) Prelude.. Lens.mapping Lens.coerced

-- | An operator that includes events that match the first few characters of
-- the event record field specified as the value of @Field@.
advancedFieldSelector_startsWith :: Lens.Lens' AdvancedFieldSelector (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
advancedFieldSelector_startsWith = Lens.lens (\AdvancedFieldSelector' {startsWith} -> startsWith) (\s@AdvancedFieldSelector' {} a -> s {startsWith = a} :: AdvancedFieldSelector) Prelude.. Lens.mapping Lens.coerced

-- | A field in a CloudTrail event record on which to filter events to be
-- logged. For event data stores for Config configuration items, Audit
-- Manager evidence, or non-Amazon Web Services events, the field is used
-- only for selecting events as filtering is not supported.
--
-- For CloudTrail event records, supported fields include @readOnly@,
-- @eventCategory@, @eventSource@ (for management events), @eventName@,
-- @resources.type@, and @resources.ARN@.
--
-- For event data stores for Config configuration items, Audit Manager
-- evidence, or non-Amazon Web Services events, the only supported field is
-- @eventCategory@.
--
-- -   __@readOnly@__ - Optional. Can be set to @Equals@ a value of @true@
--     or @false@. If you do not add this field, CloudTrail logs both
--     @read@ and @write@ events. A value of @true@ logs only @read@
--     events. A value of @false@ logs only @write@ events.
--
-- -   __@eventSource@__ - For filtering management events only. This can
--     be set only to @NotEquals@ @kms.amazonaws.com@.
--
-- -   __@eventName@__ - Can use any operator. You can use it to ﬁlter in
--     or ﬁlter out any data event logged to CloudTrail, such as
--     @PutBucket@ or @GetSnapshotBlock@. You can have multiple values for
--     this ﬁeld, separated by commas.
--
-- -   __@eventCategory@__ - This is required and must be set to @Equals@.
--
--     -   For CloudTrail event records, the value must be @Management@ or
--         @Data@.
--
--     -   For Config configuration items, the value must be
--         @ConfigurationItem@.
--
--     -   For Audit Manager evidence, the value must be @Evidence@.
--
--     -   For non-Amazon Web Services events, the value must be
--         @ActivityAuditLog@.
--
-- -   __@resources.type@__ - This ﬁeld is required for CloudTrail data
--     events. @resources.type@ can only use the @Equals@ operator, and the
--     value can be one of the following:
--
--     -   @AWS::DynamoDB::Table@
--
--     -   @AWS::Lambda::Function@
--
--     -   @AWS::S3::Object@
--
--     -   @AWS::CloudTrail::Channel@
--
--     -   @AWS::CodeWhisperer::Profile@
--
--     -   @AWS::Cognito::IdentityPool@
--
--     -   @AWS::DynamoDB::Stream@
--
--     -   @AWS::EC2::Snapshot@
--
--     -   @AWS::EMRWAL::Workspace@
--
--     -   @AWS::FinSpace::Environment@
--
--     -   @AWS::Glue::Table@
--
--     -   @AWS::GuardDuty::Detector@
--
--     -   @AWS::KendraRanking::ExecutionPlan@
--
--     -   @AWS::ManagedBlockchain::Node@
--
--     -   @AWS::SageMaker::ExperimentTrialComponent@
--
--     -   @AWS::SageMaker::FeatureGroup@
--
--     -   @AWS::S3::AccessPoint@
--
--     -   @AWS::S3ObjectLambda::AccessPoint@
--
--     -   @AWS::S3Outposts::Object@
--
--     You can have only one @resources.type@ ﬁeld per selector. To log
--     data events on more than one resource type, add another selector.
--
-- -   __@resources.ARN@__ - You can use any operator with @resources.ARN@,
--     but if you use @Equals@ or @NotEquals@, the value must exactly match
--     the ARN of a valid resource of the type you\'ve speciﬁed in the
--     template as the value of resources.type. For example, if
--     resources.type equals @AWS::S3::Object@, the ARN must be in one of
--     the following formats. To log all data events for all objects in a
--     specific S3 bucket, use the @StartsWith@ operator, and include only
--     the bucket ARN as the matching value.
--
--     The trailing slash is intentional; do not exclude it. Replace the
--     text between less than and greater than symbols (\<>) with
--     resource-specific information.
--
--     -   @arn:\<partition>:s3:::\<bucket_name>\/@
--
--     -   @arn:\<partition>:s3:::\<bucket_name>\/\<object_path>\/@
--
--     When resources.type equals @AWS::DynamoDB::Table@, and the operator
--     is set to @Equals@ or @NotEquals@, the ARN must be in the following
--     format:
--
--     -   @arn:\<partition>:dynamodb:\<region>:\<account_ID>:table\/\<table_name>@
--
--     When resources.type equals @AWS::Lambda::Function@, and the operator
--     is set to @Equals@ or @NotEquals@, the ARN must be in the following
--     format:
--
--     -   @arn:\<partition>:lambda:\<region>:\<account_ID>:function:\<function_name>@
--
--     When resources.type equals @AWS::CloudTrail::Channel@, and the
--     operator is set to @Equals@ or @NotEquals@, the ARN must be in the
--     following format:
--
--     -   @arn:\<partition>:cloudtrail:\<region>:\<account_ID>:channel\/\<channel_UUID>@
--
--     When resources.type equals @AWS::CodeWhisperer::Profile@, and the
--     operator is set to @Equals@ or @NotEquals@, the ARN must be in the
--     following format:
--
--     -   @arn:\<partition>:codewhisperer:\<region>:\<account_ID>:profile\/\<profile_ID>@
--
--     When resources.type equals @AWS::Cognito::IdentityPool@, and the
--     operator is set to @Equals@ or @NotEquals@, the ARN must be in the
--     following format:
--
--     -   @arn:\<partition>:cognito-identity:\<region>:\<account_ID>:identitypool\/\<identity_pool_ID>@
--
--     When @resources.type@ equals @AWS::DynamoDB::Stream@, and the
--     operator is set to @Equals@ or @NotEquals@, the ARN must be in the
--     following format:
--
--     -   @arn:\<partition>:dynamodb:\<region>:\<account_ID>:table\/\<table_name>\/stream\/\<date_time>@
--
--     When @resources.type@ equals @AWS::EC2::Snapshot@, and the operator
--     is set to @Equals@ or @NotEquals@, the ARN must be in the following
--     format:
--
--     -   @arn:\<partition>:ec2:\<region>::snapshot\/\<snapshot_ID>@
--
--     When @resources.type@ equals @AWS::EMRWAL::Workspace@, and the
--     operator is set to @Equals@ or @NotEquals@, the ARN must be in the
--     following format:
--
--     -   @arn:\<partition>:emrwal:\<region>::workspace\/\<workspace_name>@
--
--     When @resources.type@ equals @AWS::FinSpace::Environment@, and the
--     operator is set to @Equals@ or @NotEquals@, the ARN must be in the
--     following format:
--
--     -   @arn:\<partition>:finspace:\<region>:\<account_ID>:environment\/\<environment_ID>@
--
--     When @resources.type@ equals @AWS::Glue::Table@, and the operator is
--     set to @Equals@ or @NotEquals@, the ARN must be in the following
--     format:
--
--     -   @arn:\<partition>:glue:\<region>:\<account_ID>:table\/\<database_name>\/\<table_name>@
--
--     When @resources.type@ equals @AWS::GuardDuty::Detector@, and the
--     operator is set to @Equals@ or @NotEquals@, the ARN must be in the
--     following format:
--
--     -   @arn:\<partition>:guardduty:\<region>:\<account_ID>:detector\/\<detector_ID>@
--
--     When @resources.type@ equals @AWS::KendraRanking::ExecutionPlan@,
--     and the operator is set to @Equals@ or @NotEquals@, the ARN must be
--     in the following format:
--
--     -   @arn:\<partition>:kendra-ranking:\<region>:\<account_ID>:rescore-execution-plan\/\<rescore_execution_plan_ID>@
--
--     When @resources.type@ equals @AWS::ManagedBlockchain::Node@, and the
--     operator is set to @Equals@ or @NotEquals@, the ARN must be in the
--     following format:
--
--     -   @arn:\<partition>:managedblockchain:\<region>:\<account_ID>:nodes\/\<node_ID>@
--
--     When @resources.type@ equals
--     @AWS::SageMaker::ExperimentTrialComponent@, and the operator is set
--     to @Equals@ or @NotEquals@, the ARN must be in the following format:
--
--     -   @arn:\<partition>:sagemaker:\<region>:\<account_ID>:experiment-trial-component\/\<experiment_trial_component_name>@
--
--     When @resources.type@ equals @AWS::SageMaker::FeatureGroup@, and the
--     operator is set to @Equals@ or @NotEquals@, the ARN must be in the
--     following format:
--
--     -   @arn:\<partition>:sagemaker:\<region>:\<account_ID>:feature-group\/\<feature_group_name>@
--
--     When @resources.type@ equals @AWS::S3::AccessPoint@, and the
--     operator is set to @Equals@ or @NotEquals@, the ARN must be in one
--     of the following formats. To log events on all objects in an S3
--     access point, we recommend that you use only the access point ARN,
--     don’t include the object path, and use the @StartsWith@ or
--     @NotStartsWith@ operators.
--
--     -   @arn:\<partition>:s3:\<region>:\<account_ID>:accesspoint\/\<access_point_name>@
--
--     -   @arn:\<partition>:s3:\<region>:\<account_ID>:accesspoint\/\<access_point_name>\/object\/\<object_path>@
--
--     When @resources.type@ equals @AWS::S3ObjectLambda::AccessPoint@, and
--     the operator is set to @Equals@ or @NotEquals@, the ARN must be in
--     the following format:
--
--     -   @arn:\<partition>:s3-object-lambda:\<region>:\<account_ID>:accesspoint\/\<access_point_name>@
--
--     When @resources.type@ equals @AWS::S3Outposts::Object@, and the
--     operator is set to @Equals@ or @NotEquals@, the ARN must be in the
--     following format:
--
--     -   @arn:\<partition>:s3-outposts:\<region>:\<account_ID>:\<object_path>@
advancedFieldSelector_field :: Lens.Lens' AdvancedFieldSelector Prelude.Text
advancedFieldSelector_field = Lens.lens (\AdvancedFieldSelector' {field} -> field) (\s@AdvancedFieldSelector' {} a -> s {field = a} :: AdvancedFieldSelector)

instance Data.FromJSON AdvancedFieldSelector where
  parseJSON =
    Data.withObject
      "AdvancedFieldSelector"
      ( \x ->
          AdvancedFieldSelector'
            Prelude.<$> (x Data..:? "EndsWith")
            Prelude.<*> (x Data..:? "Equals")
            Prelude.<*> (x Data..:? "NotEndsWith")
            Prelude.<*> (x Data..:? "NotEquals")
            Prelude.<*> (x Data..:? "NotStartsWith")
            Prelude.<*> (x Data..:? "StartsWith")
            Prelude.<*> (x Data..: "Field")
      )

instance Prelude.Hashable AdvancedFieldSelector where
  hashWithSalt _salt AdvancedFieldSelector' {..} =
    _salt
      `Prelude.hashWithSalt` endsWith
      `Prelude.hashWithSalt` equals
      `Prelude.hashWithSalt` notEndsWith
      `Prelude.hashWithSalt` notEquals
      `Prelude.hashWithSalt` notStartsWith
      `Prelude.hashWithSalt` startsWith
      `Prelude.hashWithSalt` field

instance Prelude.NFData AdvancedFieldSelector where
  rnf AdvancedFieldSelector' {..} =
    Prelude.rnf endsWith
      `Prelude.seq` Prelude.rnf equals
      `Prelude.seq` Prelude.rnf notEndsWith
      `Prelude.seq` Prelude.rnf notEquals
      `Prelude.seq` Prelude.rnf notStartsWith
      `Prelude.seq` Prelude.rnf startsWith
      `Prelude.seq` Prelude.rnf field

instance Data.ToJSON AdvancedFieldSelector where
  toJSON AdvancedFieldSelector' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EndsWith" Data..=) Prelude.<$> endsWith,
            ("Equals" Data..=) Prelude.<$> equals,
            ("NotEndsWith" Data..=) Prelude.<$> notEndsWith,
            ("NotEquals" Data..=) Prelude.<$> notEquals,
            ("NotStartsWith" Data..=) Prelude.<$> notStartsWith,
            ("StartsWith" Data..=) Prelude.<$> startsWith,
            Prelude.Just ("Field" Data..= field)
          ]
      )
