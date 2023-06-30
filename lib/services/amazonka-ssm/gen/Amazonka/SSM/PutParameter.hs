{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SSM.PutParameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Add a parameter to the system.
module Amazonka.SSM.PutParameter
  ( -- * Creating a Request
    PutParameter (..),
    newPutParameter,

    -- * Request Lenses
    putParameter_allowedPattern,
    putParameter_dataType,
    putParameter_description,
    putParameter_keyId,
    putParameter_overwrite,
    putParameter_policies,
    putParameter_tags,
    putParameter_tier,
    putParameter_type,
    putParameter_name,
    putParameter_value,

    -- * Destructuring the Response
    PutParameterResponse (..),
    newPutParameterResponse,

    -- * Response Lenses
    putParameterResponse_tier,
    putParameterResponse_httpStatus,
    putParameterResponse_version,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newPutParameter' smart constructor.
data PutParameter = PutParameter'
  { -- | A regular expression used to validate the parameter value. For example,
    -- for String types with values restricted to numbers, you can specify the
    -- following: AllowedPattern=^\\d+$
    allowedPattern :: Prelude.Maybe Prelude.Text,
    -- | The data type for a @String@ parameter. Supported data types include
    -- plain text and Amazon Machine Image (AMI) IDs.
    --
    -- __The following data type values are supported.__
    --
    -- -   @text@
    --
    -- -   @aws:ec2:image@
    --
    -- -   @aws:ssm:integration@
    --
    -- When you create a @String@ parameter and specify @aws:ec2:image@, Amazon
    -- Web Services Systems Manager validates the parameter value is in the
    -- required format, such as @ami-12345abcdeEXAMPLE@, and that the specified
    -- AMI is available in your Amazon Web Services account. For more
    -- information, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-store-ec2-aliases.html Native parameter support for Amazon Machine Image (AMI) IDs>
    -- in the /Amazon Web Services Systems Manager User Guide/.
    dataType :: Prelude.Maybe Prelude.Text,
    -- | Information about the parameter that you want to add to the system.
    -- Optional but recommended.
    --
    -- Don\'t enter personally identifiable information in this field.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Key Management Service (KMS) ID that you want to use to encrypt a
    -- parameter. Either the default KMS key automatically assigned to your
    -- Amazon Web Services account or a custom key. Required for parameters
    -- that use the @SecureString@ data type.
    --
    -- If you don\'t specify a key ID, the system uses the default key
    -- associated with your Amazon Web Services account.
    --
    -- -   To use your default KMS key, choose the @SecureString@ data type,
    --     and do /not/ specify the @Key ID@ when you create the parameter. The
    --     system automatically populates @Key ID@ with your default KMS key.
    --
    -- -   To use a custom KMS key, choose the @SecureString@ data type with
    --     the @Key ID@ parameter.
    keyId :: Prelude.Maybe Prelude.Text,
    -- | Overwrite an existing parameter. The default value is @false@.
    overwrite :: Prelude.Maybe Prelude.Bool,
    -- | One or more policies to apply to a parameter. This operation takes a
    -- JSON array. Parameter Store, a capability of Amazon Web Services Systems
    -- Manager supports the following policy types:
    --
    -- Expiration: This policy deletes the parameter after it expires. When you
    -- create the policy, you specify the expiration date. You can update the
    -- expiration date and time by updating the policy. Updating the
    -- /parameter/ doesn\'t affect the expiration date and time. When the
    -- expiration time is reached, Parameter Store deletes the parameter.
    --
    -- ExpirationNotification: This policy initiates an event in Amazon
    -- CloudWatch Events that notifies you about the expiration. By using this
    -- policy, you can receive notification before or after the expiration time
    -- is reached, in units of days or hours.
    --
    -- NoChangeNotification: This policy initiates a CloudWatch Events event if
    -- a parameter hasn\'t been modified for a specified period of time. This
    -- policy type is useful when, for example, a secret needs to be changed
    -- within a period of time, but it hasn\'t been changed.
    --
    -- All existing policies are preserved until you send new policies or an
    -- empty policy. For more information about parameter policies, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-store-policies.html Assigning parameter policies>.
    policies :: Prelude.Maybe Prelude.Text,
    -- | Optional metadata that you assign to a resource. Tags enable you to
    -- categorize a resource in different ways, such as by purpose, owner, or
    -- environment. For example, you might want to tag a Systems Manager
    -- parameter to identify the type of resource to which it applies, the
    -- environment, or the type of configuration data referenced by the
    -- parameter. In this case, you could specify the following key-value
    -- pairs:
    --
    -- -   @Key=Resource,Value=S3bucket@
    --
    -- -   @Key=OS,Value=Windows@
    --
    -- -   @Key=ParameterType,Value=LicenseKey@
    --
    -- To add tags to an existing Systems Manager parameter, use the
    -- AddTagsToResource operation.
    tags :: Prelude.Maybe [Tag],
    -- | The parameter tier to assign to a parameter.
    --
    -- Parameter Store offers a standard tier and an advanced tier for
    -- parameters. Standard parameters have a content size limit of 4 KB and
    -- can\'t be configured to use parameter policies. You can create a maximum
    -- of 10,000 standard parameters for each Region in an Amazon Web Services
    -- account. Standard parameters are offered at no additional cost.
    --
    -- Advanced parameters have a content size limit of 8 KB and can be
    -- configured to use parameter policies. You can create a maximum of
    -- 100,000 advanced parameters for each Region in an Amazon Web Services
    -- account. Advanced parameters incur a charge. For more information, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-store-advanced-parameters.html Standard and advanced parameter tiers>
    -- in the /Amazon Web Services Systems Manager User Guide/.
    --
    -- You can change a standard parameter to an advanced parameter any time.
    -- But you can\'t revert an advanced parameter to a standard parameter.
    -- Reverting an advanced parameter to a standard parameter would result in
    -- data loss because the system would truncate the size of the parameter
    -- from 8 KB to 4 KB. Reverting would also remove any policies attached to
    -- the parameter. Lastly, advanced parameters use a different form of
    -- encryption than standard parameters.
    --
    -- If you no longer need an advanced parameter, or if you no longer want to
    -- incur charges for an advanced parameter, you must delete it and recreate
    -- it as a new standard parameter.
    --
    -- __Using the Default Tier Configuration__
    --
    -- In @PutParameter@ requests, you can specify the tier to create the
    -- parameter in. Whenever you specify a tier in the request, Parameter
    -- Store creates or updates the parameter according to that request.
    -- However, if you don\'t specify a tier in a request, Parameter Store
    -- assigns the tier based on the current Parameter Store default tier
    -- configuration.
    --
    -- The default tier when you begin using Parameter Store is the
    -- standard-parameter tier. If you use the advanced-parameter tier, you can
    -- specify one of the following as the default:
    --
    -- -   __Advanced__: With this option, Parameter Store evaluates all
    --     requests as advanced parameters.
    --
    -- -   __Intelligent-Tiering__: With this option, Parameter Store evaluates
    --     each request to determine if the parameter is standard or advanced.
    --
    --     If the request doesn\'t include any options that require an advanced
    --     parameter, the parameter is created in the standard-parameter tier.
    --     If one or more options requiring an advanced parameter are included
    --     in the request, Parameter Store create a parameter in the
    --     advanced-parameter tier.
    --
    --     This approach helps control your parameter-related costs by always
    --     creating standard parameters unless an advanced parameter is
    --     necessary.
    --
    -- Options that require an advanced parameter include the following:
    --
    -- -   The content size of the parameter is more than 4 KB.
    --
    -- -   The parameter uses a parameter policy.
    --
    -- -   More than 10,000 parameters already exist in your Amazon Web
    --     Services account in the current Amazon Web Services Region.
    --
    -- For more information about configuring the default tier option, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/ps-default-tier.html Specifying a default parameter tier>
    -- in the /Amazon Web Services Systems Manager User Guide/.
    tier :: Prelude.Maybe ParameterTier,
    -- | The type of parameter that you want to add to the system.
    --
    -- @SecureString@ isn\'t currently supported for CloudFormation templates.
    --
    -- Items in a @StringList@ must be separated by a comma (,). You can\'t use
    -- other punctuation or special character to escape items in the list. If
    -- you have a parameter value that requires a comma, then use the @String@
    -- data type.
    --
    -- Specifying a parameter type isn\'t required when updating a parameter.
    -- You must specify a parameter type when creating a parameter.
    type' :: Prelude.Maybe ParameterType,
    -- | The fully qualified name of the parameter that you want to add to the
    -- system. The fully qualified name includes the complete hierarchy of the
    -- parameter path and name. For parameters in a hierarchy, you must include
    -- a leading forward slash character (\/) when you create or reference a
    -- parameter. For example: @\/Dev\/DBServer\/MySQL\/db-string13@
    --
    -- Naming Constraints:
    --
    -- -   Parameter names are case sensitive.
    --
    -- -   A parameter name must be unique within an Amazon Web Services Region
    --
    -- -   A parameter name can\'t be prefixed with \"@aws@\" or \"@ssm@\"
    --     (case-insensitive).
    --
    -- -   Parameter names can include only the following symbols and letters:
    --     @a-zA-Z0-9_.-@
    --
    --     In addition, the slash character ( \/ ) is used to delineate
    --     hierarchies in parameter names. For example:
    --     @\/Dev\/Production\/East\/Project-ABC\/MyParameter@
    --
    -- -   A parameter name can\'t include spaces.
    --
    -- -   Parameter hierarchies are limited to a maximum depth of fifteen
    --     levels.
    --
    -- For additional information about valid values for parameter names, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-paramstore-su-create.html Creating Systems Manager parameters>
    -- in the /Amazon Web Services Systems Manager User Guide/.
    --
    -- The maximum length constraint of 2048 characters listed below includes
    -- 1037 characters reserved for internal use by Systems Manager. The
    -- maximum length for a parameter name that you create is 1011 characters.
    -- This includes the characters in the ARN that precede the name you
    -- specify, such as @arn:aws:ssm:us-east-2:111122223333:parameter\/@.
    name :: Prelude.Text,
    -- | The parameter value that you want to add to the system. Standard
    -- parameters have a value limit of 4 KB. Advanced parameters have a value
    -- limit of 8 KB.
    --
    -- Parameters can\'t be referenced or nested in the values of other
    -- parameters. You can\'t include @{{}}@ or @{{ssm:@/@parameter-name@/@}}@
    -- in a parameter value.
    value :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowedPattern', 'putParameter_allowedPattern' - A regular expression used to validate the parameter value. For example,
-- for String types with values restricted to numbers, you can specify the
-- following: AllowedPattern=^\\d+$
--
-- 'dataType', 'putParameter_dataType' - The data type for a @String@ parameter. Supported data types include
-- plain text and Amazon Machine Image (AMI) IDs.
--
-- __The following data type values are supported.__
--
-- -   @text@
--
-- -   @aws:ec2:image@
--
-- -   @aws:ssm:integration@
--
-- When you create a @String@ parameter and specify @aws:ec2:image@, Amazon
-- Web Services Systems Manager validates the parameter value is in the
-- required format, such as @ami-12345abcdeEXAMPLE@, and that the specified
-- AMI is available in your Amazon Web Services account. For more
-- information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-store-ec2-aliases.html Native parameter support for Amazon Machine Image (AMI) IDs>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- 'description', 'putParameter_description' - Information about the parameter that you want to add to the system.
-- Optional but recommended.
--
-- Don\'t enter personally identifiable information in this field.
--
-- 'keyId', 'putParameter_keyId' - The Key Management Service (KMS) ID that you want to use to encrypt a
-- parameter. Either the default KMS key automatically assigned to your
-- Amazon Web Services account or a custom key. Required for parameters
-- that use the @SecureString@ data type.
--
-- If you don\'t specify a key ID, the system uses the default key
-- associated with your Amazon Web Services account.
--
-- -   To use your default KMS key, choose the @SecureString@ data type,
--     and do /not/ specify the @Key ID@ when you create the parameter. The
--     system automatically populates @Key ID@ with your default KMS key.
--
-- -   To use a custom KMS key, choose the @SecureString@ data type with
--     the @Key ID@ parameter.
--
-- 'overwrite', 'putParameter_overwrite' - Overwrite an existing parameter. The default value is @false@.
--
-- 'policies', 'putParameter_policies' - One or more policies to apply to a parameter. This operation takes a
-- JSON array. Parameter Store, a capability of Amazon Web Services Systems
-- Manager supports the following policy types:
--
-- Expiration: This policy deletes the parameter after it expires. When you
-- create the policy, you specify the expiration date. You can update the
-- expiration date and time by updating the policy. Updating the
-- /parameter/ doesn\'t affect the expiration date and time. When the
-- expiration time is reached, Parameter Store deletes the parameter.
--
-- ExpirationNotification: This policy initiates an event in Amazon
-- CloudWatch Events that notifies you about the expiration. By using this
-- policy, you can receive notification before or after the expiration time
-- is reached, in units of days or hours.
--
-- NoChangeNotification: This policy initiates a CloudWatch Events event if
-- a parameter hasn\'t been modified for a specified period of time. This
-- policy type is useful when, for example, a secret needs to be changed
-- within a period of time, but it hasn\'t been changed.
--
-- All existing policies are preserved until you send new policies or an
-- empty policy. For more information about parameter policies, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-store-policies.html Assigning parameter policies>.
--
-- 'tags', 'putParameter_tags' - Optional metadata that you assign to a resource. Tags enable you to
-- categorize a resource in different ways, such as by purpose, owner, or
-- environment. For example, you might want to tag a Systems Manager
-- parameter to identify the type of resource to which it applies, the
-- environment, or the type of configuration data referenced by the
-- parameter. In this case, you could specify the following key-value
-- pairs:
--
-- -   @Key=Resource,Value=S3bucket@
--
-- -   @Key=OS,Value=Windows@
--
-- -   @Key=ParameterType,Value=LicenseKey@
--
-- To add tags to an existing Systems Manager parameter, use the
-- AddTagsToResource operation.
--
-- 'tier', 'putParameter_tier' - The parameter tier to assign to a parameter.
--
-- Parameter Store offers a standard tier and an advanced tier for
-- parameters. Standard parameters have a content size limit of 4 KB and
-- can\'t be configured to use parameter policies. You can create a maximum
-- of 10,000 standard parameters for each Region in an Amazon Web Services
-- account. Standard parameters are offered at no additional cost.
--
-- Advanced parameters have a content size limit of 8 KB and can be
-- configured to use parameter policies. You can create a maximum of
-- 100,000 advanced parameters for each Region in an Amazon Web Services
-- account. Advanced parameters incur a charge. For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-store-advanced-parameters.html Standard and advanced parameter tiers>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- You can change a standard parameter to an advanced parameter any time.
-- But you can\'t revert an advanced parameter to a standard parameter.
-- Reverting an advanced parameter to a standard parameter would result in
-- data loss because the system would truncate the size of the parameter
-- from 8 KB to 4 KB. Reverting would also remove any policies attached to
-- the parameter. Lastly, advanced parameters use a different form of
-- encryption than standard parameters.
--
-- If you no longer need an advanced parameter, or if you no longer want to
-- incur charges for an advanced parameter, you must delete it and recreate
-- it as a new standard parameter.
--
-- __Using the Default Tier Configuration__
--
-- In @PutParameter@ requests, you can specify the tier to create the
-- parameter in. Whenever you specify a tier in the request, Parameter
-- Store creates or updates the parameter according to that request.
-- However, if you don\'t specify a tier in a request, Parameter Store
-- assigns the tier based on the current Parameter Store default tier
-- configuration.
--
-- The default tier when you begin using Parameter Store is the
-- standard-parameter tier. If you use the advanced-parameter tier, you can
-- specify one of the following as the default:
--
-- -   __Advanced__: With this option, Parameter Store evaluates all
--     requests as advanced parameters.
--
-- -   __Intelligent-Tiering__: With this option, Parameter Store evaluates
--     each request to determine if the parameter is standard or advanced.
--
--     If the request doesn\'t include any options that require an advanced
--     parameter, the parameter is created in the standard-parameter tier.
--     If one or more options requiring an advanced parameter are included
--     in the request, Parameter Store create a parameter in the
--     advanced-parameter tier.
--
--     This approach helps control your parameter-related costs by always
--     creating standard parameters unless an advanced parameter is
--     necessary.
--
-- Options that require an advanced parameter include the following:
--
-- -   The content size of the parameter is more than 4 KB.
--
-- -   The parameter uses a parameter policy.
--
-- -   More than 10,000 parameters already exist in your Amazon Web
--     Services account in the current Amazon Web Services Region.
--
-- For more information about configuring the default tier option, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/ps-default-tier.html Specifying a default parameter tier>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- 'type'', 'putParameter_type' - The type of parameter that you want to add to the system.
--
-- @SecureString@ isn\'t currently supported for CloudFormation templates.
--
-- Items in a @StringList@ must be separated by a comma (,). You can\'t use
-- other punctuation or special character to escape items in the list. If
-- you have a parameter value that requires a comma, then use the @String@
-- data type.
--
-- Specifying a parameter type isn\'t required when updating a parameter.
-- You must specify a parameter type when creating a parameter.
--
-- 'name', 'putParameter_name' - The fully qualified name of the parameter that you want to add to the
-- system. The fully qualified name includes the complete hierarchy of the
-- parameter path and name. For parameters in a hierarchy, you must include
-- a leading forward slash character (\/) when you create or reference a
-- parameter. For example: @\/Dev\/DBServer\/MySQL\/db-string13@
--
-- Naming Constraints:
--
-- -   Parameter names are case sensitive.
--
-- -   A parameter name must be unique within an Amazon Web Services Region
--
-- -   A parameter name can\'t be prefixed with \"@aws@\" or \"@ssm@\"
--     (case-insensitive).
--
-- -   Parameter names can include only the following symbols and letters:
--     @a-zA-Z0-9_.-@
--
--     In addition, the slash character ( \/ ) is used to delineate
--     hierarchies in parameter names. For example:
--     @\/Dev\/Production\/East\/Project-ABC\/MyParameter@
--
-- -   A parameter name can\'t include spaces.
--
-- -   Parameter hierarchies are limited to a maximum depth of fifteen
--     levels.
--
-- For additional information about valid values for parameter names, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-paramstore-su-create.html Creating Systems Manager parameters>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- The maximum length constraint of 2048 characters listed below includes
-- 1037 characters reserved for internal use by Systems Manager. The
-- maximum length for a parameter name that you create is 1011 characters.
-- This includes the characters in the ARN that precede the name you
-- specify, such as @arn:aws:ssm:us-east-2:111122223333:parameter\/@.
--
-- 'value', 'putParameter_value' - The parameter value that you want to add to the system. Standard
-- parameters have a value limit of 4 KB. Advanced parameters have a value
-- limit of 8 KB.
--
-- Parameters can\'t be referenced or nested in the values of other
-- parameters. You can\'t include @{{}}@ or @{{ssm:@/@parameter-name@/@}}@
-- in a parameter value.
newPutParameter ::
  -- | 'name'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  PutParameter
newPutParameter pName_ pValue_ =
  PutParameter'
    { allowedPattern = Prelude.Nothing,
      dataType = Prelude.Nothing,
      description = Prelude.Nothing,
      keyId = Prelude.Nothing,
      overwrite = Prelude.Nothing,
      policies = Prelude.Nothing,
      tags = Prelude.Nothing,
      tier = Prelude.Nothing,
      type' = Prelude.Nothing,
      name = pName_,
      value = Data._Sensitive Lens.# pValue_
    }

-- | A regular expression used to validate the parameter value. For example,
-- for String types with values restricted to numbers, you can specify the
-- following: AllowedPattern=^\\d+$
putParameter_allowedPattern :: Lens.Lens' PutParameter (Prelude.Maybe Prelude.Text)
putParameter_allowedPattern = Lens.lens (\PutParameter' {allowedPattern} -> allowedPattern) (\s@PutParameter' {} a -> s {allowedPattern = a} :: PutParameter)

-- | The data type for a @String@ parameter. Supported data types include
-- plain text and Amazon Machine Image (AMI) IDs.
--
-- __The following data type values are supported.__
--
-- -   @text@
--
-- -   @aws:ec2:image@
--
-- -   @aws:ssm:integration@
--
-- When you create a @String@ parameter and specify @aws:ec2:image@, Amazon
-- Web Services Systems Manager validates the parameter value is in the
-- required format, such as @ami-12345abcdeEXAMPLE@, and that the specified
-- AMI is available in your Amazon Web Services account. For more
-- information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-store-ec2-aliases.html Native parameter support for Amazon Machine Image (AMI) IDs>
-- in the /Amazon Web Services Systems Manager User Guide/.
putParameter_dataType :: Lens.Lens' PutParameter (Prelude.Maybe Prelude.Text)
putParameter_dataType = Lens.lens (\PutParameter' {dataType} -> dataType) (\s@PutParameter' {} a -> s {dataType = a} :: PutParameter)

-- | Information about the parameter that you want to add to the system.
-- Optional but recommended.
--
-- Don\'t enter personally identifiable information in this field.
putParameter_description :: Lens.Lens' PutParameter (Prelude.Maybe Prelude.Text)
putParameter_description = Lens.lens (\PutParameter' {description} -> description) (\s@PutParameter' {} a -> s {description = a} :: PutParameter)

-- | The Key Management Service (KMS) ID that you want to use to encrypt a
-- parameter. Either the default KMS key automatically assigned to your
-- Amazon Web Services account or a custom key. Required for parameters
-- that use the @SecureString@ data type.
--
-- If you don\'t specify a key ID, the system uses the default key
-- associated with your Amazon Web Services account.
--
-- -   To use your default KMS key, choose the @SecureString@ data type,
--     and do /not/ specify the @Key ID@ when you create the parameter. The
--     system automatically populates @Key ID@ with your default KMS key.
--
-- -   To use a custom KMS key, choose the @SecureString@ data type with
--     the @Key ID@ parameter.
putParameter_keyId :: Lens.Lens' PutParameter (Prelude.Maybe Prelude.Text)
putParameter_keyId = Lens.lens (\PutParameter' {keyId} -> keyId) (\s@PutParameter' {} a -> s {keyId = a} :: PutParameter)

-- | Overwrite an existing parameter. The default value is @false@.
putParameter_overwrite :: Lens.Lens' PutParameter (Prelude.Maybe Prelude.Bool)
putParameter_overwrite = Lens.lens (\PutParameter' {overwrite} -> overwrite) (\s@PutParameter' {} a -> s {overwrite = a} :: PutParameter)

-- | One or more policies to apply to a parameter. This operation takes a
-- JSON array. Parameter Store, a capability of Amazon Web Services Systems
-- Manager supports the following policy types:
--
-- Expiration: This policy deletes the parameter after it expires. When you
-- create the policy, you specify the expiration date. You can update the
-- expiration date and time by updating the policy. Updating the
-- /parameter/ doesn\'t affect the expiration date and time. When the
-- expiration time is reached, Parameter Store deletes the parameter.
--
-- ExpirationNotification: This policy initiates an event in Amazon
-- CloudWatch Events that notifies you about the expiration. By using this
-- policy, you can receive notification before or after the expiration time
-- is reached, in units of days or hours.
--
-- NoChangeNotification: This policy initiates a CloudWatch Events event if
-- a parameter hasn\'t been modified for a specified period of time. This
-- policy type is useful when, for example, a secret needs to be changed
-- within a period of time, but it hasn\'t been changed.
--
-- All existing policies are preserved until you send new policies or an
-- empty policy. For more information about parameter policies, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-store-policies.html Assigning parameter policies>.
putParameter_policies :: Lens.Lens' PutParameter (Prelude.Maybe Prelude.Text)
putParameter_policies = Lens.lens (\PutParameter' {policies} -> policies) (\s@PutParameter' {} a -> s {policies = a} :: PutParameter)

-- | Optional metadata that you assign to a resource. Tags enable you to
-- categorize a resource in different ways, such as by purpose, owner, or
-- environment. For example, you might want to tag a Systems Manager
-- parameter to identify the type of resource to which it applies, the
-- environment, or the type of configuration data referenced by the
-- parameter. In this case, you could specify the following key-value
-- pairs:
--
-- -   @Key=Resource,Value=S3bucket@
--
-- -   @Key=OS,Value=Windows@
--
-- -   @Key=ParameterType,Value=LicenseKey@
--
-- To add tags to an existing Systems Manager parameter, use the
-- AddTagsToResource operation.
putParameter_tags :: Lens.Lens' PutParameter (Prelude.Maybe [Tag])
putParameter_tags = Lens.lens (\PutParameter' {tags} -> tags) (\s@PutParameter' {} a -> s {tags = a} :: PutParameter) Prelude.. Lens.mapping Lens.coerced

-- | The parameter tier to assign to a parameter.
--
-- Parameter Store offers a standard tier and an advanced tier for
-- parameters. Standard parameters have a content size limit of 4 KB and
-- can\'t be configured to use parameter policies. You can create a maximum
-- of 10,000 standard parameters for each Region in an Amazon Web Services
-- account. Standard parameters are offered at no additional cost.
--
-- Advanced parameters have a content size limit of 8 KB and can be
-- configured to use parameter policies. You can create a maximum of
-- 100,000 advanced parameters for each Region in an Amazon Web Services
-- account. Advanced parameters incur a charge. For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-store-advanced-parameters.html Standard and advanced parameter tiers>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- You can change a standard parameter to an advanced parameter any time.
-- But you can\'t revert an advanced parameter to a standard parameter.
-- Reverting an advanced parameter to a standard parameter would result in
-- data loss because the system would truncate the size of the parameter
-- from 8 KB to 4 KB. Reverting would also remove any policies attached to
-- the parameter. Lastly, advanced parameters use a different form of
-- encryption than standard parameters.
--
-- If you no longer need an advanced parameter, or if you no longer want to
-- incur charges for an advanced parameter, you must delete it and recreate
-- it as a new standard parameter.
--
-- __Using the Default Tier Configuration__
--
-- In @PutParameter@ requests, you can specify the tier to create the
-- parameter in. Whenever you specify a tier in the request, Parameter
-- Store creates or updates the parameter according to that request.
-- However, if you don\'t specify a tier in a request, Parameter Store
-- assigns the tier based on the current Parameter Store default tier
-- configuration.
--
-- The default tier when you begin using Parameter Store is the
-- standard-parameter tier. If you use the advanced-parameter tier, you can
-- specify one of the following as the default:
--
-- -   __Advanced__: With this option, Parameter Store evaluates all
--     requests as advanced parameters.
--
-- -   __Intelligent-Tiering__: With this option, Parameter Store evaluates
--     each request to determine if the parameter is standard or advanced.
--
--     If the request doesn\'t include any options that require an advanced
--     parameter, the parameter is created in the standard-parameter tier.
--     If one or more options requiring an advanced parameter are included
--     in the request, Parameter Store create a parameter in the
--     advanced-parameter tier.
--
--     This approach helps control your parameter-related costs by always
--     creating standard parameters unless an advanced parameter is
--     necessary.
--
-- Options that require an advanced parameter include the following:
--
-- -   The content size of the parameter is more than 4 KB.
--
-- -   The parameter uses a parameter policy.
--
-- -   More than 10,000 parameters already exist in your Amazon Web
--     Services account in the current Amazon Web Services Region.
--
-- For more information about configuring the default tier option, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/ps-default-tier.html Specifying a default parameter tier>
-- in the /Amazon Web Services Systems Manager User Guide/.
putParameter_tier :: Lens.Lens' PutParameter (Prelude.Maybe ParameterTier)
putParameter_tier = Lens.lens (\PutParameter' {tier} -> tier) (\s@PutParameter' {} a -> s {tier = a} :: PutParameter)

-- | The type of parameter that you want to add to the system.
--
-- @SecureString@ isn\'t currently supported for CloudFormation templates.
--
-- Items in a @StringList@ must be separated by a comma (,). You can\'t use
-- other punctuation or special character to escape items in the list. If
-- you have a parameter value that requires a comma, then use the @String@
-- data type.
--
-- Specifying a parameter type isn\'t required when updating a parameter.
-- You must specify a parameter type when creating a parameter.
putParameter_type :: Lens.Lens' PutParameter (Prelude.Maybe ParameterType)
putParameter_type = Lens.lens (\PutParameter' {type'} -> type') (\s@PutParameter' {} a -> s {type' = a} :: PutParameter)

-- | The fully qualified name of the parameter that you want to add to the
-- system. The fully qualified name includes the complete hierarchy of the
-- parameter path and name. For parameters in a hierarchy, you must include
-- a leading forward slash character (\/) when you create or reference a
-- parameter. For example: @\/Dev\/DBServer\/MySQL\/db-string13@
--
-- Naming Constraints:
--
-- -   Parameter names are case sensitive.
--
-- -   A parameter name must be unique within an Amazon Web Services Region
--
-- -   A parameter name can\'t be prefixed with \"@aws@\" or \"@ssm@\"
--     (case-insensitive).
--
-- -   Parameter names can include only the following symbols and letters:
--     @a-zA-Z0-9_.-@
--
--     In addition, the slash character ( \/ ) is used to delineate
--     hierarchies in parameter names. For example:
--     @\/Dev\/Production\/East\/Project-ABC\/MyParameter@
--
-- -   A parameter name can\'t include spaces.
--
-- -   Parameter hierarchies are limited to a maximum depth of fifteen
--     levels.
--
-- For additional information about valid values for parameter names, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-paramstore-su-create.html Creating Systems Manager parameters>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- The maximum length constraint of 2048 characters listed below includes
-- 1037 characters reserved for internal use by Systems Manager. The
-- maximum length for a parameter name that you create is 1011 characters.
-- This includes the characters in the ARN that precede the name you
-- specify, such as @arn:aws:ssm:us-east-2:111122223333:parameter\/@.
putParameter_name :: Lens.Lens' PutParameter Prelude.Text
putParameter_name = Lens.lens (\PutParameter' {name} -> name) (\s@PutParameter' {} a -> s {name = a} :: PutParameter)

-- | The parameter value that you want to add to the system. Standard
-- parameters have a value limit of 4 KB. Advanced parameters have a value
-- limit of 8 KB.
--
-- Parameters can\'t be referenced or nested in the values of other
-- parameters. You can\'t include @{{}}@ or @{{ssm:@/@parameter-name@/@}}@
-- in a parameter value.
putParameter_value :: Lens.Lens' PutParameter Prelude.Text
putParameter_value = Lens.lens (\PutParameter' {value} -> value) (\s@PutParameter' {} a -> s {value = a} :: PutParameter) Prelude.. Data._Sensitive

instance Core.AWSRequest PutParameter where
  type AWSResponse PutParameter = PutParameterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutParameterResponse'
            Prelude.<$> (x Data..?> "Tier")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Version")
      )

instance Prelude.Hashable PutParameter where
  hashWithSalt _salt PutParameter' {..} =
    _salt
      `Prelude.hashWithSalt` allowedPattern
      `Prelude.hashWithSalt` dataType
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` keyId
      `Prelude.hashWithSalt` overwrite
      `Prelude.hashWithSalt` policies
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` tier
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData PutParameter where
  rnf PutParameter' {..} =
    Prelude.rnf allowedPattern
      `Prelude.seq` Prelude.rnf dataType
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf keyId
      `Prelude.seq` Prelude.rnf overwrite
      `Prelude.seq` Prelude.rnf policies
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf tier
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf value

instance Data.ToHeaders PutParameter where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AmazonSSM.PutParameter" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutParameter where
  toJSON PutParameter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AllowedPattern" Data..=)
              Prelude.<$> allowedPattern,
            ("DataType" Data..=) Prelude.<$> dataType,
            ("Description" Data..=) Prelude.<$> description,
            ("KeyId" Data..=) Prelude.<$> keyId,
            ("Overwrite" Data..=) Prelude.<$> overwrite,
            ("Policies" Data..=) Prelude.<$> policies,
            ("Tags" Data..=) Prelude.<$> tags,
            ("Tier" Data..=) Prelude.<$> tier,
            ("Type" Data..=) Prelude.<$> type',
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Value" Data..= value)
          ]
      )

instance Data.ToPath PutParameter where
  toPath = Prelude.const "/"

instance Data.ToQuery PutParameter where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutParameterResponse' smart constructor.
data PutParameterResponse = PutParameterResponse'
  { -- | The tier assigned to the parameter.
    tier :: Prelude.Maybe ParameterTier,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The new version number of a parameter. If you edit a parameter value,
    -- Parameter Store automatically creates a new version and assigns this new
    -- version a unique ID. You can reference a parameter version ID in API
    -- operations or in Systems Manager documents (SSM documents). By default,
    -- if you don\'t specify a specific version, the system returns the latest
    -- parameter value when a parameter is called.
    version :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutParameterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tier', 'putParameterResponse_tier' - The tier assigned to the parameter.
--
-- 'httpStatus', 'putParameterResponse_httpStatus' - The response's http status code.
--
-- 'version', 'putParameterResponse_version' - The new version number of a parameter. If you edit a parameter value,
-- Parameter Store automatically creates a new version and assigns this new
-- version a unique ID. You can reference a parameter version ID in API
-- operations or in Systems Manager documents (SSM documents). By default,
-- if you don\'t specify a specific version, the system returns the latest
-- parameter value when a parameter is called.
newPutParameterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'version'
  Prelude.Integer ->
  PutParameterResponse
newPutParameterResponse pHttpStatus_ pVersion_ =
  PutParameterResponse'
    { tier = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      version = pVersion_
    }

-- | The tier assigned to the parameter.
putParameterResponse_tier :: Lens.Lens' PutParameterResponse (Prelude.Maybe ParameterTier)
putParameterResponse_tier = Lens.lens (\PutParameterResponse' {tier} -> tier) (\s@PutParameterResponse' {} a -> s {tier = a} :: PutParameterResponse)

-- | The response's http status code.
putParameterResponse_httpStatus :: Lens.Lens' PutParameterResponse Prelude.Int
putParameterResponse_httpStatus = Lens.lens (\PutParameterResponse' {httpStatus} -> httpStatus) (\s@PutParameterResponse' {} a -> s {httpStatus = a} :: PutParameterResponse)

-- | The new version number of a parameter. If you edit a parameter value,
-- Parameter Store automatically creates a new version and assigns this new
-- version a unique ID. You can reference a parameter version ID in API
-- operations or in Systems Manager documents (SSM documents). By default,
-- if you don\'t specify a specific version, the system returns the latest
-- parameter value when a parameter is called.
putParameterResponse_version :: Lens.Lens' PutParameterResponse Prelude.Integer
putParameterResponse_version = Lens.lens (\PutParameterResponse' {version} -> version) (\s@PutParameterResponse' {} a -> s {version = a} :: PutParameterResponse)

instance Prelude.NFData PutParameterResponse where
  rnf PutParameterResponse' {..} =
    Prelude.rnf tier
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf version
