{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.PutParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Add a parameter to the system.
module Network.AWS.SSM.PutParameter
  ( -- * Creating a request
    PutParameter (..),
    mkPutParameter,

    -- ** Request lenses
    ppKeyId,
    ppValue,
    ppName,
    ppTier,
    ppAllowedPattern,
    ppType,
    ppDataType,
    ppOverwrite,
    ppDescription,
    ppPolicies,
    ppTags,

    -- * Destructuring the response
    PutParameterResponse (..),
    mkPutParameterResponse,

    -- ** Response lenses
    pprsTier,
    pprsVersion,
    pprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkPutParameter' smart constructor.
data PutParameter = PutParameter'
  { -- | The KMS Key ID that you want to use to encrypt a parameter. Either the default AWS Key Management Service (AWS KMS) key automatically assigned to your AWS account or a custom key. Required for parameters that use the @SecureString@ data type.
    --
    -- If you don't specify a key ID, the system uses the default key associated with your AWS account.
    --
    --     * To use your default AWS KMS key, choose the @SecureString@ data type, and do /not/ specify the @Key ID@ when you create the parameter. The system automatically populates @Key ID@ with your default KMS key.
    --
    --
    --     * To use a custom KMS key, choose the @SecureString@ data type with the @Key ID@ parameter.
    keyId :: Lude.Maybe Lude.Text,
    -- | The parameter value that you want to add to the system. Standard parameters have a value limit of 4 KB. Advanced parameters have a value limit of 8 KB.
    value :: Lude.Text,
    -- | The fully qualified name of the parameter that you want to add to the system. The fully qualified name includes the complete hierarchy of the parameter path and name. For parameters in a hierarchy, you must include a leading forward slash character (/) when you create or reference a parameter. For example: @/Dev/DBServer/MySQL/db-string13@
    --
    -- Naming Constraints:
    --
    --     * Parameter names are case sensitive.
    --
    --
    --     * A parameter name must be unique within an AWS Region
    --
    --
    --     * A parameter name can't be prefixed with "aws" or "ssm" (case-insensitive).
    --
    --
    --     * Parameter names can include only the following symbols and letters: @a-zA-Z0-9_.-/@
    --
    --
    --     * A parameter name can't include spaces.
    --
    --
    --     * Parameter hierarchies are limited to a maximum depth of fifteen levels.
    --
    --
    -- For additional information about valid values for parameter names, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-parameter-name-constraints.html About requirements and constraints for parameter names> in the /AWS Systems Manager User Guide/ .
    name :: Lude.Text,
    -- | The parameter tier to assign to a parameter.
    --
    -- Parameter Store offers a standard tier and an advanced tier for parameters. Standard parameters have a content size limit of 4 KB and can't be configured to use parameter policies. You can create a maximum of 10,000 standard parameters for each Region in an AWS account. Standard parameters are offered at no additional cost.
    -- Advanced parameters have a content size limit of 8 KB and can be configured to use parameter policies. You can create a maximum of 100,000 advanced parameters for each Region in an AWS account. Advanced parameters incur a charge. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-store-advanced-parameters.html Standard and advanced parameter tiers> in the /AWS Systems Manager User Guide/ .
    -- You can change a standard parameter to an advanced parameter any time. But you can't revert an advanced parameter to a standard parameter. Reverting an advanced parameter to a standard parameter would result in data loss because the system would truncate the size of the parameter from 8 KB to 4 KB. Reverting would also remove any policies attached to the parameter. Lastly, advanced parameters use a different form of encryption than standard parameters.
    -- If you no longer need an advanced parameter, or if you no longer want to incur charges for an advanced parameter, you must delete it and recreate it as a new standard parameter.
    -- __Using the Default Tier Configuration__
    -- In @PutParameter@ requests, you can specify the tier to create the parameter in. Whenever you specify a tier in the request, Parameter Store creates or updates the parameter according to that request. However, if you do not specify a tier in a request, Parameter Store assigns the tier based on the current Parameter Store default tier configuration.
    -- The default tier when you begin using Parameter Store is the standard-parameter tier. If you use the advanced-parameter tier, you can specify one of the following as the default:
    --
    --     * __Advanced__ : With this option, Parameter Store evaluates all requests as advanced parameters.
    --
    --
    --     * __Intelligent-Tiering__ : With this option, Parameter Store evaluates each request to determine if the parameter is standard or advanced.
    -- If the request doesn't include any options that require an advanced parameter, the parameter is created in the standard-parameter tier. If one or more options requiring an advanced parameter are included in the request, Parameter Store create a parameter in the advanced-parameter tier.
    -- This approach helps control your parameter-related costs by always creating standard parameters unless an advanced parameter is necessary.
    --
    --
    -- Options that require an advanced parameter include the following:
    --
    --     * The content size of the parameter is more than 4 KB.
    --
    --
    --     * The parameter uses a parameter policy.
    --
    --
    --     * More than 10,000 parameters already exist in your AWS account in the current Region.
    --
    --
    -- For more information about configuring the default tier option, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/ps-default-tier.html Specifying a default parameter tier> in the /AWS Systems Manager User Guide/ .
    tier :: Lude.Maybe ParameterTier,
    -- | A regular expression used to validate the parameter value. For example, for String types with values restricted to numbers, you can specify the following: AllowedPattern=^\d+$
    allowedPattern :: Lude.Maybe Lude.Text,
    -- | The type of parameter that you want to add to the system.
    --
    -- Items in a @StringList@ must be separated by a comma (,). You can't use other punctuation or special character to escape items in the list. If you have a parameter value that requires a comma, then use the @String@ data type.
    -- /Important:/ Specifying a parameter type is not required when updating a parameter. You must specify a parameter type when creating a parameter.
    type' :: Lude.Maybe ParameterType,
    -- | The data type for a @String@ parameter. Supported data types include plain text and Amazon Machine Image IDs.
    --
    -- __The following data type values are supported.__
    --
    --     * @text@
    --
    --
    --     * @aws:ec2:image@
    --
    --
    -- When you create a @String@ parameter and specify @aws:ec2:image@ , Systems Manager validates the parameter value is in the required format, such as @ami-12345abcdeEXAMPLE@ , and that the specified AMI is available in your AWS account. For more information, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-store-ec2-aliases.html Native parameter support for Amazon Machine Image IDs> in the /AWS Systems Manager User Guide/ .
    dataType :: Lude.Maybe Lude.Text,
    -- | Overwrite an existing parameter. If not specified, will default to "false".
    overwrite :: Lude.Maybe Lude.Bool,
    -- | Information about the parameter that you want to add to the system. Optional but recommended.
    --
    -- /Important:/ Do not enter personally identifiable information in this field.
    description :: Lude.Maybe Lude.Text,
    -- | One or more policies to apply to a parameter. This action takes a JSON array. Parameter Store supports the following policy types:
    --
    -- Expiration: This policy deletes the parameter after it expires. When you create the policy, you specify the expiration date. You can update the expiration date and time by updating the policy. Updating the /parameter/ does not affect the expiration date and time. When the expiration time is reached, Parameter Store deletes the parameter.
    -- ExpirationNotification: This policy triggers an event in Amazon CloudWatch Events that notifies you about the expiration. By using this policy, you can receive notification before or after the expiration time is reached, in units of days or hours.
    -- NoChangeNotification: This policy triggers a CloudWatch event if a parameter has not been modified for a specified period of time. This policy type is useful when, for example, a secret needs to be changed within a period of time, but it has not been changed.
    -- All existing policies are preserved until you send new policies or an empty policy. For more information about parameter policies, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-store-policies.html Assigning parameter policies> .
    policies :: Lude.Maybe Lude.Text,
    -- | Optional metadata that you assign to a resource. Tags enable you to categorize a resource in different ways, such as by purpose, owner, or environment. For example, you might want to tag a Systems Manager parameter to identify the type of resource to which it applies, the environment, or the type of configuration data referenced by the parameter. In this case, you could specify the following key name/value pairs:
    --
    --
    --     * @Key=Resource,Value=S3bucket@
    --
    --
    --     * @Key=OS,Value=Windows@
    --
    --
    --     * @Key=ParameterType,Value=LicenseKey@
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutParameter' with the minimum fields required to make a request.
--
-- * 'keyId' - The KMS Key ID that you want to use to encrypt a parameter. Either the default AWS Key Management Service (AWS KMS) key automatically assigned to your AWS account or a custom key. Required for parameters that use the @SecureString@ data type.
--
-- If you don't specify a key ID, the system uses the default key associated with your AWS account.
--
--     * To use your default AWS KMS key, choose the @SecureString@ data type, and do /not/ specify the @Key ID@ when you create the parameter. The system automatically populates @Key ID@ with your default KMS key.
--
--
--     * To use a custom KMS key, choose the @SecureString@ data type with the @Key ID@ parameter.
--
--
-- * 'value' - The parameter value that you want to add to the system. Standard parameters have a value limit of 4 KB. Advanced parameters have a value limit of 8 KB.
-- * 'name' - The fully qualified name of the parameter that you want to add to the system. The fully qualified name includes the complete hierarchy of the parameter path and name. For parameters in a hierarchy, you must include a leading forward slash character (/) when you create or reference a parameter. For example: @/Dev/DBServer/MySQL/db-string13@
--
-- Naming Constraints:
--
--     * Parameter names are case sensitive.
--
--
--     * A parameter name must be unique within an AWS Region
--
--
--     * A parameter name can't be prefixed with "aws" or "ssm" (case-insensitive).
--
--
--     * Parameter names can include only the following symbols and letters: @a-zA-Z0-9_.-/@
--
--
--     * A parameter name can't include spaces.
--
--
--     * Parameter hierarchies are limited to a maximum depth of fifteen levels.
--
--
-- For additional information about valid values for parameter names, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-parameter-name-constraints.html About requirements and constraints for parameter names> in the /AWS Systems Manager User Guide/ .
-- * 'tier' - The parameter tier to assign to a parameter.
--
-- Parameter Store offers a standard tier and an advanced tier for parameters. Standard parameters have a content size limit of 4 KB and can't be configured to use parameter policies. You can create a maximum of 10,000 standard parameters for each Region in an AWS account. Standard parameters are offered at no additional cost.
-- Advanced parameters have a content size limit of 8 KB and can be configured to use parameter policies. You can create a maximum of 100,000 advanced parameters for each Region in an AWS account. Advanced parameters incur a charge. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-store-advanced-parameters.html Standard and advanced parameter tiers> in the /AWS Systems Manager User Guide/ .
-- You can change a standard parameter to an advanced parameter any time. But you can't revert an advanced parameter to a standard parameter. Reverting an advanced parameter to a standard parameter would result in data loss because the system would truncate the size of the parameter from 8 KB to 4 KB. Reverting would also remove any policies attached to the parameter. Lastly, advanced parameters use a different form of encryption than standard parameters.
-- If you no longer need an advanced parameter, or if you no longer want to incur charges for an advanced parameter, you must delete it and recreate it as a new standard parameter.
-- __Using the Default Tier Configuration__
-- In @PutParameter@ requests, you can specify the tier to create the parameter in. Whenever you specify a tier in the request, Parameter Store creates or updates the parameter according to that request. However, if you do not specify a tier in a request, Parameter Store assigns the tier based on the current Parameter Store default tier configuration.
-- The default tier when you begin using Parameter Store is the standard-parameter tier. If you use the advanced-parameter tier, you can specify one of the following as the default:
--
--     * __Advanced__ : With this option, Parameter Store evaluates all requests as advanced parameters.
--
--
--     * __Intelligent-Tiering__ : With this option, Parameter Store evaluates each request to determine if the parameter is standard or advanced.
-- If the request doesn't include any options that require an advanced parameter, the parameter is created in the standard-parameter tier. If one or more options requiring an advanced parameter are included in the request, Parameter Store create a parameter in the advanced-parameter tier.
-- This approach helps control your parameter-related costs by always creating standard parameters unless an advanced parameter is necessary.
--
--
-- Options that require an advanced parameter include the following:
--
--     * The content size of the parameter is more than 4 KB.
--
--
--     * The parameter uses a parameter policy.
--
--
--     * More than 10,000 parameters already exist in your AWS account in the current Region.
--
--
-- For more information about configuring the default tier option, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/ps-default-tier.html Specifying a default parameter tier> in the /AWS Systems Manager User Guide/ .
-- * 'allowedPattern' - A regular expression used to validate the parameter value. For example, for String types with values restricted to numbers, you can specify the following: AllowedPattern=^\d+$
-- * 'type'' - The type of parameter that you want to add to the system.
--
-- Items in a @StringList@ must be separated by a comma (,). You can't use other punctuation or special character to escape items in the list. If you have a parameter value that requires a comma, then use the @String@ data type.
-- /Important:/ Specifying a parameter type is not required when updating a parameter. You must specify a parameter type when creating a parameter.
-- * 'dataType' - The data type for a @String@ parameter. Supported data types include plain text and Amazon Machine Image IDs.
--
-- __The following data type values are supported.__
--
--     * @text@
--
--
--     * @aws:ec2:image@
--
--
-- When you create a @String@ parameter and specify @aws:ec2:image@ , Systems Manager validates the parameter value is in the required format, such as @ami-12345abcdeEXAMPLE@ , and that the specified AMI is available in your AWS account. For more information, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-store-ec2-aliases.html Native parameter support for Amazon Machine Image IDs> in the /AWS Systems Manager User Guide/ .
-- * 'overwrite' - Overwrite an existing parameter. If not specified, will default to "false".
-- * 'description' - Information about the parameter that you want to add to the system. Optional but recommended.
--
-- /Important:/ Do not enter personally identifiable information in this field.
-- * 'policies' - One or more policies to apply to a parameter. This action takes a JSON array. Parameter Store supports the following policy types:
--
-- Expiration: This policy deletes the parameter after it expires. When you create the policy, you specify the expiration date. You can update the expiration date and time by updating the policy. Updating the /parameter/ does not affect the expiration date and time. When the expiration time is reached, Parameter Store deletes the parameter.
-- ExpirationNotification: This policy triggers an event in Amazon CloudWatch Events that notifies you about the expiration. By using this policy, you can receive notification before or after the expiration time is reached, in units of days or hours.
-- NoChangeNotification: This policy triggers a CloudWatch event if a parameter has not been modified for a specified period of time. This policy type is useful when, for example, a secret needs to be changed within a period of time, but it has not been changed.
-- All existing policies are preserved until you send new policies or an empty policy. For more information about parameter policies, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-store-policies.html Assigning parameter policies> .
-- * 'tags' - Optional metadata that you assign to a resource. Tags enable you to categorize a resource in different ways, such as by purpose, owner, or environment. For example, you might want to tag a Systems Manager parameter to identify the type of resource to which it applies, the environment, or the type of configuration data referenced by the parameter. In this case, you could specify the following key name/value pairs:
--
--
--     * @Key=Resource,Value=S3bucket@
--
--
--     * @Key=OS,Value=Windows@
--
--
--     * @Key=ParameterType,Value=LicenseKey@
mkPutParameter ::
  -- | 'value'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  PutParameter
mkPutParameter pValue_ pName_ =
  PutParameter'
    { keyId = Lude.Nothing,
      value = pValue_,
      name = pName_,
      tier = Lude.Nothing,
      allowedPattern = Lude.Nothing,
      type' = Lude.Nothing,
      dataType = Lude.Nothing,
      overwrite = Lude.Nothing,
      description = Lude.Nothing,
      policies = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The KMS Key ID that you want to use to encrypt a parameter. Either the default AWS Key Management Service (AWS KMS) key automatically assigned to your AWS account or a custom key. Required for parameters that use the @SecureString@ data type.
--
-- If you don't specify a key ID, the system uses the default key associated with your AWS account.
--
--     * To use your default AWS KMS key, choose the @SecureString@ data type, and do /not/ specify the @Key ID@ when you create the parameter. The system automatically populates @Key ID@ with your default KMS key.
--
--
--     * To use a custom KMS key, choose the @SecureString@ data type with the @Key ID@ parameter.
--
--
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppKeyId :: Lens.Lens' PutParameter (Lude.Maybe Lude.Text)
ppKeyId = Lens.lens (keyId :: PutParameter -> Lude.Maybe Lude.Text) (\s a -> s {keyId = a} :: PutParameter)
{-# DEPRECATED ppKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | The parameter value that you want to add to the system. Standard parameters have a value limit of 4 KB. Advanced parameters have a value limit of 8 KB.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppValue :: Lens.Lens' PutParameter Lude.Text
ppValue = Lens.lens (value :: PutParameter -> Lude.Text) (\s a -> s {value = a} :: PutParameter)
{-# DEPRECATED ppValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The fully qualified name of the parameter that you want to add to the system. The fully qualified name includes the complete hierarchy of the parameter path and name. For parameters in a hierarchy, you must include a leading forward slash character (/) when you create or reference a parameter. For example: @/Dev/DBServer/MySQL/db-string13@
--
-- Naming Constraints:
--
--     * Parameter names are case sensitive.
--
--
--     * A parameter name must be unique within an AWS Region
--
--
--     * A parameter name can't be prefixed with "aws" or "ssm" (case-insensitive).
--
--
--     * Parameter names can include only the following symbols and letters: @a-zA-Z0-9_.-/@
--
--
--     * A parameter name can't include spaces.
--
--
--     * Parameter hierarchies are limited to a maximum depth of fifteen levels.
--
--
-- For additional information about valid values for parameter names, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-parameter-name-constraints.html About requirements and constraints for parameter names> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppName :: Lens.Lens' PutParameter Lude.Text
ppName = Lens.lens (name :: PutParameter -> Lude.Text) (\s a -> s {name = a} :: PutParameter)
{-# DEPRECATED ppName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The parameter tier to assign to a parameter.
--
-- Parameter Store offers a standard tier and an advanced tier for parameters. Standard parameters have a content size limit of 4 KB and can't be configured to use parameter policies. You can create a maximum of 10,000 standard parameters for each Region in an AWS account. Standard parameters are offered at no additional cost.
-- Advanced parameters have a content size limit of 8 KB and can be configured to use parameter policies. You can create a maximum of 100,000 advanced parameters for each Region in an AWS account. Advanced parameters incur a charge. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-store-advanced-parameters.html Standard and advanced parameter tiers> in the /AWS Systems Manager User Guide/ .
-- You can change a standard parameter to an advanced parameter any time. But you can't revert an advanced parameter to a standard parameter. Reverting an advanced parameter to a standard parameter would result in data loss because the system would truncate the size of the parameter from 8 KB to 4 KB. Reverting would also remove any policies attached to the parameter. Lastly, advanced parameters use a different form of encryption than standard parameters.
-- If you no longer need an advanced parameter, or if you no longer want to incur charges for an advanced parameter, you must delete it and recreate it as a new standard parameter.
-- __Using the Default Tier Configuration__
-- In @PutParameter@ requests, you can specify the tier to create the parameter in. Whenever you specify a tier in the request, Parameter Store creates or updates the parameter according to that request. However, if you do not specify a tier in a request, Parameter Store assigns the tier based on the current Parameter Store default tier configuration.
-- The default tier when you begin using Parameter Store is the standard-parameter tier. If you use the advanced-parameter tier, you can specify one of the following as the default:
--
--     * __Advanced__ : With this option, Parameter Store evaluates all requests as advanced parameters.
--
--
--     * __Intelligent-Tiering__ : With this option, Parameter Store evaluates each request to determine if the parameter is standard or advanced.
-- If the request doesn't include any options that require an advanced parameter, the parameter is created in the standard-parameter tier. If one or more options requiring an advanced parameter are included in the request, Parameter Store create a parameter in the advanced-parameter tier.
-- This approach helps control your parameter-related costs by always creating standard parameters unless an advanced parameter is necessary.
--
--
-- Options that require an advanced parameter include the following:
--
--     * The content size of the parameter is more than 4 KB.
--
--
--     * The parameter uses a parameter policy.
--
--
--     * More than 10,000 parameters already exist in your AWS account in the current Region.
--
--
-- For more information about configuring the default tier option, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/ps-default-tier.html Specifying a default parameter tier> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'tier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppTier :: Lens.Lens' PutParameter (Lude.Maybe ParameterTier)
ppTier = Lens.lens (tier :: PutParameter -> Lude.Maybe ParameterTier) (\s a -> s {tier = a} :: PutParameter)
{-# DEPRECATED ppTier "Use generic-lens or generic-optics with 'tier' instead." #-}

-- | A regular expression used to validate the parameter value. For example, for String types with values restricted to numbers, you can specify the following: AllowedPattern=^\d+$
--
-- /Note:/ Consider using 'allowedPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppAllowedPattern :: Lens.Lens' PutParameter (Lude.Maybe Lude.Text)
ppAllowedPattern = Lens.lens (allowedPattern :: PutParameter -> Lude.Maybe Lude.Text) (\s a -> s {allowedPattern = a} :: PutParameter)
{-# DEPRECATED ppAllowedPattern "Use generic-lens or generic-optics with 'allowedPattern' instead." #-}

-- | The type of parameter that you want to add to the system.
--
-- Items in a @StringList@ must be separated by a comma (,). You can't use other punctuation or special character to escape items in the list. If you have a parameter value that requires a comma, then use the @String@ data type.
-- /Important:/ Specifying a parameter type is not required when updating a parameter. You must specify a parameter type when creating a parameter.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppType :: Lens.Lens' PutParameter (Lude.Maybe ParameterType)
ppType = Lens.lens (type' :: PutParameter -> Lude.Maybe ParameterType) (\s a -> s {type' = a} :: PutParameter)
{-# DEPRECATED ppType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The data type for a @String@ parameter. Supported data types include plain text and Amazon Machine Image IDs.
--
-- __The following data type values are supported.__
--
--     * @text@
--
--
--     * @aws:ec2:image@
--
--
-- When you create a @String@ parameter and specify @aws:ec2:image@ , Systems Manager validates the parameter value is in the required format, such as @ami-12345abcdeEXAMPLE@ , and that the specified AMI is available in your AWS account. For more information, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-store-ec2-aliases.html Native parameter support for Amazon Machine Image IDs> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'dataType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppDataType :: Lens.Lens' PutParameter (Lude.Maybe Lude.Text)
ppDataType = Lens.lens (dataType :: PutParameter -> Lude.Maybe Lude.Text) (\s a -> s {dataType = a} :: PutParameter)
{-# DEPRECATED ppDataType "Use generic-lens or generic-optics with 'dataType' instead." #-}

-- | Overwrite an existing parameter. If not specified, will default to "false".
--
-- /Note:/ Consider using 'overwrite' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppOverwrite :: Lens.Lens' PutParameter (Lude.Maybe Lude.Bool)
ppOverwrite = Lens.lens (overwrite :: PutParameter -> Lude.Maybe Lude.Bool) (\s a -> s {overwrite = a} :: PutParameter)
{-# DEPRECATED ppOverwrite "Use generic-lens or generic-optics with 'overwrite' instead." #-}

-- | Information about the parameter that you want to add to the system. Optional but recommended.
--
-- /Important:/ Do not enter personally identifiable information in this field.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppDescription :: Lens.Lens' PutParameter (Lude.Maybe Lude.Text)
ppDescription = Lens.lens (description :: PutParameter -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: PutParameter)
{-# DEPRECATED ppDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | One or more policies to apply to a parameter. This action takes a JSON array. Parameter Store supports the following policy types:
--
-- Expiration: This policy deletes the parameter after it expires. When you create the policy, you specify the expiration date. You can update the expiration date and time by updating the policy. Updating the /parameter/ does not affect the expiration date and time. When the expiration time is reached, Parameter Store deletes the parameter.
-- ExpirationNotification: This policy triggers an event in Amazon CloudWatch Events that notifies you about the expiration. By using this policy, you can receive notification before or after the expiration time is reached, in units of days or hours.
-- NoChangeNotification: This policy triggers a CloudWatch event if a parameter has not been modified for a specified period of time. This policy type is useful when, for example, a secret needs to be changed within a period of time, but it has not been changed.
-- All existing policies are preserved until you send new policies or an empty policy. For more information about parameter policies, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-store-policies.html Assigning parameter policies> .
--
-- /Note:/ Consider using 'policies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppPolicies :: Lens.Lens' PutParameter (Lude.Maybe Lude.Text)
ppPolicies = Lens.lens (policies :: PutParameter -> Lude.Maybe Lude.Text) (\s a -> s {policies = a} :: PutParameter)
{-# DEPRECATED ppPolicies "Use generic-lens or generic-optics with 'policies' instead." #-}

-- | Optional metadata that you assign to a resource. Tags enable you to categorize a resource in different ways, such as by purpose, owner, or environment. For example, you might want to tag a Systems Manager parameter to identify the type of resource to which it applies, the environment, or the type of configuration data referenced by the parameter. In this case, you could specify the following key name/value pairs:
--
--
--     * @Key=Resource,Value=S3bucket@
--
--
--     * @Key=OS,Value=Windows@
--
--
--     * @Key=ParameterType,Value=LicenseKey@
--
--
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppTags :: Lens.Lens' PutParameter (Lude.Maybe [Tag])
ppTags = Lens.lens (tags :: PutParameter -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: PutParameter)
{-# DEPRECATED ppTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest PutParameter where
  type Rs PutParameter = PutParameterResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutParameterResponse'
            Lude.<$> (x Lude..?> "Tier")
            Lude.<*> (x Lude..?> "Version")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutParameter where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.PutParameter" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutParameter where
  toJSON PutParameter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("KeyId" Lude..=) Lude.<$> keyId,
            Lude.Just ("Value" Lude..= value),
            Lude.Just ("Name" Lude..= name),
            ("Tier" Lude..=) Lude.<$> tier,
            ("AllowedPattern" Lude..=) Lude.<$> allowedPattern,
            ("Type" Lude..=) Lude.<$> type',
            ("DataType" Lude..=) Lude.<$> dataType,
            ("Overwrite" Lude..=) Lude.<$> overwrite,
            ("Description" Lude..=) Lude.<$> description,
            ("Policies" Lude..=) Lude.<$> policies,
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath PutParameter where
  toPath = Lude.const "/"

instance Lude.ToQuery PutParameter where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutParameterResponse' smart constructor.
data PutParameterResponse = PutParameterResponse'
  { -- | The tier assigned to the parameter.
    tier :: Lude.Maybe ParameterTier,
    -- | The new version number of a parameter. If you edit a parameter value, Parameter Store automatically creates a new version and assigns this new version a unique ID. You can reference a parameter version ID in API actions or in Systems Manager documents (SSM documents). By default, if you don't specify a specific version, the system returns the latest parameter value when a parameter is called.
    version :: Lude.Maybe Lude.Integer,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutParameterResponse' with the minimum fields required to make a request.
--
-- * 'tier' - The tier assigned to the parameter.
-- * 'version' - The new version number of a parameter. If you edit a parameter value, Parameter Store automatically creates a new version and assigns this new version a unique ID. You can reference a parameter version ID in API actions or in Systems Manager documents (SSM documents). By default, if you don't specify a specific version, the system returns the latest parameter value when a parameter is called.
-- * 'responseStatus' - The response status code.
mkPutParameterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutParameterResponse
mkPutParameterResponse pResponseStatus_ =
  PutParameterResponse'
    { tier = Lude.Nothing,
      version = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The tier assigned to the parameter.
--
-- /Note:/ Consider using 'tier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pprsTier :: Lens.Lens' PutParameterResponse (Lude.Maybe ParameterTier)
pprsTier = Lens.lens (tier :: PutParameterResponse -> Lude.Maybe ParameterTier) (\s a -> s {tier = a} :: PutParameterResponse)
{-# DEPRECATED pprsTier "Use generic-lens or generic-optics with 'tier' instead." #-}

-- | The new version number of a parameter. If you edit a parameter value, Parameter Store automatically creates a new version and assigns this new version a unique ID. You can reference a parameter version ID in API actions or in Systems Manager documents (SSM documents). By default, if you don't specify a specific version, the system returns the latest parameter value when a parameter is called.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pprsVersion :: Lens.Lens' PutParameterResponse (Lude.Maybe Lude.Integer)
pprsVersion = Lens.lens (version :: PutParameterResponse -> Lude.Maybe Lude.Integer) (\s a -> s {version = a} :: PutParameterResponse)
{-# DEPRECATED pprsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pprsResponseStatus :: Lens.Lens' PutParameterResponse Lude.Int
pprsResponseStatus = Lens.lens (responseStatus :: PutParameterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutParameterResponse)
{-# DEPRECATED pprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
