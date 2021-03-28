{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      PutParameter (..)
    , mkPutParameter
    -- ** Request lenses
    , ppName
    , ppValue
    , ppAllowedPattern
    , ppDataType
    , ppDescription
    , ppKeyId
    , ppOverwrite
    , ppPolicies
    , ppTags
    , ppTier
    , ppType

    -- * Destructuring the response
    , PutParameterResponse (..)
    , mkPutParameterResponse
    -- ** Response lenses
    , pprrsTier
    , pprrsVersion
    , pprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkPutParameter' smart constructor.
data PutParameter = PutParameter'
  { name :: Types.PSParameterName
    -- ^ The fully qualified name of the parameter that you want to add to the system. The fully qualified name includes the complete hierarchy of the parameter path and name. For parameters in a hierarchy, you must include a leading forward slash character (/) when you create or reference a parameter. For example: @/Dev/DBServer/MySQL/db-string13@ 
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
  , value :: Types.Value
    -- ^ The parameter value that you want to add to the system. Standard parameters have a value limit of 4 KB. Advanced parameters have a value limit of 8 KB.
  , allowedPattern :: Core.Maybe Types.AllowedPattern
    -- ^ A regular expression used to validate the parameter value. For example, for String types with values restricted to numbers, you can specify the following: AllowedPattern=^\d+$ 
  , dataType :: Core.Maybe Types.DataType
    -- ^ The data type for a @String@ parameter. Supported data types include plain text and Amazon Machine Image IDs.
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
  , description :: Core.Maybe Types.ParameterDescription
    -- ^ Information about the parameter that you want to add to the system. Optional but recommended.
--
-- /Important:/ Do not enter personally identifiable information in this field.
  , keyId :: Core.Maybe Types.ParameterKeyId
    -- ^ The KMS Key ID that you want to use to encrypt a parameter. Either the default AWS Key Management Service (AWS KMS) key automatically assigned to your AWS account or a custom key. Required for parameters that use the @SecureString@ data type.
--
-- If you don't specify a key ID, the system uses the default key associated with your AWS account.
--
--     * To use your default AWS KMS key, choose the @SecureString@ data type, and do /not/ specify the @Key ID@ when you create the parameter. The system automatically populates @Key ID@ with your default KMS key.
--
--
--     * To use a custom KMS key, choose the @SecureString@ data type with the @Key ID@ parameter.
--
--
  , overwrite :: Core.Maybe Core.Bool
    -- ^ Overwrite an existing parameter. If not specified, will default to "false".
  , policies :: Core.Maybe Types.Policies
    -- ^ One or more policies to apply to a parameter. This action takes a JSON array. Parameter Store supports the following policy types:
--
-- Expiration: This policy deletes the parameter after it expires. When you create the policy, you specify the expiration date. You can update the expiration date and time by updating the policy. Updating the /parameter/ does not affect the expiration date and time. When the expiration time is reached, Parameter Store deletes the parameter.
-- ExpirationNotification: This policy triggers an event in Amazon CloudWatch Events that notifies you about the expiration. By using this policy, you can receive notification before or after the expiration time is reached, in units of days or hours.
-- NoChangeNotification: This policy triggers a CloudWatch event if a parameter has not been modified for a specified period of time. This policy type is useful when, for example, a secret needs to be changed within a period of time, but it has not been changed.
-- All existing policies are preserved until you send new policies or an empty policy. For more information about parameter policies, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-store-policies.html Assigning parameter policies> . 
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Optional metadata that you assign to a resource. Tags enable you to categorize a resource in different ways, such as by purpose, owner, or environment. For example, you might want to tag a Systems Manager parameter to identify the type of resource to which it applies, the environment, or the type of configuration data referenced by the parameter. In this case, you could specify the following key name/value pairs:
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
  , tier :: Core.Maybe Types.ParameterTier
    -- ^ The parameter tier to assign to a parameter.
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
  , type' :: Core.Maybe Types.ParameterType
    -- ^ The type of parameter that you want to add to the system.
--
-- Items in a @StringList@ must be separated by a comma (,). You can't use other punctuation or special character to escape items in the list. If you have a parameter value that requires a comma, then use the @String@ data type.
-- /Important:/ Specifying a parameter type is not required when updating a parameter. You must specify a parameter type when creating a parameter.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutParameter' value with any optional fields omitted.
mkPutParameter
    :: Types.PSParameterName -- ^ 'name'
    -> Types.Value -- ^ 'value'
    -> PutParameter
mkPutParameter name value
  = PutParameter'{name, value, allowedPattern = Core.Nothing,
                  dataType = Core.Nothing, description = Core.Nothing,
                  keyId = Core.Nothing, overwrite = Core.Nothing,
                  policies = Core.Nothing, tags = Core.Nothing, tier = Core.Nothing,
                  type' = Core.Nothing}

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
ppName :: Lens.Lens' PutParameter Types.PSParameterName
ppName = Lens.field @"name"
{-# INLINEABLE ppName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The parameter value that you want to add to the system. Standard parameters have a value limit of 4 KB. Advanced parameters have a value limit of 8 KB.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppValue :: Lens.Lens' PutParameter Types.Value
ppValue = Lens.field @"value"
{-# INLINEABLE ppValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

-- | A regular expression used to validate the parameter value. For example, for String types with values restricted to numbers, you can specify the following: AllowedPattern=^\d+$ 
--
-- /Note:/ Consider using 'allowedPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppAllowedPattern :: Lens.Lens' PutParameter (Core.Maybe Types.AllowedPattern)
ppAllowedPattern = Lens.field @"allowedPattern"
{-# INLINEABLE ppAllowedPattern #-}
{-# DEPRECATED allowedPattern "Use generic-lens or generic-optics with 'allowedPattern' instead"  #-}

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
ppDataType :: Lens.Lens' PutParameter (Core.Maybe Types.DataType)
ppDataType = Lens.field @"dataType"
{-# INLINEABLE ppDataType #-}
{-# DEPRECATED dataType "Use generic-lens or generic-optics with 'dataType' instead"  #-}

-- | Information about the parameter that you want to add to the system. Optional but recommended.
--
-- /Important:/ Do not enter personally identifiable information in this field.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppDescription :: Lens.Lens' PutParameter (Core.Maybe Types.ParameterDescription)
ppDescription = Lens.field @"description"
{-# INLINEABLE ppDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

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
ppKeyId :: Lens.Lens' PutParameter (Core.Maybe Types.ParameterKeyId)
ppKeyId = Lens.field @"keyId"
{-# INLINEABLE ppKeyId #-}
{-# DEPRECATED keyId "Use generic-lens or generic-optics with 'keyId' instead"  #-}

-- | Overwrite an existing parameter. If not specified, will default to "false".
--
-- /Note:/ Consider using 'overwrite' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppOverwrite :: Lens.Lens' PutParameter (Core.Maybe Core.Bool)
ppOverwrite = Lens.field @"overwrite"
{-# INLINEABLE ppOverwrite #-}
{-# DEPRECATED overwrite "Use generic-lens or generic-optics with 'overwrite' instead"  #-}

-- | One or more policies to apply to a parameter. This action takes a JSON array. Parameter Store supports the following policy types:
--
-- Expiration: This policy deletes the parameter after it expires. When you create the policy, you specify the expiration date. You can update the expiration date and time by updating the policy. Updating the /parameter/ does not affect the expiration date and time. When the expiration time is reached, Parameter Store deletes the parameter.
-- ExpirationNotification: This policy triggers an event in Amazon CloudWatch Events that notifies you about the expiration. By using this policy, you can receive notification before or after the expiration time is reached, in units of days or hours.
-- NoChangeNotification: This policy triggers a CloudWatch event if a parameter has not been modified for a specified period of time. This policy type is useful when, for example, a secret needs to be changed within a period of time, but it has not been changed.
-- All existing policies are preserved until you send new policies or an empty policy. For more information about parameter policies, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-store-policies.html Assigning parameter policies> . 
--
-- /Note:/ Consider using 'policies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppPolicies :: Lens.Lens' PutParameter (Core.Maybe Types.Policies)
ppPolicies = Lens.field @"policies"
{-# INLINEABLE ppPolicies #-}
{-# DEPRECATED policies "Use generic-lens or generic-optics with 'policies' instead"  #-}

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
ppTags :: Lens.Lens' PutParameter (Core.Maybe [Types.Tag])
ppTags = Lens.field @"tags"
{-# INLINEABLE ppTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

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
ppTier :: Lens.Lens' PutParameter (Core.Maybe Types.ParameterTier)
ppTier = Lens.field @"tier"
{-# INLINEABLE ppTier #-}
{-# DEPRECATED tier "Use generic-lens or generic-optics with 'tier' instead"  #-}

-- | The type of parameter that you want to add to the system.
--
-- Items in a @StringList@ must be separated by a comma (,). You can't use other punctuation or special character to escape items in the list. If you have a parameter value that requires a comma, then use the @String@ data type.
-- /Important:/ Specifying a parameter type is not required when updating a parameter. You must specify a parameter type when creating a parameter.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppType :: Lens.Lens' PutParameter (Core.Maybe Types.ParameterType)
ppType = Lens.field @"type'"
{-# INLINEABLE ppType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.ToQuery PutParameter where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutParameter where
        toHeaders PutParameter{..}
          = Core.pure ("X-Amz-Target", "AmazonSSM.PutParameter") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutParameter where
        toJSON PutParameter{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("Value" Core..= value),
                  ("AllowedPattern" Core..=) Core.<$> allowedPattern,
                  ("DataType" Core..=) Core.<$> dataType,
                  ("Description" Core..=) Core.<$> description,
                  ("KeyId" Core..=) Core.<$> keyId,
                  ("Overwrite" Core..=) Core.<$> overwrite,
                  ("Policies" Core..=) Core.<$> policies,
                  ("Tags" Core..=) Core.<$> tags, ("Tier" Core..=) Core.<$> tier,
                  ("Type" Core..=) Core.<$> type'])

instance Core.AWSRequest PutParameter where
        type Rs PutParameter = PutParameterResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 PutParameterResponse' Core.<$>
                   (x Core..:? "Tier") Core.<*> x Core..:? "Version" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutParameterResponse' smart constructor.
data PutParameterResponse = PutParameterResponse'
  { tier :: Core.Maybe Types.ParameterTier
    -- ^ The tier assigned to the parameter.
  , version :: Core.Maybe Core.Integer
    -- ^ The new version number of a parameter. If you edit a parameter value, Parameter Store automatically creates a new version and assigns this new version a unique ID. You can reference a parameter version ID in API actions or in Systems Manager documents (SSM documents). By default, if you don't specify a specific version, the system returns the latest parameter value when a parameter is called.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutParameterResponse' value with any optional fields omitted.
mkPutParameterResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutParameterResponse
mkPutParameterResponse responseStatus
  = PutParameterResponse'{tier = Core.Nothing,
                          version = Core.Nothing, responseStatus}

-- | The tier assigned to the parameter.
--
-- /Note:/ Consider using 'tier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pprrsTier :: Lens.Lens' PutParameterResponse (Core.Maybe Types.ParameterTier)
pprrsTier = Lens.field @"tier"
{-# INLINEABLE pprrsTier #-}
{-# DEPRECATED tier "Use generic-lens or generic-optics with 'tier' instead"  #-}

-- | The new version number of a parameter. If you edit a parameter value, Parameter Store automatically creates a new version and assigns this new version a unique ID. You can reference a parameter version ID in API actions or in Systems Manager documents (SSM documents). By default, if you don't specify a specific version, the system returns the latest parameter value when a parameter is called.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pprrsVersion :: Lens.Lens' PutParameterResponse (Core.Maybe Core.Integer)
pprrsVersion = Lens.field @"version"
{-# INLINEABLE pprrsVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pprrsResponseStatus :: Lens.Lens' PutParameterResponse Core.Int
pprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE pprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
