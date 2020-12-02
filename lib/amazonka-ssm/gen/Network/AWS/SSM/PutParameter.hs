{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
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
  ( -- * Creating a Request
    putParameter,
    PutParameter,

    -- * Request Lenses
    ppKeyId,
    ppTier,
    ppAllowedPattern,
    ppType,
    ppDataType,
    ppOverwrite,
    ppDescription,
    ppPolicies,
    ppTags,
    ppName,
    ppValue,

    -- * Destructuring the Response
    putParameterResponse,
    PutParameterResponse,

    -- * Response Lenses
    pprsTier,
    pprsVersion,
    pprsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types

-- | /See:/ 'putParameter' smart constructor.
data PutParameter = PutParameter'
  { _ppKeyId :: !(Maybe Text),
    _ppTier :: !(Maybe ParameterTier),
    _ppAllowedPattern :: !(Maybe Text),
    _ppType :: !(Maybe ParameterType),
    _ppDataType :: !(Maybe Text),
    _ppOverwrite :: !(Maybe Bool),
    _ppDescription :: !(Maybe Text),
    _ppPolicies :: !(Maybe Text),
    _ppTags :: !(Maybe [Tag]),
    _ppName :: !Text,
    _ppValue :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutParameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppKeyId' - The KMS Key ID that you want to use to encrypt a parameter. Either the default AWS Key Management Service (AWS KMS) key automatically assigned to your AWS account or a custom key. Required for parameters that use the @SecureString@ data type. If you don't specify a key ID, the system uses the default key associated with your AWS account.     * To use your default AWS KMS key, choose the @SecureString@ data type, and do /not/ specify the @Key ID@ when you create the parameter. The system automatically populates @Key ID@ with your default KMS key.     * To use a custom KMS key, choose the @SecureString@ data type with the @Key ID@ parameter.
--
-- * 'ppTier' - The parameter tier to assign to a parameter. Parameter Store offers a standard tier and an advanced tier for parameters. Standard parameters have a content size limit of 4 KB and can't be configured to use parameter policies. You can create a maximum of 10,000 standard parameters for each Region in an AWS account. Standard parameters are offered at no additional cost.  Advanced parameters have a content size limit of 8 KB and can be configured to use parameter policies. You can create a maximum of 100,000 advanced parameters for each Region in an AWS account. Advanced parameters incur a charge. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-store-advanced-parameters.html Standard and advanced parameter tiers> in the /AWS Systems Manager User Guide/ . You can change a standard parameter to an advanced parameter any time. But you can't revert an advanced parameter to a standard parameter. Reverting an advanced parameter to a standard parameter would result in data loss because the system would truncate the size of the parameter from 8 KB to 4 KB. Reverting would also remove any policies attached to the parameter. Lastly, advanced parameters use a different form of encryption than standard parameters.  If you no longer need an advanced parameter, or if you no longer want to incur charges for an advanced parameter, you must delete it and recreate it as a new standard parameter.  __Using the Default Tier Configuration__  In @PutParameter@ requests, you can specify the tier to create the parameter in. Whenever you specify a tier in the request, Parameter Store creates or updates the parameter according to that request. However, if you do not specify a tier in a request, Parameter Store assigns the tier based on the current Parameter Store default tier configuration. The default tier when you begin using Parameter Store is the standard-parameter tier. If you use the advanced-parameter tier, you can specify one of the following as the default:     * __Advanced__ : With this option, Parameter Store evaluates all requests as advanced parameters.      * __Intelligent-Tiering__ : With this option, Parameter Store evaluates each request to determine if the parameter is standard or advanced.  If the request doesn't include any options that require an advanced parameter, the parameter is created in the standard-parameter tier. If one or more options requiring an advanced parameter are included in the request, Parameter Store create a parameter in the advanced-parameter tier. This approach helps control your parameter-related costs by always creating standard parameters unless an advanced parameter is necessary.  Options that require an advanced parameter include the following:     * The content size of the parameter is more than 4 KB.     * The parameter uses a parameter policy.     * More than 10,000 parameters already exist in your AWS account in the current Region. For more information about configuring the default tier option, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/ps-default-tier.html Specifying a default parameter tier> in the /AWS Systems Manager User Guide/ .
--
-- * 'ppAllowedPattern' - A regular expression used to validate the parameter value. For example, for String types with values restricted to numbers, you can specify the following: AllowedPattern=^\d+$
--
-- * 'ppType' - The type of parameter that you want to add to the system. Items in a @StringList@ must be separated by a comma (,). You can't use other punctuation or special character to escape items in the list. If you have a parameter value that requires a comma, then use the @String@ data type. /Important:/ Specifying a parameter type is not required when updating a parameter. You must specify a parameter type when creating a parameter.
--
-- * 'ppDataType' - The data type for a @String@ parameter. Supported data types include plain text and Amazon Machine Image IDs. __The following data type values are supported.__      * @text@      * @aws:ec2:image@  When you create a @String@ parameter and specify @aws:ec2:image@ , Systems Manager validates the parameter value is in the required format, such as @ami-12345abcdeEXAMPLE@ , and that the specified AMI is available in your AWS account. For more information, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-store-ec2-aliases.html Native parameter support for Amazon Machine Image IDs> in the /AWS Systems Manager User Guide/ .
--
-- * 'ppOverwrite' - Overwrite an existing parameter. If not specified, will default to "false".
--
-- * 'ppDescription' - Information about the parameter that you want to add to the system. Optional but recommended. /Important:/ Do not enter personally identifiable information in this field.
--
-- * 'ppPolicies' - One or more policies to apply to a parameter. This action takes a JSON array. Parameter Store supports the following policy types: Expiration: This policy deletes the parameter after it expires. When you create the policy, you specify the expiration date. You can update the expiration date and time by updating the policy. Updating the /parameter/ does not affect the expiration date and time. When the expiration time is reached, Parameter Store deletes the parameter. ExpirationNotification: This policy triggers an event in Amazon CloudWatch Events that notifies you about the expiration. By using this policy, you can receive notification before or after the expiration time is reached, in units of days or hours. NoChangeNotification: This policy triggers a CloudWatch event if a parameter has not been modified for a specified period of time. This policy type is useful when, for example, a secret needs to be changed within a period of time, but it has not been changed. All existing policies are preserved until you send new policies or an empty policy. For more information about parameter policies, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-store-policies.html Assigning parameter policies> .
--
-- * 'ppTags' - Optional metadata that you assign to a resource. Tags enable you to categorize a resource in different ways, such as by purpose, owner, or environment. For example, you might want to tag a Systems Manager parameter to identify the type of resource to which it applies, the environment, or the type of configuration data referenced by the parameter. In this case, you could specify the following key name/value pairs:     * @Key=Resource,Value=S3bucket@      * @Key=OS,Value=Windows@      * @Key=ParameterType,Value=LicenseKey@
--
-- * 'ppName' - The fully qualified name of the parameter that you want to add to the system. The fully qualified name includes the complete hierarchy of the parameter path and name. For parameters in a hierarchy, you must include a leading forward slash character (/) when you create or reference a parameter. For example: @/Dev/DBServer/MySQL/db-string13@  Naming Constraints:     * Parameter names are case sensitive.     * A parameter name must be unique within an AWS Region     * A parameter name can't be prefixed with "aws" or "ssm" (case-insensitive).     * Parameter names can include only the following symbols and letters: @a-zA-Z0-9_.-/@      * A parameter name can't include spaces.     * Parameter hierarchies are limited to a maximum depth of fifteen levels. For additional information about valid values for parameter names, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-parameter-name-constraints.html About requirements and constraints for parameter names> in the /AWS Systems Manager User Guide/ .
--
-- * 'ppValue' - The parameter value that you want to add to the system. Standard parameters have a value limit of 4 KB. Advanced parameters have a value limit of 8 KB.
putParameter ::
  -- | 'ppName'
  Text ->
  -- | 'ppValue'
  Text ->
  PutParameter
putParameter pName_ pValue_ =
  PutParameter'
    { _ppKeyId = Nothing,
      _ppTier = Nothing,
      _ppAllowedPattern = Nothing,
      _ppType = Nothing,
      _ppDataType = Nothing,
      _ppOverwrite = Nothing,
      _ppDescription = Nothing,
      _ppPolicies = Nothing,
      _ppTags = Nothing,
      _ppName = pName_,
      _ppValue = pValue_
    }

-- | The KMS Key ID that you want to use to encrypt a parameter. Either the default AWS Key Management Service (AWS KMS) key automatically assigned to your AWS account or a custom key. Required for parameters that use the @SecureString@ data type. If you don't specify a key ID, the system uses the default key associated with your AWS account.     * To use your default AWS KMS key, choose the @SecureString@ data type, and do /not/ specify the @Key ID@ when you create the parameter. The system automatically populates @Key ID@ with your default KMS key.     * To use a custom KMS key, choose the @SecureString@ data type with the @Key ID@ parameter.
ppKeyId :: Lens' PutParameter (Maybe Text)
ppKeyId = lens _ppKeyId (\s a -> s {_ppKeyId = a})

-- | The parameter tier to assign to a parameter. Parameter Store offers a standard tier and an advanced tier for parameters. Standard parameters have a content size limit of 4 KB and can't be configured to use parameter policies. You can create a maximum of 10,000 standard parameters for each Region in an AWS account. Standard parameters are offered at no additional cost.  Advanced parameters have a content size limit of 8 KB and can be configured to use parameter policies. You can create a maximum of 100,000 advanced parameters for each Region in an AWS account. Advanced parameters incur a charge. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-store-advanced-parameters.html Standard and advanced parameter tiers> in the /AWS Systems Manager User Guide/ . You can change a standard parameter to an advanced parameter any time. But you can't revert an advanced parameter to a standard parameter. Reverting an advanced parameter to a standard parameter would result in data loss because the system would truncate the size of the parameter from 8 KB to 4 KB. Reverting would also remove any policies attached to the parameter. Lastly, advanced parameters use a different form of encryption than standard parameters.  If you no longer need an advanced parameter, or if you no longer want to incur charges for an advanced parameter, you must delete it and recreate it as a new standard parameter.  __Using the Default Tier Configuration__  In @PutParameter@ requests, you can specify the tier to create the parameter in. Whenever you specify a tier in the request, Parameter Store creates or updates the parameter according to that request. However, if you do not specify a tier in a request, Parameter Store assigns the tier based on the current Parameter Store default tier configuration. The default tier when you begin using Parameter Store is the standard-parameter tier. If you use the advanced-parameter tier, you can specify one of the following as the default:     * __Advanced__ : With this option, Parameter Store evaluates all requests as advanced parameters.      * __Intelligent-Tiering__ : With this option, Parameter Store evaluates each request to determine if the parameter is standard or advanced.  If the request doesn't include any options that require an advanced parameter, the parameter is created in the standard-parameter tier. If one or more options requiring an advanced parameter are included in the request, Parameter Store create a parameter in the advanced-parameter tier. This approach helps control your parameter-related costs by always creating standard parameters unless an advanced parameter is necessary.  Options that require an advanced parameter include the following:     * The content size of the parameter is more than 4 KB.     * The parameter uses a parameter policy.     * More than 10,000 parameters already exist in your AWS account in the current Region. For more information about configuring the default tier option, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/ps-default-tier.html Specifying a default parameter tier> in the /AWS Systems Manager User Guide/ .
ppTier :: Lens' PutParameter (Maybe ParameterTier)
ppTier = lens _ppTier (\s a -> s {_ppTier = a})

-- | A regular expression used to validate the parameter value. For example, for String types with values restricted to numbers, you can specify the following: AllowedPattern=^\d+$
ppAllowedPattern :: Lens' PutParameter (Maybe Text)
ppAllowedPattern = lens _ppAllowedPattern (\s a -> s {_ppAllowedPattern = a})

-- | The type of parameter that you want to add to the system. Items in a @StringList@ must be separated by a comma (,). You can't use other punctuation or special character to escape items in the list. If you have a parameter value that requires a comma, then use the @String@ data type. /Important:/ Specifying a parameter type is not required when updating a parameter. You must specify a parameter type when creating a parameter.
ppType :: Lens' PutParameter (Maybe ParameterType)
ppType = lens _ppType (\s a -> s {_ppType = a})

-- | The data type for a @String@ parameter. Supported data types include plain text and Amazon Machine Image IDs. __The following data type values are supported.__      * @text@      * @aws:ec2:image@  When you create a @String@ parameter and specify @aws:ec2:image@ , Systems Manager validates the parameter value is in the required format, such as @ami-12345abcdeEXAMPLE@ , and that the specified AMI is available in your AWS account. For more information, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-store-ec2-aliases.html Native parameter support for Amazon Machine Image IDs> in the /AWS Systems Manager User Guide/ .
ppDataType :: Lens' PutParameter (Maybe Text)
ppDataType = lens _ppDataType (\s a -> s {_ppDataType = a})

-- | Overwrite an existing parameter. If not specified, will default to "false".
ppOverwrite :: Lens' PutParameter (Maybe Bool)
ppOverwrite = lens _ppOverwrite (\s a -> s {_ppOverwrite = a})

-- | Information about the parameter that you want to add to the system. Optional but recommended. /Important:/ Do not enter personally identifiable information in this field.
ppDescription :: Lens' PutParameter (Maybe Text)
ppDescription = lens _ppDescription (\s a -> s {_ppDescription = a})

-- | One or more policies to apply to a parameter. This action takes a JSON array. Parameter Store supports the following policy types: Expiration: This policy deletes the parameter after it expires. When you create the policy, you specify the expiration date. You can update the expiration date and time by updating the policy. Updating the /parameter/ does not affect the expiration date and time. When the expiration time is reached, Parameter Store deletes the parameter. ExpirationNotification: This policy triggers an event in Amazon CloudWatch Events that notifies you about the expiration. By using this policy, you can receive notification before or after the expiration time is reached, in units of days or hours. NoChangeNotification: This policy triggers a CloudWatch event if a parameter has not been modified for a specified period of time. This policy type is useful when, for example, a secret needs to be changed within a period of time, but it has not been changed. All existing policies are preserved until you send new policies or an empty policy. For more information about parameter policies, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-store-policies.html Assigning parameter policies> .
ppPolicies :: Lens' PutParameter (Maybe Text)
ppPolicies = lens _ppPolicies (\s a -> s {_ppPolicies = a})

-- | Optional metadata that you assign to a resource. Tags enable you to categorize a resource in different ways, such as by purpose, owner, or environment. For example, you might want to tag a Systems Manager parameter to identify the type of resource to which it applies, the environment, or the type of configuration data referenced by the parameter. In this case, you could specify the following key name/value pairs:     * @Key=Resource,Value=S3bucket@      * @Key=OS,Value=Windows@      * @Key=ParameterType,Value=LicenseKey@
ppTags :: Lens' PutParameter [Tag]
ppTags = lens _ppTags (\s a -> s {_ppTags = a}) . _Default . _Coerce

-- | The fully qualified name of the parameter that you want to add to the system. The fully qualified name includes the complete hierarchy of the parameter path and name. For parameters in a hierarchy, you must include a leading forward slash character (/) when you create or reference a parameter. For example: @/Dev/DBServer/MySQL/db-string13@  Naming Constraints:     * Parameter names are case sensitive.     * A parameter name must be unique within an AWS Region     * A parameter name can't be prefixed with "aws" or "ssm" (case-insensitive).     * Parameter names can include only the following symbols and letters: @a-zA-Z0-9_.-/@      * A parameter name can't include spaces.     * Parameter hierarchies are limited to a maximum depth of fifteen levels. For additional information about valid values for parameter names, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-parameter-name-constraints.html About requirements and constraints for parameter names> in the /AWS Systems Manager User Guide/ .
ppName :: Lens' PutParameter Text
ppName = lens _ppName (\s a -> s {_ppName = a})

-- | The parameter value that you want to add to the system. Standard parameters have a value limit of 4 KB. Advanced parameters have a value limit of 8 KB.
ppValue :: Lens' PutParameter Text
ppValue = lens _ppValue (\s a -> s {_ppValue = a})

instance AWSRequest PutParameter where
  type Rs PutParameter = PutParameterResponse
  request = postJSON ssm
  response =
    receiveJSON
      ( \s h x ->
          PutParameterResponse'
            <$> (x .?> "Tier") <*> (x .?> "Version") <*> (pure (fromEnum s))
      )

instance Hashable PutParameter

instance NFData PutParameter

instance ToHeaders PutParameter where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AmazonSSM.PutParameter" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON PutParameter where
  toJSON PutParameter' {..} =
    object
      ( catMaybes
          [ ("KeyId" .=) <$> _ppKeyId,
            ("Tier" .=) <$> _ppTier,
            ("AllowedPattern" .=) <$> _ppAllowedPattern,
            ("Type" .=) <$> _ppType,
            ("DataType" .=) <$> _ppDataType,
            ("Overwrite" .=) <$> _ppOverwrite,
            ("Description" .=) <$> _ppDescription,
            ("Policies" .=) <$> _ppPolicies,
            ("Tags" .=) <$> _ppTags,
            Just ("Name" .= _ppName),
            Just ("Value" .= _ppValue)
          ]
      )

instance ToPath PutParameter where
  toPath = const "/"

instance ToQuery PutParameter where
  toQuery = const mempty

-- | /See:/ 'putParameterResponse' smart constructor.
data PutParameterResponse = PutParameterResponse'
  { _pprsTier ::
      !(Maybe ParameterTier),
    _pprsVersion :: !(Maybe Integer),
    _pprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutParameterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pprsTier' - The tier assigned to the parameter.
--
-- * 'pprsVersion' - The new version number of a parameter. If you edit a parameter value, Parameter Store automatically creates a new version and assigns this new version a unique ID. You can reference a parameter version ID in API actions or in Systems Manager documents (SSM documents). By default, if you don't specify a specific version, the system returns the latest parameter value when a parameter is called.
--
-- * 'pprsResponseStatus' - -- | The response status code.
putParameterResponse ::
  -- | 'pprsResponseStatus'
  Int ->
  PutParameterResponse
putParameterResponse pResponseStatus_ =
  PutParameterResponse'
    { _pprsTier = Nothing,
      _pprsVersion = Nothing,
      _pprsResponseStatus = pResponseStatus_
    }

-- | The tier assigned to the parameter.
pprsTier :: Lens' PutParameterResponse (Maybe ParameterTier)
pprsTier = lens _pprsTier (\s a -> s {_pprsTier = a})

-- | The new version number of a parameter. If you edit a parameter value, Parameter Store automatically creates a new version and assigns this new version a unique ID. You can reference a parameter version ID in API actions or in Systems Manager documents (SSM documents). By default, if you don't specify a specific version, the system returns the latest parameter value when a parameter is called.
pprsVersion :: Lens' PutParameterResponse (Maybe Integer)
pprsVersion = lens _pprsVersion (\s a -> s {_pprsVersion = a})

-- | -- | The response status code.
pprsResponseStatus :: Lens' PutParameterResponse Int
pprsResponseStatus = lens _pprsResponseStatus (\s a -> s {_pprsResponseStatus = a})

instance NFData PutParameterResponse
