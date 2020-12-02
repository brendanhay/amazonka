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
-- Module      : Network.AWS.CloudFormation.DescribeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns detailed information about a type that has been registered.
--
--
-- If you specify a @VersionId@ , @DescribeType@ returns information about that specific type version. Otherwise, it returns information about the default type version.
module Network.AWS.CloudFormation.DescribeType
  ( -- * Creating a Request
    describeType,
    DescribeType,

    -- * Request Lenses
    dtVersionId,
    dtTypeName,
    dtARN,
    dtType,

    -- * Destructuring the Response
    describeTypeResponse,
    DescribeTypeResponse,

    -- * Response Lenses
    dttrsLastUpdated,
    dttrsTypeName,
    dttrsARN,
    dttrsExecutionRoleARN,
    dttrsVisibility,
    dttrsSchema,
    dttrsDefaultVersionId,
    dttrsDeprecatedStatus,
    dttrsTimeCreated,
    dttrsType,
    dttrsIsDefaultVersion,
    dttrsDescription,
    dttrsSourceURL,
    dttrsDocumentationURL,
    dttrsProvisioningType,
    dttrsLoggingConfig,
    dttrsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeType' smart constructor.
data DescribeType = DescribeType'
  { _dtVersionId :: !(Maybe Text),
    _dtTypeName :: !(Maybe Text),
    _dtARN :: !(Maybe Text),
    _dtType :: !(Maybe RegistryType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtVersionId' - The ID of a specific version of the type. The version ID is the value at the end of the Amazon Resource Name (ARN) assigned to the type version when it is registered. If you specify a @VersionId@ , @DescribeType@ returns information about that specific type version. Otherwise, it returns information about the default type version.
--
-- * 'dtTypeName' - The name of the type. Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- * 'dtARN' - The Amazon Resource Name (ARN) of the type. Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- * 'dtType' - The kind of type.  Currently the only valid value is @RESOURCE@ . Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
describeType ::
  DescribeType
describeType =
  DescribeType'
    { _dtVersionId = Nothing,
      _dtTypeName = Nothing,
      _dtARN = Nothing,
      _dtType = Nothing
    }

-- | The ID of a specific version of the type. The version ID is the value at the end of the Amazon Resource Name (ARN) assigned to the type version when it is registered. If you specify a @VersionId@ , @DescribeType@ returns information about that specific type version. Otherwise, it returns information about the default type version.
dtVersionId :: Lens' DescribeType (Maybe Text)
dtVersionId = lens _dtVersionId (\s a -> s {_dtVersionId = a})

-- | The name of the type. Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
dtTypeName :: Lens' DescribeType (Maybe Text)
dtTypeName = lens _dtTypeName (\s a -> s {_dtTypeName = a})

-- | The Amazon Resource Name (ARN) of the type. Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
dtARN :: Lens' DescribeType (Maybe Text)
dtARN = lens _dtARN (\s a -> s {_dtARN = a})

-- | The kind of type.  Currently the only valid value is @RESOURCE@ . Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
dtType :: Lens' DescribeType (Maybe RegistryType)
dtType = lens _dtType (\s a -> s {_dtType = a})

instance AWSRequest DescribeType where
  type Rs DescribeType = DescribeTypeResponse
  request = postQuery cloudFormation
  response =
    receiveXMLWrapper
      "DescribeTypeResult"
      ( \s h x ->
          DescribeTypeResponse'
            <$> (x .@? "LastUpdated")
            <*> (x .@? "TypeName")
            <*> (x .@? "Arn")
            <*> (x .@? "ExecutionRoleArn")
            <*> (x .@? "Visibility")
            <*> (x .@? "Schema")
            <*> (x .@? "DefaultVersionId")
            <*> (x .@? "DeprecatedStatus")
            <*> (x .@? "TimeCreated")
            <*> (x .@? "Type")
            <*> (x .@? "IsDefaultVersion")
            <*> (x .@? "Description")
            <*> (x .@? "SourceUrl")
            <*> (x .@? "DocumentationUrl")
            <*> (x .@? "ProvisioningType")
            <*> (x .@? "LoggingConfig")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeType

instance NFData DescribeType

instance ToHeaders DescribeType where
  toHeaders = const mempty

instance ToPath DescribeType where
  toPath = const "/"

instance ToQuery DescribeType where
  toQuery DescribeType' {..} =
    mconcat
      [ "Action" =: ("DescribeType" :: ByteString),
        "Version" =: ("2010-05-15" :: ByteString),
        "VersionId" =: _dtVersionId,
        "TypeName" =: _dtTypeName,
        "Arn" =: _dtARN,
        "Type" =: _dtType
      ]

-- | /See:/ 'describeTypeResponse' smart constructor.
data DescribeTypeResponse = DescribeTypeResponse'
  { _dttrsLastUpdated ::
      !(Maybe ISO8601),
    _dttrsTypeName :: !(Maybe Text),
    _dttrsARN :: !(Maybe Text),
    _dttrsExecutionRoleARN :: !(Maybe Text),
    _dttrsVisibility :: !(Maybe Visibility),
    _dttrsSchema :: !(Maybe Text),
    _dttrsDefaultVersionId :: !(Maybe Text),
    _dttrsDeprecatedStatus ::
      !(Maybe DeprecatedStatus),
    _dttrsTimeCreated :: !(Maybe ISO8601),
    _dttrsType :: !(Maybe RegistryType),
    _dttrsIsDefaultVersion :: !(Maybe Bool),
    _dttrsDescription :: !(Maybe Text),
    _dttrsSourceURL :: !(Maybe Text),
    _dttrsDocumentationURL :: !(Maybe Text),
    _dttrsProvisioningType ::
      !(Maybe ProvisioningType),
    _dttrsLoggingConfig :: !(Maybe LoggingConfig),
    _dttrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeTypeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dttrsLastUpdated' - When the specified type version was registered.
--
-- * 'dttrsTypeName' - The name of the registered type.
--
-- * 'dttrsARN' - The Amazon Resource Name (ARN) of the type.
--
-- * 'dttrsExecutionRoleARN' - The Amazon Resource Name (ARN) of the IAM execution role used to register the type. If your resource type calls AWS APIs in any of its handlers, you must create an /<https:\/\/docs.aws.amazon.com\/IAM\/latest\/UserGuide\/id_roles.html IAM execution role> / that includes the necessary permissions to call those AWS APIs, and provision that execution role in your account. CloudFormation then assumes that execution role to provide your resource type with the appropriate credentials.
--
-- * 'dttrsVisibility' - The scope at which the type is visible and usable in CloudFormation operations. Valid values include:     * @PRIVATE@ : The type is only visible and usable within the account in which it is registered. Currently, AWS CloudFormation marks any types you register as @PRIVATE@ .     * @PUBLIC@ : The type is publically visible and usable within any Amazon account.
--
-- * 'dttrsSchema' - The schema that defines the type. For more information on type schemas, see <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-schema.html Resource Provider Schema> in the /CloudFormation CLI User Guide/ .
--
-- * 'dttrsDefaultVersionId' - The ID of the default version of the type. The default version is used when the type version is not specified. To set the default version of a type, use @'SetTypeDefaultVersion' @ .
--
-- * 'dttrsDeprecatedStatus' - The deprecation status of the type. Valid values include:     * @LIVE@ : The type is registered and can be used in CloudFormation operations, dependent on its provisioning behavior and visibility scope.     * @DEPRECATED@ : The type has been deregistered and can no longer be used in CloudFormation operations.
--
-- * 'dttrsTimeCreated' - When the specified type version was registered.
--
-- * 'dttrsType' - The kind of type.  Currently the only valid value is @RESOURCE@ .
--
-- * 'dttrsIsDefaultVersion' - Whether the specified type version is set as the default version.
--
-- * 'dttrsDescription' - The description of the registered type.
--
-- * 'dttrsSourceURL' - The URL of the source code for the type.
--
-- * 'dttrsDocumentationURL' - The URL of a page providing detailed documentation for this type.
--
-- * 'dttrsProvisioningType' - The provisioning behavior of the type. AWS CloudFormation determines the provisioning type during registration, based on the types of handlers in the schema handler package submitted. Valid values include:     * @FULLY_MUTABLE@ : The type includes an update handler to process updates to the type during stack update operations.     * @IMMUTABLE@ : The type does not include an update handler, so the type cannot be updated and must instead be replaced during stack update operations.     * @NON_PROVISIONABLE@ : The type does not include all of the following handlers, and therefore cannot actually be provisioned.     * create     * read     * delete
--
-- * 'dttrsLoggingConfig' - Contains logging configuration information for a type.
--
-- * 'dttrsResponseStatus' - -- | The response status code.
describeTypeResponse ::
  -- | 'dttrsResponseStatus'
  Int ->
  DescribeTypeResponse
describeTypeResponse pResponseStatus_ =
  DescribeTypeResponse'
    { _dttrsLastUpdated = Nothing,
      _dttrsTypeName = Nothing,
      _dttrsARN = Nothing,
      _dttrsExecutionRoleARN = Nothing,
      _dttrsVisibility = Nothing,
      _dttrsSchema = Nothing,
      _dttrsDefaultVersionId = Nothing,
      _dttrsDeprecatedStatus = Nothing,
      _dttrsTimeCreated = Nothing,
      _dttrsType = Nothing,
      _dttrsIsDefaultVersion = Nothing,
      _dttrsDescription = Nothing,
      _dttrsSourceURL = Nothing,
      _dttrsDocumentationURL = Nothing,
      _dttrsProvisioningType = Nothing,
      _dttrsLoggingConfig = Nothing,
      _dttrsResponseStatus = pResponseStatus_
    }

-- | When the specified type version was registered.
dttrsLastUpdated :: Lens' DescribeTypeResponse (Maybe UTCTime)
dttrsLastUpdated = lens _dttrsLastUpdated (\s a -> s {_dttrsLastUpdated = a}) . mapping _Time

-- | The name of the registered type.
dttrsTypeName :: Lens' DescribeTypeResponse (Maybe Text)
dttrsTypeName = lens _dttrsTypeName (\s a -> s {_dttrsTypeName = a})

-- | The Amazon Resource Name (ARN) of the type.
dttrsARN :: Lens' DescribeTypeResponse (Maybe Text)
dttrsARN = lens _dttrsARN (\s a -> s {_dttrsARN = a})

-- | The Amazon Resource Name (ARN) of the IAM execution role used to register the type. If your resource type calls AWS APIs in any of its handlers, you must create an /<https:\/\/docs.aws.amazon.com\/IAM\/latest\/UserGuide\/id_roles.html IAM execution role> / that includes the necessary permissions to call those AWS APIs, and provision that execution role in your account. CloudFormation then assumes that execution role to provide your resource type with the appropriate credentials.
dttrsExecutionRoleARN :: Lens' DescribeTypeResponse (Maybe Text)
dttrsExecutionRoleARN = lens _dttrsExecutionRoleARN (\s a -> s {_dttrsExecutionRoleARN = a})

-- | The scope at which the type is visible and usable in CloudFormation operations. Valid values include:     * @PRIVATE@ : The type is only visible and usable within the account in which it is registered. Currently, AWS CloudFormation marks any types you register as @PRIVATE@ .     * @PUBLIC@ : The type is publically visible and usable within any Amazon account.
dttrsVisibility :: Lens' DescribeTypeResponse (Maybe Visibility)
dttrsVisibility = lens _dttrsVisibility (\s a -> s {_dttrsVisibility = a})

-- | The schema that defines the type. For more information on type schemas, see <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-schema.html Resource Provider Schema> in the /CloudFormation CLI User Guide/ .
dttrsSchema :: Lens' DescribeTypeResponse (Maybe Text)
dttrsSchema = lens _dttrsSchema (\s a -> s {_dttrsSchema = a})

-- | The ID of the default version of the type. The default version is used when the type version is not specified. To set the default version of a type, use @'SetTypeDefaultVersion' @ .
dttrsDefaultVersionId :: Lens' DescribeTypeResponse (Maybe Text)
dttrsDefaultVersionId = lens _dttrsDefaultVersionId (\s a -> s {_dttrsDefaultVersionId = a})

-- | The deprecation status of the type. Valid values include:     * @LIVE@ : The type is registered and can be used in CloudFormation operations, dependent on its provisioning behavior and visibility scope.     * @DEPRECATED@ : The type has been deregistered and can no longer be used in CloudFormation operations.
dttrsDeprecatedStatus :: Lens' DescribeTypeResponse (Maybe DeprecatedStatus)
dttrsDeprecatedStatus = lens _dttrsDeprecatedStatus (\s a -> s {_dttrsDeprecatedStatus = a})

-- | When the specified type version was registered.
dttrsTimeCreated :: Lens' DescribeTypeResponse (Maybe UTCTime)
dttrsTimeCreated = lens _dttrsTimeCreated (\s a -> s {_dttrsTimeCreated = a}) . mapping _Time

-- | The kind of type.  Currently the only valid value is @RESOURCE@ .
dttrsType :: Lens' DescribeTypeResponse (Maybe RegistryType)
dttrsType = lens _dttrsType (\s a -> s {_dttrsType = a})

-- | Whether the specified type version is set as the default version.
dttrsIsDefaultVersion :: Lens' DescribeTypeResponse (Maybe Bool)
dttrsIsDefaultVersion = lens _dttrsIsDefaultVersion (\s a -> s {_dttrsIsDefaultVersion = a})

-- | The description of the registered type.
dttrsDescription :: Lens' DescribeTypeResponse (Maybe Text)
dttrsDescription = lens _dttrsDescription (\s a -> s {_dttrsDescription = a})

-- | The URL of the source code for the type.
dttrsSourceURL :: Lens' DescribeTypeResponse (Maybe Text)
dttrsSourceURL = lens _dttrsSourceURL (\s a -> s {_dttrsSourceURL = a})

-- | The URL of a page providing detailed documentation for this type.
dttrsDocumentationURL :: Lens' DescribeTypeResponse (Maybe Text)
dttrsDocumentationURL = lens _dttrsDocumentationURL (\s a -> s {_dttrsDocumentationURL = a})

-- | The provisioning behavior of the type. AWS CloudFormation determines the provisioning type during registration, based on the types of handlers in the schema handler package submitted. Valid values include:     * @FULLY_MUTABLE@ : The type includes an update handler to process updates to the type during stack update operations.     * @IMMUTABLE@ : The type does not include an update handler, so the type cannot be updated and must instead be replaced during stack update operations.     * @NON_PROVISIONABLE@ : The type does not include all of the following handlers, and therefore cannot actually be provisioned.     * create     * read     * delete
dttrsProvisioningType :: Lens' DescribeTypeResponse (Maybe ProvisioningType)
dttrsProvisioningType = lens _dttrsProvisioningType (\s a -> s {_dttrsProvisioningType = a})

-- | Contains logging configuration information for a type.
dttrsLoggingConfig :: Lens' DescribeTypeResponse (Maybe LoggingConfig)
dttrsLoggingConfig = lens _dttrsLoggingConfig (\s a -> s {_dttrsLoggingConfig = a})

-- | -- | The response status code.
dttrsResponseStatus :: Lens' DescribeTypeResponse Int
dttrsResponseStatus = lens _dttrsResponseStatus (\s a -> s {_dttrsResponseStatus = a})

instance NFData DescribeTypeResponse
