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
-- Module      : Network.AWS.CloudFormation.RegisterType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a type with the CloudFormation service. Registering a type makes it available for use in CloudFormation templates in your AWS account, and includes:
--
--
--     * Validating the resource schema
--
--     * Determining which handlers have been specified for the resource
--
--     * Making the resource type available for use in your account
--
--
--
-- For more information on how to develop types and ready them for registeration, see <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-types.html Creating Resource Providers> in the /CloudFormation CLI User Guide/ .
--
-- You can have a maximum of 50 resource type versions registered at a time. This maximum is per account and per region. Use <AWSCloudFormation/latest/APIReference/API_DeregisterType.html DeregisterType> to deregister specific resource type versions if necessary.
--
-- Once you have initiated a registration request using @'RegisterType' @ , you can use @'DescribeTypeRegistration' @ to monitor the progress of the registration request.
module Network.AWS.CloudFormation.RegisterType
  ( -- * Creating a Request
    registerType,
    RegisterType,

    -- * Request Lenses
    rExecutionRoleARN,
    rType,
    rClientRequestToken,
    rLoggingConfig,
    rTypeName,
    rSchemaHandlerPackage,

    -- * Destructuring the Response
    registerTypeResponse,
    RegisterTypeResponse,

    -- * Response Lenses
    rtrsRegistrationToken,
    rtrsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'registerType' smart constructor.
data RegisterType = RegisterType'
  { _rExecutionRoleARN ::
      !(Maybe Text),
    _rType :: !(Maybe RegistryType),
    _rClientRequestToken :: !(Maybe Text),
    _rLoggingConfig :: !(Maybe LoggingConfig),
    _rTypeName :: !Text,
    _rSchemaHandlerPackage :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RegisterType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rExecutionRoleARN' - The Amazon Resource Name (ARN) of the IAM role for CloudFormation to assume when invoking the resource provider. If your resource type calls AWS APIs in any of its handlers, you must create an /<https:\/\/docs.aws.amazon.com\/IAM\/latest\/UserGuide\/id_roles.html IAM execution role> / that includes the necessary permissions to call those AWS APIs, and provision that execution role in your account. When CloudFormation needs to invoke the resource provider handler, CloudFormation assumes this execution role to create a temporary session token, which it then passes to the resource provider handler, thereby supplying your resource provider with the appropriate credentials.
--
-- * 'rType' - The kind of type. Currently, the only valid value is @RESOURCE@ .
--
-- * 'rClientRequestToken' - A unique identifier that acts as an idempotency key for this registration request. Specifying a client request token prevents CloudFormation from generating more than one version of a type from the same registeration request, even if the request is submitted multiple times.
--
-- * 'rLoggingConfig' - Specifies logging configuration information for a type.
--
-- * 'rTypeName' - The name of the type being registered. We recommend that type names adhere to the following pattern: /company_or_organization/ ::/service/ ::/type/ .
--
-- * 'rSchemaHandlerPackage' - A url to the S3 bucket containing the schema handler package that contains the schema, event handlers, and associated files for the type you want to register. For information on generating a schema handler package for the type you want to register, see <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-cli-submit.html submit> in the /CloudFormation CLI User Guide/ .
registerType ::
  -- | 'rTypeName'
  Text ->
  -- | 'rSchemaHandlerPackage'
  Text ->
  RegisterType
registerType pTypeName_ pSchemaHandlerPackage_ =
  RegisterType'
    { _rExecutionRoleARN = Nothing,
      _rType = Nothing,
      _rClientRequestToken = Nothing,
      _rLoggingConfig = Nothing,
      _rTypeName = pTypeName_,
      _rSchemaHandlerPackage = pSchemaHandlerPackage_
    }

-- | The Amazon Resource Name (ARN) of the IAM role for CloudFormation to assume when invoking the resource provider. If your resource type calls AWS APIs in any of its handlers, you must create an /<https:\/\/docs.aws.amazon.com\/IAM\/latest\/UserGuide\/id_roles.html IAM execution role> / that includes the necessary permissions to call those AWS APIs, and provision that execution role in your account. When CloudFormation needs to invoke the resource provider handler, CloudFormation assumes this execution role to create a temporary session token, which it then passes to the resource provider handler, thereby supplying your resource provider with the appropriate credentials.
rExecutionRoleARN :: Lens' RegisterType (Maybe Text)
rExecutionRoleARN = lens _rExecutionRoleARN (\s a -> s {_rExecutionRoleARN = a})

-- | The kind of type. Currently, the only valid value is @RESOURCE@ .
rType :: Lens' RegisterType (Maybe RegistryType)
rType = lens _rType (\s a -> s {_rType = a})

-- | A unique identifier that acts as an idempotency key for this registration request. Specifying a client request token prevents CloudFormation from generating more than one version of a type from the same registeration request, even if the request is submitted multiple times.
rClientRequestToken :: Lens' RegisterType (Maybe Text)
rClientRequestToken = lens _rClientRequestToken (\s a -> s {_rClientRequestToken = a})

-- | Specifies logging configuration information for a type.
rLoggingConfig :: Lens' RegisterType (Maybe LoggingConfig)
rLoggingConfig = lens _rLoggingConfig (\s a -> s {_rLoggingConfig = a})

-- | The name of the type being registered. We recommend that type names adhere to the following pattern: /company_or_organization/ ::/service/ ::/type/ .
rTypeName :: Lens' RegisterType Text
rTypeName = lens _rTypeName (\s a -> s {_rTypeName = a})

-- | A url to the S3 bucket containing the schema handler package that contains the schema, event handlers, and associated files for the type you want to register. For information on generating a schema handler package for the type you want to register, see <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-cli-submit.html submit> in the /CloudFormation CLI User Guide/ .
rSchemaHandlerPackage :: Lens' RegisterType Text
rSchemaHandlerPackage = lens _rSchemaHandlerPackage (\s a -> s {_rSchemaHandlerPackage = a})

instance AWSRequest RegisterType where
  type Rs RegisterType = RegisterTypeResponse
  request = postQuery cloudFormation
  response =
    receiveXMLWrapper
      "RegisterTypeResult"
      ( \s h x ->
          RegisterTypeResponse'
            <$> (x .@? "RegistrationToken") <*> (pure (fromEnum s))
      )

instance Hashable RegisterType

instance NFData RegisterType

instance ToHeaders RegisterType where
  toHeaders = const mempty

instance ToPath RegisterType where
  toPath = const "/"

instance ToQuery RegisterType where
  toQuery RegisterType' {..} =
    mconcat
      [ "Action" =: ("RegisterType" :: ByteString),
        "Version" =: ("2010-05-15" :: ByteString),
        "ExecutionRoleArn" =: _rExecutionRoleARN,
        "Type" =: _rType,
        "ClientRequestToken" =: _rClientRequestToken,
        "LoggingConfig" =: _rLoggingConfig,
        "TypeName" =: _rTypeName,
        "SchemaHandlerPackage" =: _rSchemaHandlerPackage
      ]

-- | /See:/ 'registerTypeResponse' smart constructor.
data RegisterTypeResponse = RegisterTypeResponse'
  { _rtrsRegistrationToken ::
      !(Maybe Text),
    _rtrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RegisterTypeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtrsRegistrationToken' - The identifier for this registration request. Use this registration token when calling @'DescribeTypeRegistration' @ , which returns information about the status and IDs of the type registration.
--
-- * 'rtrsResponseStatus' - -- | The response status code.
registerTypeResponse ::
  -- | 'rtrsResponseStatus'
  Int ->
  RegisterTypeResponse
registerTypeResponse pResponseStatus_ =
  RegisterTypeResponse'
    { _rtrsRegistrationToken = Nothing,
      _rtrsResponseStatus = pResponseStatus_
    }

-- | The identifier for this registration request. Use this registration token when calling @'DescribeTypeRegistration' @ , which returns information about the status and IDs of the type registration.
rtrsRegistrationToken :: Lens' RegisterTypeResponse (Maybe Text)
rtrsRegistrationToken = lens _rtrsRegistrationToken (\s a -> s {_rtrsRegistrationToken = a})

-- | -- | The response status code.
rtrsResponseStatus :: Lens' RegisterTypeResponse Int
rtrsResponseStatus = lens _rtrsResponseStatus (\s a -> s {_rtrsResponseStatus = a})

instance NFData RegisterTypeResponse
