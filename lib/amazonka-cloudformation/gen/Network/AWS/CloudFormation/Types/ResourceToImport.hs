{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ResourceToImport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.ResourceToImport where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the target resource of an import operation.
--
--
--
-- /See:/ 'resourceToImport' smart constructor.
data ResourceToImport = ResourceToImport'
  { _rtiResourceType ::
      !Text,
    _rtiLogicalResourceId :: !Text,
    _rtiResourceIdentifier :: !(Map Text (Text))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceToImport' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtiResourceType' - The type of resource to import into your stack, such as @AWS::S3::Bucket@ . For a list of supported resource types, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/resource-import-supported-resources.html Resources that support import operations> in the AWS CloudFormation User Guide.
--
-- * 'rtiLogicalResourceId' - The logical ID of the target resource as specified in the template.
--
-- * 'rtiResourceIdentifier' - A key-value pair that identifies the target resource. The key is an identifier property (for example, @BucketName@ for @AWS::S3::Bucket@ resources) and the value is the actual property value (for example, @MyS3Bucket@ ).
resourceToImport ::
  -- | 'rtiResourceType'
  Text ->
  -- | 'rtiLogicalResourceId'
  Text ->
  ResourceToImport
resourceToImport pResourceType_ pLogicalResourceId_ =
  ResourceToImport'
    { _rtiResourceType = pResourceType_,
      _rtiLogicalResourceId = pLogicalResourceId_,
      _rtiResourceIdentifier = mempty
    }

-- | The type of resource to import into your stack, such as @AWS::S3::Bucket@ . For a list of supported resource types, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/resource-import-supported-resources.html Resources that support import operations> in the AWS CloudFormation User Guide.
rtiResourceType :: Lens' ResourceToImport Text
rtiResourceType = lens _rtiResourceType (\s a -> s {_rtiResourceType = a})

-- | The logical ID of the target resource as specified in the template.
rtiLogicalResourceId :: Lens' ResourceToImport Text
rtiLogicalResourceId = lens _rtiLogicalResourceId (\s a -> s {_rtiLogicalResourceId = a})

-- | A key-value pair that identifies the target resource. The key is an identifier property (for example, @BucketName@ for @AWS::S3::Bucket@ resources) and the value is the actual property value (for example, @MyS3Bucket@ ).
rtiResourceIdentifier :: Lens' ResourceToImport (HashMap Text (Text))
rtiResourceIdentifier = lens _rtiResourceIdentifier (\s a -> s {_rtiResourceIdentifier = a}) . _Map

instance Hashable ResourceToImport

instance NFData ResourceToImport

instance ToQuery ResourceToImport where
  toQuery ResourceToImport' {..} =
    mconcat
      [ "ResourceType" =: _rtiResourceType,
        "LogicalResourceId" =: _rtiLogicalResourceId,
        "ResourceIdentifier"
          =: toQueryMap "entry" "key" "value" _rtiResourceIdentifier
      ]
