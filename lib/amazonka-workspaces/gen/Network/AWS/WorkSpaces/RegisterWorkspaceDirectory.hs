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
-- Module      : Network.AWS.WorkSpaces.RegisterWorkspaceDirectory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers the specified directory. This operation is asynchronous and returns before the WorkSpace directory is registered. If this is the first time you are registering a directory, you will need to create the workspaces_DefaultRole role before you can register a directory. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/workspaces-access-control.html#create-default-role Creating the workspaces_DefaultRole Role> .
module Network.AWS.WorkSpaces.RegisterWorkspaceDirectory
  ( -- * Creating a Request
    registerWorkspaceDirectory,
    RegisterWorkspaceDirectory,

    -- * Request Lenses
    rwdSubnetIds,
    rwdEnableSelfService,
    rwdTenancy,
    rwdTags,
    rwdDirectoryId,
    rwdEnableWorkDocs,

    -- * Destructuring the Response
    registerWorkspaceDirectoryResponse,
    RegisterWorkspaceDirectoryResponse,

    -- * Response Lenses
    rwdrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'registerWorkspaceDirectory' smart constructor.
data RegisterWorkspaceDirectory = RegisterWorkspaceDirectory'
  { _rwdSubnetIds ::
      !(Maybe [Text]),
    _rwdEnableSelfService ::
      !(Maybe Bool),
    _rwdTenancy :: !(Maybe Tenancy),
    _rwdTags :: !(Maybe [Tag]),
    _rwdDirectoryId :: !Text,
    _rwdEnableWorkDocs :: !Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RegisterWorkspaceDirectory' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rwdSubnetIds' - The identifiers of the subnets for your virtual private cloud (VPC). Make sure that the subnets are in supported Availability Zones. The subnets must also be in separate Availability Zones. If these conditions are not met, you will receive an OperationNotSupportedException error.
--
-- * 'rwdEnableSelfService' - Indicates whether self-service capabilities are enabled or disabled.
--
-- * 'rwdTenancy' - Indicates whether your WorkSpace directory is dedicated or shared. To use Bring Your Own License (BYOL) images, this value must be set to @DEDICATED@ and your AWS account must be enabled for BYOL. If your account has not been enabled for BYOL, you will receive an InvalidParameterValuesException error. For more information about BYOL images, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Images> .
--
-- * 'rwdTags' - The tags associated with the directory.
--
-- * 'rwdDirectoryId' - The identifier of the directory. You cannot register a directory if it does not have a status of Active. If the directory does not have a status of Active, you will receive an InvalidResourceStateException error. If you have already registered the maximum number of directories that you can register with Amazon WorkSpaces, you will receive a ResourceLimitExceededException error. Deregister directories that you are not using for WorkSpaces, and try again.
--
-- * 'rwdEnableWorkDocs' - Indicates whether Amazon WorkDocs is enabled or disabled. If you have enabled this parameter and WorkDocs is not available in the Region, you will receive an OperationNotSupportedException error. Set @EnableWorkDocs@ to disabled, and try again.
registerWorkspaceDirectory ::
  -- | 'rwdDirectoryId'
  Text ->
  -- | 'rwdEnableWorkDocs'
  Bool ->
  RegisterWorkspaceDirectory
registerWorkspaceDirectory pDirectoryId_ pEnableWorkDocs_ =
  RegisterWorkspaceDirectory'
    { _rwdSubnetIds = Nothing,
      _rwdEnableSelfService = Nothing,
      _rwdTenancy = Nothing,
      _rwdTags = Nothing,
      _rwdDirectoryId = pDirectoryId_,
      _rwdEnableWorkDocs = pEnableWorkDocs_
    }

-- | The identifiers of the subnets for your virtual private cloud (VPC). Make sure that the subnets are in supported Availability Zones. The subnets must also be in separate Availability Zones. If these conditions are not met, you will receive an OperationNotSupportedException error.
rwdSubnetIds :: Lens' RegisterWorkspaceDirectory [Text]
rwdSubnetIds = lens _rwdSubnetIds (\s a -> s {_rwdSubnetIds = a}) . _Default . _Coerce

-- | Indicates whether self-service capabilities are enabled or disabled.
rwdEnableSelfService :: Lens' RegisterWorkspaceDirectory (Maybe Bool)
rwdEnableSelfService = lens _rwdEnableSelfService (\s a -> s {_rwdEnableSelfService = a})

-- | Indicates whether your WorkSpace directory is dedicated or shared. To use Bring Your Own License (BYOL) images, this value must be set to @DEDICATED@ and your AWS account must be enabled for BYOL. If your account has not been enabled for BYOL, you will receive an InvalidParameterValuesException error. For more information about BYOL images, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Images> .
rwdTenancy :: Lens' RegisterWorkspaceDirectory (Maybe Tenancy)
rwdTenancy = lens _rwdTenancy (\s a -> s {_rwdTenancy = a})

-- | The tags associated with the directory.
rwdTags :: Lens' RegisterWorkspaceDirectory [Tag]
rwdTags = lens _rwdTags (\s a -> s {_rwdTags = a}) . _Default . _Coerce

-- | The identifier of the directory. You cannot register a directory if it does not have a status of Active. If the directory does not have a status of Active, you will receive an InvalidResourceStateException error. If you have already registered the maximum number of directories that you can register with Amazon WorkSpaces, you will receive a ResourceLimitExceededException error. Deregister directories that you are not using for WorkSpaces, and try again.
rwdDirectoryId :: Lens' RegisterWorkspaceDirectory Text
rwdDirectoryId = lens _rwdDirectoryId (\s a -> s {_rwdDirectoryId = a})

-- | Indicates whether Amazon WorkDocs is enabled or disabled. If you have enabled this parameter and WorkDocs is not available in the Region, you will receive an OperationNotSupportedException error. Set @EnableWorkDocs@ to disabled, and try again.
rwdEnableWorkDocs :: Lens' RegisterWorkspaceDirectory Bool
rwdEnableWorkDocs = lens _rwdEnableWorkDocs (\s a -> s {_rwdEnableWorkDocs = a})

instance AWSRequest RegisterWorkspaceDirectory where
  type
    Rs RegisterWorkspaceDirectory =
      RegisterWorkspaceDirectoryResponse
  request = postJSON workSpaces
  response =
    receiveEmpty
      ( \s h x ->
          RegisterWorkspaceDirectoryResponse' <$> (pure (fromEnum s))
      )

instance Hashable RegisterWorkspaceDirectory

instance NFData RegisterWorkspaceDirectory

instance ToHeaders RegisterWorkspaceDirectory where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("WorkspacesService.RegisterWorkspaceDirectory" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON RegisterWorkspaceDirectory where
  toJSON RegisterWorkspaceDirectory' {..} =
    object
      ( catMaybes
          [ ("SubnetIds" .=) <$> _rwdSubnetIds,
            ("EnableSelfService" .=) <$> _rwdEnableSelfService,
            ("Tenancy" .=) <$> _rwdTenancy,
            ("Tags" .=) <$> _rwdTags,
            Just ("DirectoryId" .= _rwdDirectoryId),
            Just ("EnableWorkDocs" .= _rwdEnableWorkDocs)
          ]
      )

instance ToPath RegisterWorkspaceDirectory where
  toPath = const "/"

instance ToQuery RegisterWorkspaceDirectory where
  toQuery = const mempty

-- | /See:/ 'registerWorkspaceDirectoryResponse' smart constructor.
newtype RegisterWorkspaceDirectoryResponse = RegisterWorkspaceDirectoryResponse'
  { _rwdrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RegisterWorkspaceDirectoryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rwdrsResponseStatus' - -- | The response status code.
registerWorkspaceDirectoryResponse ::
  -- | 'rwdrsResponseStatus'
  Int ->
  RegisterWorkspaceDirectoryResponse
registerWorkspaceDirectoryResponse pResponseStatus_ =
  RegisterWorkspaceDirectoryResponse'
    { _rwdrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
rwdrsResponseStatus :: Lens' RegisterWorkspaceDirectoryResponse Int
rwdrsResponseStatus = lens _rwdrsResponseStatus (\s a -> s {_rwdrsResponseStatus = a})

instance NFData RegisterWorkspaceDirectoryResponse
