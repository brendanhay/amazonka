{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
  ( -- * Creating a request
    RegisterWorkspaceDirectory (..),
    mkRegisterWorkspaceDirectory,

    -- ** Request lenses
    rwdDirectoryId,
    rwdSubnetIds,
    rwdEnableSelfService,
    rwdEnableWorkDocs,
    rwdTenancy,
    rwdTags,

    -- * Destructuring the response
    RegisterWorkspaceDirectoryResponse (..),
    mkRegisterWorkspaceDirectoryResponse,

    -- ** Response lenses
    rwdrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkRegisterWorkspaceDirectory' smart constructor.
data RegisterWorkspaceDirectory = RegisterWorkspaceDirectory'
  { -- | The identifier of the directory. You cannot register a directory if it does not have a status of Active. If the directory does not have a status of Active, you will receive an InvalidResourceStateException error. If you have already registered the maximum number of directories that you can register with Amazon WorkSpaces, you will receive a ResourceLimitExceededException error. Deregister directories that you are not using for WorkSpaces, and try again.
    directoryId :: Lude.Text,
    -- | The identifiers of the subnets for your virtual private cloud (VPC). Make sure that the subnets are in supported Availability Zones. The subnets must also be in separate Availability Zones. If these conditions are not met, you will receive an OperationNotSupportedException error.
    subnetIds :: Lude.Maybe [Lude.Text],
    -- | Indicates whether self-service capabilities are enabled or disabled.
    enableSelfService :: Lude.Maybe Lude.Bool,
    -- | Indicates whether Amazon WorkDocs is enabled or disabled. If you have enabled this parameter and WorkDocs is not available in the Region, you will receive an OperationNotSupportedException error. Set @EnableWorkDocs@ to disabled, and try again.
    enableWorkDocs :: Lude.Bool,
    -- | Indicates whether your WorkSpace directory is dedicated or shared. To use Bring Your Own License (BYOL) images, this value must be set to @DEDICATED@ and your AWS account must be enabled for BYOL. If your account has not been enabled for BYOL, you will receive an InvalidParameterValuesException error. For more information about BYOL images, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Images> .
    tenancy :: Lude.Maybe Tenancy,
    -- | The tags associated with the directory.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterWorkspaceDirectory' with the minimum fields required to make a request.
--
-- * 'directoryId' - The identifier of the directory. You cannot register a directory if it does not have a status of Active. If the directory does not have a status of Active, you will receive an InvalidResourceStateException error. If you have already registered the maximum number of directories that you can register with Amazon WorkSpaces, you will receive a ResourceLimitExceededException error. Deregister directories that you are not using for WorkSpaces, and try again.
-- * 'subnetIds' - The identifiers of the subnets for your virtual private cloud (VPC). Make sure that the subnets are in supported Availability Zones. The subnets must also be in separate Availability Zones. If these conditions are not met, you will receive an OperationNotSupportedException error.
-- * 'enableSelfService' - Indicates whether self-service capabilities are enabled or disabled.
-- * 'enableWorkDocs' - Indicates whether Amazon WorkDocs is enabled or disabled. If you have enabled this parameter and WorkDocs is not available in the Region, you will receive an OperationNotSupportedException error. Set @EnableWorkDocs@ to disabled, and try again.
-- * 'tenancy' - Indicates whether your WorkSpace directory is dedicated or shared. To use Bring Your Own License (BYOL) images, this value must be set to @DEDICATED@ and your AWS account must be enabled for BYOL. If your account has not been enabled for BYOL, you will receive an InvalidParameterValuesException error. For more information about BYOL images, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Images> .
-- * 'tags' - The tags associated with the directory.
mkRegisterWorkspaceDirectory ::
  -- | 'directoryId'
  Lude.Text ->
  -- | 'enableWorkDocs'
  Lude.Bool ->
  RegisterWorkspaceDirectory
mkRegisterWorkspaceDirectory pDirectoryId_ pEnableWorkDocs_ =
  RegisterWorkspaceDirectory'
    { directoryId = pDirectoryId_,
      subnetIds = Lude.Nothing,
      enableSelfService = Lude.Nothing,
      enableWorkDocs = pEnableWorkDocs_,
      tenancy = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The identifier of the directory. You cannot register a directory if it does not have a status of Active. If the directory does not have a status of Active, you will receive an InvalidResourceStateException error. If you have already registered the maximum number of directories that you can register with Amazon WorkSpaces, you will receive a ResourceLimitExceededException error. Deregister directories that you are not using for WorkSpaces, and try again.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwdDirectoryId :: Lens.Lens' RegisterWorkspaceDirectory Lude.Text
rwdDirectoryId = Lens.lens (directoryId :: RegisterWorkspaceDirectory -> Lude.Text) (\s a -> s {directoryId = a} :: RegisterWorkspaceDirectory)
{-# DEPRECATED rwdDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The identifiers of the subnets for your virtual private cloud (VPC). Make sure that the subnets are in supported Availability Zones. The subnets must also be in separate Availability Zones. If these conditions are not met, you will receive an OperationNotSupportedException error.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwdSubnetIds :: Lens.Lens' RegisterWorkspaceDirectory (Lude.Maybe [Lude.Text])
rwdSubnetIds = Lens.lens (subnetIds :: RegisterWorkspaceDirectory -> Lude.Maybe [Lude.Text]) (\s a -> s {subnetIds = a} :: RegisterWorkspaceDirectory)
{-# DEPRECATED rwdSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | Indicates whether self-service capabilities are enabled or disabled.
--
-- /Note:/ Consider using 'enableSelfService' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwdEnableSelfService :: Lens.Lens' RegisterWorkspaceDirectory (Lude.Maybe Lude.Bool)
rwdEnableSelfService = Lens.lens (enableSelfService :: RegisterWorkspaceDirectory -> Lude.Maybe Lude.Bool) (\s a -> s {enableSelfService = a} :: RegisterWorkspaceDirectory)
{-# DEPRECATED rwdEnableSelfService "Use generic-lens or generic-optics with 'enableSelfService' instead." #-}

-- | Indicates whether Amazon WorkDocs is enabled or disabled. If you have enabled this parameter and WorkDocs is not available in the Region, you will receive an OperationNotSupportedException error. Set @EnableWorkDocs@ to disabled, and try again.
--
-- /Note:/ Consider using 'enableWorkDocs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwdEnableWorkDocs :: Lens.Lens' RegisterWorkspaceDirectory Lude.Bool
rwdEnableWorkDocs = Lens.lens (enableWorkDocs :: RegisterWorkspaceDirectory -> Lude.Bool) (\s a -> s {enableWorkDocs = a} :: RegisterWorkspaceDirectory)
{-# DEPRECATED rwdEnableWorkDocs "Use generic-lens or generic-optics with 'enableWorkDocs' instead." #-}

-- | Indicates whether your WorkSpace directory is dedicated or shared. To use Bring Your Own License (BYOL) images, this value must be set to @DEDICATED@ and your AWS account must be enabled for BYOL. If your account has not been enabled for BYOL, you will receive an InvalidParameterValuesException error. For more information about BYOL images, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Images> .
--
-- /Note:/ Consider using 'tenancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwdTenancy :: Lens.Lens' RegisterWorkspaceDirectory (Lude.Maybe Tenancy)
rwdTenancy = Lens.lens (tenancy :: RegisterWorkspaceDirectory -> Lude.Maybe Tenancy) (\s a -> s {tenancy = a} :: RegisterWorkspaceDirectory)
{-# DEPRECATED rwdTenancy "Use generic-lens or generic-optics with 'tenancy' instead." #-}

-- | The tags associated with the directory.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwdTags :: Lens.Lens' RegisterWorkspaceDirectory (Lude.Maybe [Tag])
rwdTags = Lens.lens (tags :: RegisterWorkspaceDirectory -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: RegisterWorkspaceDirectory)
{-# DEPRECATED rwdTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest RegisterWorkspaceDirectory where
  type
    Rs RegisterWorkspaceDirectory =
      RegisterWorkspaceDirectoryResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveEmpty
      ( \s h x ->
          RegisterWorkspaceDirectoryResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RegisterWorkspaceDirectory where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "WorkspacesService.RegisterWorkspaceDirectory" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RegisterWorkspaceDirectory where
  toJSON RegisterWorkspaceDirectory' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DirectoryId" Lude..= directoryId),
            ("SubnetIds" Lude..=) Lude.<$> subnetIds,
            ("EnableSelfService" Lude..=) Lude.<$> enableSelfService,
            Lude.Just ("EnableWorkDocs" Lude..= enableWorkDocs),
            ("Tenancy" Lude..=) Lude.<$> tenancy,
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath RegisterWorkspaceDirectory where
  toPath = Lude.const "/"

instance Lude.ToQuery RegisterWorkspaceDirectory where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRegisterWorkspaceDirectoryResponse' smart constructor.
newtype RegisterWorkspaceDirectoryResponse = RegisterWorkspaceDirectoryResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterWorkspaceDirectoryResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkRegisterWorkspaceDirectoryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RegisterWorkspaceDirectoryResponse
mkRegisterWorkspaceDirectoryResponse pResponseStatus_ =
  RegisterWorkspaceDirectoryResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwdrsResponseStatus :: Lens.Lens' RegisterWorkspaceDirectoryResponse Lude.Int
rwdrsResponseStatus = Lens.lens (responseStatus :: RegisterWorkspaceDirectoryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RegisterWorkspaceDirectoryResponse)
{-# DEPRECATED rwdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
