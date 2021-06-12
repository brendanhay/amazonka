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
-- Module      : Network.AWS.DirectoryService.CreateMicrosoftAD
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Microsoft AD directory in the AWS Cloud. For more information,
-- see
-- <https://docs.aws.amazon.com/directoryservice/latest/admin-guide/directory_microsoft_ad.html AWS Managed Microsoft AD>
-- in the /AWS Directory Service Admin Guide/.
--
-- Before you call /CreateMicrosoftAD/, ensure that all of the required
-- permissions have been explicitly granted through a policy. For details
-- about what permissions are required to run the /CreateMicrosoftAD/
-- operation, see
-- <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/UsingWithDS_IAM_ResourcePermissions.html AWS Directory Service API Permissions: Actions, Resources, and Conditions Reference>.
module Network.AWS.DirectoryService.CreateMicrosoftAD
  ( -- * Creating a Request
    CreateMicrosoftAD (..),
    newCreateMicrosoftAD,

    -- * Request Lenses
    createMicrosoftAD_shortName,
    createMicrosoftAD_edition,
    createMicrosoftAD_tags,
    createMicrosoftAD_description,
    createMicrosoftAD_name,
    createMicrosoftAD_password,
    createMicrosoftAD_vpcSettings,

    -- * Destructuring the Response
    CreateMicrosoftADResponse (..),
    newCreateMicrosoftADResponse,

    -- * Response Lenses
    createMicrosoftADResponse_directoryId,
    createMicrosoftADResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Creates an AWS Managed Microsoft AD directory.
--
-- /See:/ 'newCreateMicrosoftAD' smart constructor.
data CreateMicrosoftAD = CreateMicrosoftAD'
  { -- | The NetBIOS name for your domain, such as @CORP@. If you don\'t specify
    -- a NetBIOS name, it will default to the first part of your directory DNS.
    -- For example, @CORP@ for the directory DNS @corp.example.com@.
    shortName :: Core.Maybe Core.Text,
    -- | AWS Managed Microsoft AD is available in two editions: @Standard@ and
    -- @Enterprise@. @Enterprise@ is the default.
    edition :: Core.Maybe DirectoryEdition,
    -- | The tags to be assigned to the AWS Managed Microsoft AD directory.
    tags :: Core.Maybe [Tag],
    -- | A description for the directory. This label will appear on the AWS
    -- console @Directory Details@ page after the directory is created.
    description :: Core.Maybe Core.Text,
    -- | The fully qualified domain name for the AWS Managed Microsoft AD
    -- directory, such as @corp.example.com@. This name will resolve inside
    -- your VPC only. It does not need to be publicly resolvable.
    name :: Core.Text,
    -- | The password for the default administrative user named @Admin@.
    --
    -- If you need to change the password for the administrator account, you
    -- can use the ResetUserPassword API call.
    password :: Core.Sensitive Core.Text,
    -- | Contains VPC information for the CreateDirectory or CreateMicrosoftAD
    -- operation.
    vpcSettings :: DirectoryVpcSettings
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateMicrosoftAD' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'shortName', 'createMicrosoftAD_shortName' - The NetBIOS name for your domain, such as @CORP@. If you don\'t specify
-- a NetBIOS name, it will default to the first part of your directory DNS.
-- For example, @CORP@ for the directory DNS @corp.example.com@.
--
-- 'edition', 'createMicrosoftAD_edition' - AWS Managed Microsoft AD is available in two editions: @Standard@ and
-- @Enterprise@. @Enterprise@ is the default.
--
-- 'tags', 'createMicrosoftAD_tags' - The tags to be assigned to the AWS Managed Microsoft AD directory.
--
-- 'description', 'createMicrosoftAD_description' - A description for the directory. This label will appear on the AWS
-- console @Directory Details@ page after the directory is created.
--
-- 'name', 'createMicrosoftAD_name' - The fully qualified domain name for the AWS Managed Microsoft AD
-- directory, such as @corp.example.com@. This name will resolve inside
-- your VPC only. It does not need to be publicly resolvable.
--
-- 'password', 'createMicrosoftAD_password' - The password for the default administrative user named @Admin@.
--
-- If you need to change the password for the administrator account, you
-- can use the ResetUserPassword API call.
--
-- 'vpcSettings', 'createMicrosoftAD_vpcSettings' - Contains VPC information for the CreateDirectory or CreateMicrosoftAD
-- operation.
newCreateMicrosoftAD ::
  -- | 'name'
  Core.Text ->
  -- | 'password'
  Core.Text ->
  -- | 'vpcSettings'
  DirectoryVpcSettings ->
  CreateMicrosoftAD
newCreateMicrosoftAD pName_ pPassword_ pVpcSettings_ =
  CreateMicrosoftAD'
    { shortName = Core.Nothing,
      edition = Core.Nothing,
      tags = Core.Nothing,
      description = Core.Nothing,
      name = pName_,
      password = Core._Sensitive Lens.# pPassword_,
      vpcSettings = pVpcSettings_
    }

-- | The NetBIOS name for your domain, such as @CORP@. If you don\'t specify
-- a NetBIOS name, it will default to the first part of your directory DNS.
-- For example, @CORP@ for the directory DNS @corp.example.com@.
createMicrosoftAD_shortName :: Lens.Lens' CreateMicrosoftAD (Core.Maybe Core.Text)
createMicrosoftAD_shortName = Lens.lens (\CreateMicrosoftAD' {shortName} -> shortName) (\s@CreateMicrosoftAD' {} a -> s {shortName = a} :: CreateMicrosoftAD)

-- | AWS Managed Microsoft AD is available in two editions: @Standard@ and
-- @Enterprise@. @Enterprise@ is the default.
createMicrosoftAD_edition :: Lens.Lens' CreateMicrosoftAD (Core.Maybe DirectoryEdition)
createMicrosoftAD_edition = Lens.lens (\CreateMicrosoftAD' {edition} -> edition) (\s@CreateMicrosoftAD' {} a -> s {edition = a} :: CreateMicrosoftAD)

-- | The tags to be assigned to the AWS Managed Microsoft AD directory.
createMicrosoftAD_tags :: Lens.Lens' CreateMicrosoftAD (Core.Maybe [Tag])
createMicrosoftAD_tags = Lens.lens (\CreateMicrosoftAD' {tags} -> tags) (\s@CreateMicrosoftAD' {} a -> s {tags = a} :: CreateMicrosoftAD) Core.. Lens.mapping Lens._Coerce

-- | A description for the directory. This label will appear on the AWS
-- console @Directory Details@ page after the directory is created.
createMicrosoftAD_description :: Lens.Lens' CreateMicrosoftAD (Core.Maybe Core.Text)
createMicrosoftAD_description = Lens.lens (\CreateMicrosoftAD' {description} -> description) (\s@CreateMicrosoftAD' {} a -> s {description = a} :: CreateMicrosoftAD)

-- | The fully qualified domain name for the AWS Managed Microsoft AD
-- directory, such as @corp.example.com@. This name will resolve inside
-- your VPC only. It does not need to be publicly resolvable.
createMicrosoftAD_name :: Lens.Lens' CreateMicrosoftAD Core.Text
createMicrosoftAD_name = Lens.lens (\CreateMicrosoftAD' {name} -> name) (\s@CreateMicrosoftAD' {} a -> s {name = a} :: CreateMicrosoftAD)

-- | The password for the default administrative user named @Admin@.
--
-- If you need to change the password for the administrator account, you
-- can use the ResetUserPassword API call.
createMicrosoftAD_password :: Lens.Lens' CreateMicrosoftAD Core.Text
createMicrosoftAD_password = Lens.lens (\CreateMicrosoftAD' {password} -> password) (\s@CreateMicrosoftAD' {} a -> s {password = a} :: CreateMicrosoftAD) Core.. Core._Sensitive

-- | Contains VPC information for the CreateDirectory or CreateMicrosoftAD
-- operation.
createMicrosoftAD_vpcSettings :: Lens.Lens' CreateMicrosoftAD DirectoryVpcSettings
createMicrosoftAD_vpcSettings = Lens.lens (\CreateMicrosoftAD' {vpcSettings} -> vpcSettings) (\s@CreateMicrosoftAD' {} a -> s {vpcSettings = a} :: CreateMicrosoftAD)

instance Core.AWSRequest CreateMicrosoftAD where
  type
    AWSResponse CreateMicrosoftAD =
      CreateMicrosoftADResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMicrosoftADResponse'
            Core.<$> (x Core..?> "DirectoryId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateMicrosoftAD

instance Core.NFData CreateMicrosoftAD

instance Core.ToHeaders CreateMicrosoftAD where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.CreateMicrosoftAD" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateMicrosoftAD where
  toJSON CreateMicrosoftAD' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ShortName" Core..=) Core.<$> shortName,
            ("Edition" Core..=) Core.<$> edition,
            ("Tags" Core..=) Core.<$> tags,
            ("Description" Core..=) Core.<$> description,
            Core.Just ("Name" Core..= name),
            Core.Just ("Password" Core..= password),
            Core.Just ("VpcSettings" Core..= vpcSettings)
          ]
      )

instance Core.ToPath CreateMicrosoftAD where
  toPath = Core.const "/"

instance Core.ToQuery CreateMicrosoftAD where
  toQuery = Core.const Core.mempty

-- | Result of a CreateMicrosoftAD request.
--
-- /See:/ 'newCreateMicrosoftADResponse' smart constructor.
data CreateMicrosoftADResponse = CreateMicrosoftADResponse'
  { -- | The identifier of the directory that was created.
    directoryId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateMicrosoftADResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'createMicrosoftADResponse_directoryId' - The identifier of the directory that was created.
--
-- 'httpStatus', 'createMicrosoftADResponse_httpStatus' - The response's http status code.
newCreateMicrosoftADResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateMicrosoftADResponse
newCreateMicrosoftADResponse pHttpStatus_ =
  CreateMicrosoftADResponse'
    { directoryId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the directory that was created.
createMicrosoftADResponse_directoryId :: Lens.Lens' CreateMicrosoftADResponse (Core.Maybe Core.Text)
createMicrosoftADResponse_directoryId = Lens.lens (\CreateMicrosoftADResponse' {directoryId} -> directoryId) (\s@CreateMicrosoftADResponse' {} a -> s {directoryId = a} :: CreateMicrosoftADResponse)

-- | The response's http status code.
createMicrosoftADResponse_httpStatus :: Lens.Lens' CreateMicrosoftADResponse Core.Int
createMicrosoftADResponse_httpStatus = Lens.lens (\CreateMicrosoftADResponse' {httpStatus} -> httpStatus) (\s@CreateMicrosoftADResponse' {} a -> s {httpStatus = a} :: CreateMicrosoftADResponse)

instance Core.NFData CreateMicrosoftADResponse
