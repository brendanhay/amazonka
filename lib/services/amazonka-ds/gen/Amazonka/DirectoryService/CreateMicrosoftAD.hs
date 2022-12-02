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
-- Module      : Amazonka.DirectoryService.CreateMicrosoftAD
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Microsoft AD directory in the Amazon Web Services Cloud. For
-- more information, see
-- <https://docs.aws.amazon.com/directoryservice/latest/admin-guide/directory_microsoft_ad.html Managed Microsoft AD>
-- in the /Directory Service Admin Guide/.
--
-- Before you call /CreateMicrosoftAD/, ensure that all of the required
-- permissions have been explicitly granted through a policy. For details
-- about what permissions are required to run the /CreateMicrosoftAD/
-- operation, see
-- <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/UsingWithDS_IAM_ResourcePermissions.html Directory Service API Permissions: Actions, Resources, and Conditions Reference>.
module Amazonka.DirectoryService.CreateMicrosoftAD
  ( -- * Creating a Request
    CreateMicrosoftAD (..),
    newCreateMicrosoftAD,

    -- * Request Lenses
    createMicrosoftAD_tags,
    createMicrosoftAD_edition,
    createMicrosoftAD_description,
    createMicrosoftAD_shortName,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Creates an Managed Microsoft AD directory.
--
-- /See:/ 'newCreateMicrosoftAD' smart constructor.
data CreateMicrosoftAD = CreateMicrosoftAD'
  { -- | The tags to be assigned to the Managed Microsoft AD directory.
    tags :: Prelude.Maybe [Tag],
    -- | Managed Microsoft AD is available in two editions: @Standard@ and
    -- @Enterprise@. @Enterprise@ is the default.
    edition :: Prelude.Maybe DirectoryEdition,
    -- | A description for the directory. This label will appear on the Amazon
    -- Web Services console @Directory Details@ page after the directory is
    -- created.
    description :: Prelude.Maybe Prelude.Text,
    -- | The NetBIOS name for your domain, such as @CORP@. If you don\'t specify
    -- a NetBIOS name, it will default to the first part of your directory DNS.
    -- For example, @CORP@ for the directory DNS @corp.example.com@.
    shortName :: Prelude.Maybe Prelude.Text,
    -- | The fully qualified domain name for the Managed Microsoft AD directory,
    -- such as @corp.example.com@. This name will resolve inside your VPC only.
    -- It does not need to be publicly resolvable.
    name :: Prelude.Text,
    -- | The password for the default administrative user named @Admin@.
    --
    -- If you need to change the password for the administrator account, you
    -- can use the ResetUserPassword API call.
    password :: Data.Sensitive Prelude.Text,
    -- | Contains VPC information for the CreateDirectory or CreateMicrosoftAD
    -- operation.
    vpcSettings :: DirectoryVpcSettings
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMicrosoftAD' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createMicrosoftAD_tags' - The tags to be assigned to the Managed Microsoft AD directory.
--
-- 'edition', 'createMicrosoftAD_edition' - Managed Microsoft AD is available in two editions: @Standard@ and
-- @Enterprise@. @Enterprise@ is the default.
--
-- 'description', 'createMicrosoftAD_description' - A description for the directory. This label will appear on the Amazon
-- Web Services console @Directory Details@ page after the directory is
-- created.
--
-- 'shortName', 'createMicrosoftAD_shortName' - The NetBIOS name for your domain, such as @CORP@. If you don\'t specify
-- a NetBIOS name, it will default to the first part of your directory DNS.
-- For example, @CORP@ for the directory DNS @corp.example.com@.
--
-- 'name', 'createMicrosoftAD_name' - The fully qualified domain name for the Managed Microsoft AD directory,
-- such as @corp.example.com@. This name will resolve inside your VPC only.
-- It does not need to be publicly resolvable.
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
  Prelude.Text ->
  -- | 'password'
  Prelude.Text ->
  -- | 'vpcSettings'
  DirectoryVpcSettings ->
  CreateMicrosoftAD
newCreateMicrosoftAD pName_ pPassword_ pVpcSettings_ =
  CreateMicrosoftAD'
    { tags = Prelude.Nothing,
      edition = Prelude.Nothing,
      description = Prelude.Nothing,
      shortName = Prelude.Nothing,
      name = pName_,
      password = Data._Sensitive Lens.# pPassword_,
      vpcSettings = pVpcSettings_
    }

-- | The tags to be assigned to the Managed Microsoft AD directory.
createMicrosoftAD_tags :: Lens.Lens' CreateMicrosoftAD (Prelude.Maybe [Tag])
createMicrosoftAD_tags = Lens.lens (\CreateMicrosoftAD' {tags} -> tags) (\s@CreateMicrosoftAD' {} a -> s {tags = a} :: CreateMicrosoftAD) Prelude.. Lens.mapping Lens.coerced

-- | Managed Microsoft AD is available in two editions: @Standard@ and
-- @Enterprise@. @Enterprise@ is the default.
createMicrosoftAD_edition :: Lens.Lens' CreateMicrosoftAD (Prelude.Maybe DirectoryEdition)
createMicrosoftAD_edition = Lens.lens (\CreateMicrosoftAD' {edition} -> edition) (\s@CreateMicrosoftAD' {} a -> s {edition = a} :: CreateMicrosoftAD)

-- | A description for the directory. This label will appear on the Amazon
-- Web Services console @Directory Details@ page after the directory is
-- created.
createMicrosoftAD_description :: Lens.Lens' CreateMicrosoftAD (Prelude.Maybe Prelude.Text)
createMicrosoftAD_description = Lens.lens (\CreateMicrosoftAD' {description} -> description) (\s@CreateMicrosoftAD' {} a -> s {description = a} :: CreateMicrosoftAD)

-- | The NetBIOS name for your domain, such as @CORP@. If you don\'t specify
-- a NetBIOS name, it will default to the first part of your directory DNS.
-- For example, @CORP@ for the directory DNS @corp.example.com@.
createMicrosoftAD_shortName :: Lens.Lens' CreateMicrosoftAD (Prelude.Maybe Prelude.Text)
createMicrosoftAD_shortName = Lens.lens (\CreateMicrosoftAD' {shortName} -> shortName) (\s@CreateMicrosoftAD' {} a -> s {shortName = a} :: CreateMicrosoftAD)

-- | The fully qualified domain name for the Managed Microsoft AD directory,
-- such as @corp.example.com@. This name will resolve inside your VPC only.
-- It does not need to be publicly resolvable.
createMicrosoftAD_name :: Lens.Lens' CreateMicrosoftAD Prelude.Text
createMicrosoftAD_name = Lens.lens (\CreateMicrosoftAD' {name} -> name) (\s@CreateMicrosoftAD' {} a -> s {name = a} :: CreateMicrosoftAD)

-- | The password for the default administrative user named @Admin@.
--
-- If you need to change the password for the administrator account, you
-- can use the ResetUserPassword API call.
createMicrosoftAD_password :: Lens.Lens' CreateMicrosoftAD Prelude.Text
createMicrosoftAD_password = Lens.lens (\CreateMicrosoftAD' {password} -> password) (\s@CreateMicrosoftAD' {} a -> s {password = a} :: CreateMicrosoftAD) Prelude.. Data._Sensitive

-- | Contains VPC information for the CreateDirectory or CreateMicrosoftAD
-- operation.
createMicrosoftAD_vpcSettings :: Lens.Lens' CreateMicrosoftAD DirectoryVpcSettings
createMicrosoftAD_vpcSettings = Lens.lens (\CreateMicrosoftAD' {vpcSettings} -> vpcSettings) (\s@CreateMicrosoftAD' {} a -> s {vpcSettings = a} :: CreateMicrosoftAD)

instance Core.AWSRequest CreateMicrosoftAD where
  type
    AWSResponse CreateMicrosoftAD =
      CreateMicrosoftADResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMicrosoftADResponse'
            Prelude.<$> (x Data..?> "DirectoryId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateMicrosoftAD where
  hashWithSalt _salt CreateMicrosoftAD' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` edition
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` shortName
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` password
      `Prelude.hashWithSalt` vpcSettings

instance Prelude.NFData CreateMicrosoftAD where
  rnf CreateMicrosoftAD' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf edition
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf shortName
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf password
      `Prelude.seq` Prelude.rnf vpcSettings

instance Data.ToHeaders CreateMicrosoftAD where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DirectoryService_20150416.CreateMicrosoftAD" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateMicrosoftAD where
  toJSON CreateMicrosoftAD' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("Edition" Data..=) Prelude.<$> edition,
            ("Description" Data..=) Prelude.<$> description,
            ("ShortName" Data..=) Prelude.<$> shortName,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Password" Data..= password),
            Prelude.Just ("VpcSettings" Data..= vpcSettings)
          ]
      )

instance Data.ToPath CreateMicrosoftAD where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateMicrosoftAD where
  toQuery = Prelude.const Prelude.mempty

-- | Result of a CreateMicrosoftAD request.
--
-- /See:/ 'newCreateMicrosoftADResponse' smart constructor.
data CreateMicrosoftADResponse = CreateMicrosoftADResponse'
  { -- | The identifier of the directory that was created.
    directoryId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateMicrosoftADResponse
newCreateMicrosoftADResponse pHttpStatus_ =
  CreateMicrosoftADResponse'
    { directoryId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the directory that was created.
createMicrosoftADResponse_directoryId :: Lens.Lens' CreateMicrosoftADResponse (Prelude.Maybe Prelude.Text)
createMicrosoftADResponse_directoryId = Lens.lens (\CreateMicrosoftADResponse' {directoryId} -> directoryId) (\s@CreateMicrosoftADResponse' {} a -> s {directoryId = a} :: CreateMicrosoftADResponse)

-- | The response's http status code.
createMicrosoftADResponse_httpStatus :: Lens.Lens' CreateMicrosoftADResponse Prelude.Int
createMicrosoftADResponse_httpStatus = Lens.lens (\CreateMicrosoftADResponse' {httpStatus} -> httpStatus) (\s@CreateMicrosoftADResponse' {} a -> s {httpStatus = a} :: CreateMicrosoftADResponse)

instance Prelude.NFData CreateMicrosoftADResponse where
  rnf CreateMicrosoftADResponse' {..} =
    Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf httpStatus
