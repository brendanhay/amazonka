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
-- Module      : Amazonka.AppStream.CreateApplication
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an application.
--
-- Applications are an Amazon AppStream 2.0 resource that stores the
-- details about how to launch applications on Elastic fleet streaming
-- instances. An application consists of the launch details, icon, and
-- display name. Applications are associated with an app block that
-- contains the application binaries and other files. The applications
-- assigned to an Elastic fleet are the applications users can launch.
--
-- This is only supported for Elastic fleets.
module Amazonka.AppStream.CreateApplication
  ( -- * Creating a Request
    CreateApplication (..),
    newCreateApplication,

    -- * Request Lenses
    createApplication_description,
    createApplication_displayName,
    createApplication_launchParameters,
    createApplication_tags,
    createApplication_workingDirectory,
    createApplication_name,
    createApplication_iconS3Location,
    createApplication_launchPath,
    createApplication_platforms,
    createApplication_instanceFamilies,
    createApplication_appBlockArn,

    -- * Destructuring the Response
    CreateApplicationResponse (..),
    newCreateApplicationResponse,

    -- * Response Lenses
    createApplicationResponse_application,
    createApplicationResponse_httpStatus,
  )
where

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateApplication' smart constructor.
data CreateApplication = CreateApplication'
  { -- | The description of the application.
    description :: Prelude.Maybe Prelude.Text,
    -- | The display name of the application. This name is visible to users in
    -- the application catalog.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The launch parameters of the application.
    launchParameters :: Prelude.Maybe Prelude.Text,
    -- | The tags assigned to the application.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The working directory of the application.
    workingDirectory :: Prelude.Maybe Prelude.Text,
    -- | The name of the application. This name is visible to users when display
    -- name is not specified.
    name :: Prelude.Text,
    -- | The location in S3 of the application icon.
    iconS3Location :: S3Location,
    -- | The launch path of the application.
    launchPath :: Prelude.Text,
    -- | The platforms the application supports. WINDOWS_SERVER_2019 and
    -- AMAZON_LINUX2 are supported for Elastic fleets.
    platforms :: [PlatformType],
    -- | The instance families the application supports. Valid values are
    -- GENERAL_PURPOSE and GRAPHICS_G4.
    instanceFamilies :: [Prelude.Text],
    -- | The app block ARN to which the application should be associated
    appBlockArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createApplication_description' - The description of the application.
--
-- 'displayName', 'createApplication_displayName' - The display name of the application. This name is visible to users in
-- the application catalog.
--
-- 'launchParameters', 'createApplication_launchParameters' - The launch parameters of the application.
--
-- 'tags', 'createApplication_tags' - The tags assigned to the application.
--
-- 'workingDirectory', 'createApplication_workingDirectory' - The working directory of the application.
--
-- 'name', 'createApplication_name' - The name of the application. This name is visible to users when display
-- name is not specified.
--
-- 'iconS3Location', 'createApplication_iconS3Location' - The location in S3 of the application icon.
--
-- 'launchPath', 'createApplication_launchPath' - The launch path of the application.
--
-- 'platforms', 'createApplication_platforms' - The platforms the application supports. WINDOWS_SERVER_2019 and
-- AMAZON_LINUX2 are supported for Elastic fleets.
--
-- 'instanceFamilies', 'createApplication_instanceFamilies' - The instance families the application supports. Valid values are
-- GENERAL_PURPOSE and GRAPHICS_G4.
--
-- 'appBlockArn', 'createApplication_appBlockArn' - The app block ARN to which the application should be associated
newCreateApplication ::
  -- | 'name'
  Prelude.Text ->
  -- | 'iconS3Location'
  S3Location ->
  -- | 'launchPath'
  Prelude.Text ->
  -- | 'appBlockArn'
  Prelude.Text ->
  CreateApplication
newCreateApplication
  pName_
  pIconS3Location_
  pLaunchPath_
  pAppBlockArn_ =
    CreateApplication'
      { description = Prelude.Nothing,
        displayName = Prelude.Nothing,
        launchParameters = Prelude.Nothing,
        tags = Prelude.Nothing,
        workingDirectory = Prelude.Nothing,
        name = pName_,
        iconS3Location = pIconS3Location_,
        launchPath = pLaunchPath_,
        platforms = Prelude.mempty,
        instanceFamilies = Prelude.mempty,
        appBlockArn = pAppBlockArn_
      }

-- | The description of the application.
createApplication_description :: Lens.Lens' CreateApplication (Prelude.Maybe Prelude.Text)
createApplication_description = Lens.lens (\CreateApplication' {description} -> description) (\s@CreateApplication' {} a -> s {description = a} :: CreateApplication)

-- | The display name of the application. This name is visible to users in
-- the application catalog.
createApplication_displayName :: Lens.Lens' CreateApplication (Prelude.Maybe Prelude.Text)
createApplication_displayName = Lens.lens (\CreateApplication' {displayName} -> displayName) (\s@CreateApplication' {} a -> s {displayName = a} :: CreateApplication)

-- | The launch parameters of the application.
createApplication_launchParameters :: Lens.Lens' CreateApplication (Prelude.Maybe Prelude.Text)
createApplication_launchParameters = Lens.lens (\CreateApplication' {launchParameters} -> launchParameters) (\s@CreateApplication' {} a -> s {launchParameters = a} :: CreateApplication)

-- | The tags assigned to the application.
createApplication_tags :: Lens.Lens' CreateApplication (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createApplication_tags = Lens.lens (\CreateApplication' {tags} -> tags) (\s@CreateApplication' {} a -> s {tags = a} :: CreateApplication) Prelude.. Lens.mapping Lens.coerced

-- | The working directory of the application.
createApplication_workingDirectory :: Lens.Lens' CreateApplication (Prelude.Maybe Prelude.Text)
createApplication_workingDirectory = Lens.lens (\CreateApplication' {workingDirectory} -> workingDirectory) (\s@CreateApplication' {} a -> s {workingDirectory = a} :: CreateApplication)

-- | The name of the application. This name is visible to users when display
-- name is not specified.
createApplication_name :: Lens.Lens' CreateApplication Prelude.Text
createApplication_name = Lens.lens (\CreateApplication' {name} -> name) (\s@CreateApplication' {} a -> s {name = a} :: CreateApplication)

-- | The location in S3 of the application icon.
createApplication_iconS3Location :: Lens.Lens' CreateApplication S3Location
createApplication_iconS3Location = Lens.lens (\CreateApplication' {iconS3Location} -> iconS3Location) (\s@CreateApplication' {} a -> s {iconS3Location = a} :: CreateApplication)

-- | The launch path of the application.
createApplication_launchPath :: Lens.Lens' CreateApplication Prelude.Text
createApplication_launchPath = Lens.lens (\CreateApplication' {launchPath} -> launchPath) (\s@CreateApplication' {} a -> s {launchPath = a} :: CreateApplication)

-- | The platforms the application supports. WINDOWS_SERVER_2019 and
-- AMAZON_LINUX2 are supported for Elastic fleets.
createApplication_platforms :: Lens.Lens' CreateApplication [PlatformType]
createApplication_platforms = Lens.lens (\CreateApplication' {platforms} -> platforms) (\s@CreateApplication' {} a -> s {platforms = a} :: CreateApplication) Prelude.. Lens.coerced

-- | The instance families the application supports. Valid values are
-- GENERAL_PURPOSE and GRAPHICS_G4.
createApplication_instanceFamilies :: Lens.Lens' CreateApplication [Prelude.Text]
createApplication_instanceFamilies = Lens.lens (\CreateApplication' {instanceFamilies} -> instanceFamilies) (\s@CreateApplication' {} a -> s {instanceFamilies = a} :: CreateApplication) Prelude.. Lens.coerced

-- | The app block ARN to which the application should be associated
createApplication_appBlockArn :: Lens.Lens' CreateApplication Prelude.Text
createApplication_appBlockArn = Lens.lens (\CreateApplication' {appBlockArn} -> appBlockArn) (\s@CreateApplication' {} a -> s {appBlockArn = a} :: CreateApplication)

instance Core.AWSRequest CreateApplication where
  type
    AWSResponse CreateApplication =
      CreateApplicationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateApplicationResponse'
            Prelude.<$> (x Data..?> "Application")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateApplication where
  hashWithSalt _salt CreateApplication' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` launchParameters
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` workingDirectory
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` iconS3Location
      `Prelude.hashWithSalt` launchPath
      `Prelude.hashWithSalt` platforms
      `Prelude.hashWithSalt` instanceFamilies
      `Prelude.hashWithSalt` appBlockArn

instance Prelude.NFData CreateApplication where
  rnf CreateApplication' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf launchParameters
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf workingDirectory
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf iconS3Location
      `Prelude.seq` Prelude.rnf launchPath
      `Prelude.seq` Prelude.rnf platforms
      `Prelude.seq` Prelude.rnf instanceFamilies
      `Prelude.seq` Prelude.rnf appBlockArn

instance Data.ToHeaders CreateApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PhotonAdminProxyService.CreateApplication" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateApplication where
  toJSON CreateApplication' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("DisplayName" Data..=) Prelude.<$> displayName,
            ("LaunchParameters" Data..=)
              Prelude.<$> launchParameters,
            ("Tags" Data..=) Prelude.<$> tags,
            ("WorkingDirectory" Data..=)
              Prelude.<$> workingDirectory,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("IconS3Location" Data..= iconS3Location),
            Prelude.Just ("LaunchPath" Data..= launchPath),
            Prelude.Just ("Platforms" Data..= platforms),
            Prelude.Just
              ("InstanceFamilies" Data..= instanceFamilies),
            Prelude.Just ("AppBlockArn" Data..= appBlockArn)
          ]
      )

instance Data.ToPath CreateApplication where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateApplicationResponse' smart constructor.
data CreateApplicationResponse = CreateApplicationResponse'
  { application :: Prelude.Maybe Application,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'application', 'createApplicationResponse_application' - Undocumented member.
--
-- 'httpStatus', 'createApplicationResponse_httpStatus' - The response's http status code.
newCreateApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateApplicationResponse
newCreateApplicationResponse pHttpStatus_ =
  CreateApplicationResponse'
    { application =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createApplicationResponse_application :: Lens.Lens' CreateApplicationResponse (Prelude.Maybe Application)
createApplicationResponse_application = Lens.lens (\CreateApplicationResponse' {application} -> application) (\s@CreateApplicationResponse' {} a -> s {application = a} :: CreateApplicationResponse)

-- | The response's http status code.
createApplicationResponse_httpStatus :: Lens.Lens' CreateApplicationResponse Prelude.Int
createApplicationResponse_httpStatus = Lens.lens (\CreateApplicationResponse' {httpStatus} -> httpStatus) (\s@CreateApplicationResponse' {} a -> s {httpStatus = a} :: CreateApplicationResponse)

instance Prelude.NFData CreateApplicationResponse where
  rnf CreateApplicationResponse' {..} =
    Prelude.rnf application
      `Prelude.seq` Prelude.rnf httpStatus
