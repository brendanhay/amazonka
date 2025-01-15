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
-- Module      : Amazonka.QuickSight.CreateTheme
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a theme.
--
-- A /theme/ is set of configuration options for color and layout. Themes
-- apply to analyses and dashboards. For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/themes-in-quicksight.html Using Themes in Amazon QuickSight>
-- in the /Amazon QuickSight User Guide/.
module Amazonka.QuickSight.CreateTheme
  ( -- * Creating a Request
    CreateTheme (..),
    newCreateTheme,

    -- * Request Lenses
    createTheme_permissions,
    createTheme_tags,
    createTheme_versionDescription,
    createTheme_awsAccountId,
    createTheme_themeId,
    createTheme_name,
    createTheme_baseThemeId,
    createTheme_configuration,

    -- * Destructuring the Response
    CreateThemeResponse (..),
    newCreateThemeResponse,

    -- * Response Lenses
    createThemeResponse_arn,
    createThemeResponse_creationStatus,
    createThemeResponse_requestId,
    createThemeResponse_themeId,
    createThemeResponse_versionArn,
    createThemeResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateTheme' smart constructor.
data CreateTheme = CreateTheme'
  { -- | A valid grouping of resource permissions to apply to the new theme.
    permissions :: Prelude.Maybe (Prelude.NonEmpty ResourcePermission),
    -- | A map of the key-value pairs for the resource tag or tags that you want
    -- to add to the resource.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | A description of the first version of the theme that you\'re creating.
    -- Every time @UpdateTheme@ is called, a new version is created. Each
    -- version of the theme has a description of the version in the
    -- @VersionDescription@ field.
    versionDescription :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account where you want to store the
    -- new theme.
    awsAccountId :: Prelude.Text,
    -- | An ID for the theme that you want to create. The theme ID is unique per
    -- Amazon Web Services Region in each Amazon Web Services account.
    themeId :: Prelude.Text,
    -- | A display name for the theme.
    name :: Prelude.Text,
    -- | The ID of the theme that a custom theme will inherit from. All themes
    -- inherit from one of the starting themes defined by Amazon QuickSight.
    -- For a list of the starting themes, use @ListThemes@ or choose __Themes__
    -- from within an analysis.
    baseThemeId :: Prelude.Text,
    -- | The theme configuration, which contains the theme display properties.
    configuration :: ThemeConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTheme' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'permissions', 'createTheme_permissions' - A valid grouping of resource permissions to apply to the new theme.
--
-- 'tags', 'createTheme_tags' - A map of the key-value pairs for the resource tag or tags that you want
-- to add to the resource.
--
-- 'versionDescription', 'createTheme_versionDescription' - A description of the first version of the theme that you\'re creating.
-- Every time @UpdateTheme@ is called, a new version is created. Each
-- version of the theme has a description of the version in the
-- @VersionDescription@ field.
--
-- 'awsAccountId', 'createTheme_awsAccountId' - The ID of the Amazon Web Services account where you want to store the
-- new theme.
--
-- 'themeId', 'createTheme_themeId' - An ID for the theme that you want to create. The theme ID is unique per
-- Amazon Web Services Region in each Amazon Web Services account.
--
-- 'name', 'createTheme_name' - A display name for the theme.
--
-- 'baseThemeId', 'createTheme_baseThemeId' - The ID of the theme that a custom theme will inherit from. All themes
-- inherit from one of the starting themes defined by Amazon QuickSight.
-- For a list of the starting themes, use @ListThemes@ or choose __Themes__
-- from within an analysis.
--
-- 'configuration', 'createTheme_configuration' - The theme configuration, which contains the theme display properties.
newCreateTheme ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'themeId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'baseThemeId'
  Prelude.Text ->
  -- | 'configuration'
  ThemeConfiguration ->
  CreateTheme
newCreateTheme
  pAwsAccountId_
  pThemeId_
  pName_
  pBaseThemeId_
  pConfiguration_ =
    CreateTheme'
      { permissions = Prelude.Nothing,
        tags = Prelude.Nothing,
        versionDescription = Prelude.Nothing,
        awsAccountId = pAwsAccountId_,
        themeId = pThemeId_,
        name = pName_,
        baseThemeId = pBaseThemeId_,
        configuration = pConfiguration_
      }

-- | A valid grouping of resource permissions to apply to the new theme.
createTheme_permissions :: Lens.Lens' CreateTheme (Prelude.Maybe (Prelude.NonEmpty ResourcePermission))
createTheme_permissions = Lens.lens (\CreateTheme' {permissions} -> permissions) (\s@CreateTheme' {} a -> s {permissions = a} :: CreateTheme) Prelude.. Lens.mapping Lens.coerced

-- | A map of the key-value pairs for the resource tag or tags that you want
-- to add to the resource.
createTheme_tags :: Lens.Lens' CreateTheme (Prelude.Maybe (Prelude.NonEmpty Tag))
createTheme_tags = Lens.lens (\CreateTheme' {tags} -> tags) (\s@CreateTheme' {} a -> s {tags = a} :: CreateTheme) Prelude.. Lens.mapping Lens.coerced

-- | A description of the first version of the theme that you\'re creating.
-- Every time @UpdateTheme@ is called, a new version is created. Each
-- version of the theme has a description of the version in the
-- @VersionDescription@ field.
createTheme_versionDescription :: Lens.Lens' CreateTheme (Prelude.Maybe Prelude.Text)
createTheme_versionDescription = Lens.lens (\CreateTheme' {versionDescription} -> versionDescription) (\s@CreateTheme' {} a -> s {versionDescription = a} :: CreateTheme)

-- | The ID of the Amazon Web Services account where you want to store the
-- new theme.
createTheme_awsAccountId :: Lens.Lens' CreateTheme Prelude.Text
createTheme_awsAccountId = Lens.lens (\CreateTheme' {awsAccountId} -> awsAccountId) (\s@CreateTheme' {} a -> s {awsAccountId = a} :: CreateTheme)

-- | An ID for the theme that you want to create. The theme ID is unique per
-- Amazon Web Services Region in each Amazon Web Services account.
createTheme_themeId :: Lens.Lens' CreateTheme Prelude.Text
createTheme_themeId = Lens.lens (\CreateTheme' {themeId} -> themeId) (\s@CreateTheme' {} a -> s {themeId = a} :: CreateTheme)

-- | A display name for the theme.
createTheme_name :: Lens.Lens' CreateTheme Prelude.Text
createTheme_name = Lens.lens (\CreateTheme' {name} -> name) (\s@CreateTheme' {} a -> s {name = a} :: CreateTheme)

-- | The ID of the theme that a custom theme will inherit from. All themes
-- inherit from one of the starting themes defined by Amazon QuickSight.
-- For a list of the starting themes, use @ListThemes@ or choose __Themes__
-- from within an analysis.
createTheme_baseThemeId :: Lens.Lens' CreateTheme Prelude.Text
createTheme_baseThemeId = Lens.lens (\CreateTheme' {baseThemeId} -> baseThemeId) (\s@CreateTheme' {} a -> s {baseThemeId = a} :: CreateTheme)

-- | The theme configuration, which contains the theme display properties.
createTheme_configuration :: Lens.Lens' CreateTheme ThemeConfiguration
createTheme_configuration = Lens.lens (\CreateTheme' {configuration} -> configuration) (\s@CreateTheme' {} a -> s {configuration = a} :: CreateTheme)

instance Core.AWSRequest CreateTheme where
  type AWSResponse CreateTheme = CreateThemeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateThemeResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CreationStatus")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "ThemeId")
            Prelude.<*> (x Data..?> "VersionArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateTheme where
  hashWithSalt _salt CreateTheme' {..} =
    _salt
      `Prelude.hashWithSalt` permissions
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` versionDescription
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` themeId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` baseThemeId
      `Prelude.hashWithSalt` configuration

instance Prelude.NFData CreateTheme where
  rnf CreateTheme' {..} =
    Prelude.rnf permissions `Prelude.seq`
      Prelude.rnf tags `Prelude.seq`
        Prelude.rnf versionDescription `Prelude.seq`
          Prelude.rnf awsAccountId `Prelude.seq`
            Prelude.rnf themeId `Prelude.seq`
              Prelude.rnf name `Prelude.seq`
                Prelude.rnf baseThemeId `Prelude.seq`
                  Prelude.rnf configuration

instance Data.ToHeaders CreateTheme where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateTheme where
  toJSON CreateTheme' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Permissions" Data..=) Prelude.<$> permissions,
            ("Tags" Data..=) Prelude.<$> tags,
            ("VersionDescription" Data..=)
              Prelude.<$> versionDescription,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("BaseThemeId" Data..= baseThemeId),
            Prelude.Just
              ("Configuration" Data..= configuration)
          ]
      )

instance Data.ToPath CreateTheme where
  toPath CreateTheme' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/themes/",
        Data.toBS themeId
      ]

instance Data.ToQuery CreateTheme where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateThemeResponse' smart constructor.
data CreateThemeResponse = CreateThemeResponse'
  { -- | The Amazon Resource Name (ARN) for the theme.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The theme creation status.
    creationStatus :: Prelude.Maybe ResourceStatus,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the theme.
    themeId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the new theme.
    versionArn :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateThemeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createThemeResponse_arn' - The Amazon Resource Name (ARN) for the theme.
--
-- 'creationStatus', 'createThemeResponse_creationStatus' - The theme creation status.
--
-- 'requestId', 'createThemeResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'themeId', 'createThemeResponse_themeId' - The ID of the theme.
--
-- 'versionArn', 'createThemeResponse_versionArn' - The Amazon Resource Name (ARN) for the new theme.
--
-- 'status', 'createThemeResponse_status' - The HTTP status of the request.
newCreateThemeResponse ::
  -- | 'status'
  Prelude.Int ->
  CreateThemeResponse
newCreateThemeResponse pStatus_ =
  CreateThemeResponse'
    { arn = Prelude.Nothing,
      creationStatus = Prelude.Nothing,
      requestId = Prelude.Nothing,
      themeId = Prelude.Nothing,
      versionArn = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Resource Name (ARN) for the theme.
createThemeResponse_arn :: Lens.Lens' CreateThemeResponse (Prelude.Maybe Prelude.Text)
createThemeResponse_arn = Lens.lens (\CreateThemeResponse' {arn} -> arn) (\s@CreateThemeResponse' {} a -> s {arn = a} :: CreateThemeResponse)

-- | The theme creation status.
createThemeResponse_creationStatus :: Lens.Lens' CreateThemeResponse (Prelude.Maybe ResourceStatus)
createThemeResponse_creationStatus = Lens.lens (\CreateThemeResponse' {creationStatus} -> creationStatus) (\s@CreateThemeResponse' {} a -> s {creationStatus = a} :: CreateThemeResponse)

-- | The Amazon Web Services request ID for this operation.
createThemeResponse_requestId :: Lens.Lens' CreateThemeResponse (Prelude.Maybe Prelude.Text)
createThemeResponse_requestId = Lens.lens (\CreateThemeResponse' {requestId} -> requestId) (\s@CreateThemeResponse' {} a -> s {requestId = a} :: CreateThemeResponse)

-- | The ID of the theme.
createThemeResponse_themeId :: Lens.Lens' CreateThemeResponse (Prelude.Maybe Prelude.Text)
createThemeResponse_themeId = Lens.lens (\CreateThemeResponse' {themeId} -> themeId) (\s@CreateThemeResponse' {} a -> s {themeId = a} :: CreateThemeResponse)

-- | The Amazon Resource Name (ARN) for the new theme.
createThemeResponse_versionArn :: Lens.Lens' CreateThemeResponse (Prelude.Maybe Prelude.Text)
createThemeResponse_versionArn = Lens.lens (\CreateThemeResponse' {versionArn} -> versionArn) (\s@CreateThemeResponse' {} a -> s {versionArn = a} :: CreateThemeResponse)

-- | The HTTP status of the request.
createThemeResponse_status :: Lens.Lens' CreateThemeResponse Prelude.Int
createThemeResponse_status = Lens.lens (\CreateThemeResponse' {status} -> status) (\s@CreateThemeResponse' {} a -> s {status = a} :: CreateThemeResponse)

instance Prelude.NFData CreateThemeResponse where
  rnf CreateThemeResponse' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf creationStatus `Prelude.seq`
        Prelude.rnf requestId `Prelude.seq`
          Prelude.rnf themeId `Prelude.seq`
            Prelude.rnf versionArn `Prelude.seq`
              Prelude.rnf status
