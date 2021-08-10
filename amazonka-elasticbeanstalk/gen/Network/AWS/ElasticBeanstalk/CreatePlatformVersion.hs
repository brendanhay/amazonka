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
-- Module      : Network.AWS.ElasticBeanstalk.CreatePlatformVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new version of your custom platform.
module Network.AWS.ElasticBeanstalk.CreatePlatformVersion
  ( -- * Creating a Request
    CreatePlatformVersion (..),
    newCreatePlatformVersion,

    -- * Request Lenses
    createPlatformVersion_environmentName,
    createPlatformVersion_tags,
    createPlatformVersion_optionSettings,
    createPlatformVersion_platformName,
    createPlatformVersion_platformVersion,
    createPlatformVersion_platformDefinitionBundle,

    -- * Destructuring the Response
    CreatePlatformVersionResponse (..),
    newCreatePlatformVersionResponse,

    -- * Response Lenses
    createPlatformVersionResponse_platformSummary,
    createPlatformVersionResponse_builder,
    createPlatformVersionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to create a new platform version.
--
-- /See:/ 'newCreatePlatformVersion' smart constructor.
data CreatePlatformVersion = CreatePlatformVersion'
  { -- | The name of the builder environment.
    environmentName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the tags applied to the new platform version.
    --
    -- Elastic Beanstalk applies these tags only to the platform version.
    -- Environments that you create using the platform version don\'t inherit
    -- the tags.
    tags :: Prelude.Maybe [Tag],
    -- | The configuration option settings to apply to the builder environment.
    optionSettings :: Prelude.Maybe [ConfigurationOptionSetting],
    -- | The name of your custom platform.
    platformName :: Prelude.Text,
    -- | The number, such as 1.0.2, for the new platform version.
    platformVersion :: Prelude.Text,
    -- | The location of the platform definition archive in Amazon S3.
    platformDefinitionBundle :: S3Location
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePlatformVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentName', 'createPlatformVersion_environmentName' - The name of the builder environment.
--
-- 'tags', 'createPlatformVersion_tags' - Specifies the tags applied to the new platform version.
--
-- Elastic Beanstalk applies these tags only to the platform version.
-- Environments that you create using the platform version don\'t inherit
-- the tags.
--
-- 'optionSettings', 'createPlatformVersion_optionSettings' - The configuration option settings to apply to the builder environment.
--
-- 'platformName', 'createPlatformVersion_platformName' - The name of your custom platform.
--
-- 'platformVersion', 'createPlatformVersion_platformVersion' - The number, such as 1.0.2, for the new platform version.
--
-- 'platformDefinitionBundle', 'createPlatformVersion_platformDefinitionBundle' - The location of the platform definition archive in Amazon S3.
newCreatePlatformVersion ::
  -- | 'platformName'
  Prelude.Text ->
  -- | 'platformVersion'
  Prelude.Text ->
  -- | 'platformDefinitionBundle'
  S3Location ->
  CreatePlatformVersion
newCreatePlatformVersion
  pPlatformName_
  pPlatformVersion_
  pPlatformDefinitionBundle_ =
    CreatePlatformVersion'
      { environmentName =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        optionSettings = Prelude.Nothing,
        platformName = pPlatformName_,
        platformVersion = pPlatformVersion_,
        platformDefinitionBundle =
          pPlatformDefinitionBundle_
      }

-- | The name of the builder environment.
createPlatformVersion_environmentName :: Lens.Lens' CreatePlatformVersion (Prelude.Maybe Prelude.Text)
createPlatformVersion_environmentName = Lens.lens (\CreatePlatformVersion' {environmentName} -> environmentName) (\s@CreatePlatformVersion' {} a -> s {environmentName = a} :: CreatePlatformVersion)

-- | Specifies the tags applied to the new platform version.
--
-- Elastic Beanstalk applies these tags only to the platform version.
-- Environments that you create using the platform version don\'t inherit
-- the tags.
createPlatformVersion_tags :: Lens.Lens' CreatePlatformVersion (Prelude.Maybe [Tag])
createPlatformVersion_tags = Lens.lens (\CreatePlatformVersion' {tags} -> tags) (\s@CreatePlatformVersion' {} a -> s {tags = a} :: CreatePlatformVersion) Prelude.. Lens.mapping Lens._Coerce

-- | The configuration option settings to apply to the builder environment.
createPlatformVersion_optionSettings :: Lens.Lens' CreatePlatformVersion (Prelude.Maybe [ConfigurationOptionSetting])
createPlatformVersion_optionSettings = Lens.lens (\CreatePlatformVersion' {optionSettings} -> optionSettings) (\s@CreatePlatformVersion' {} a -> s {optionSettings = a} :: CreatePlatformVersion) Prelude.. Lens.mapping Lens._Coerce

-- | The name of your custom platform.
createPlatformVersion_platformName :: Lens.Lens' CreatePlatformVersion Prelude.Text
createPlatformVersion_platformName = Lens.lens (\CreatePlatformVersion' {platformName} -> platformName) (\s@CreatePlatformVersion' {} a -> s {platformName = a} :: CreatePlatformVersion)

-- | The number, such as 1.0.2, for the new platform version.
createPlatformVersion_platformVersion :: Lens.Lens' CreatePlatformVersion Prelude.Text
createPlatformVersion_platformVersion = Lens.lens (\CreatePlatformVersion' {platformVersion} -> platformVersion) (\s@CreatePlatformVersion' {} a -> s {platformVersion = a} :: CreatePlatformVersion)

-- | The location of the platform definition archive in Amazon S3.
createPlatformVersion_platformDefinitionBundle :: Lens.Lens' CreatePlatformVersion S3Location
createPlatformVersion_platformDefinitionBundle = Lens.lens (\CreatePlatformVersion' {platformDefinitionBundle} -> platformDefinitionBundle) (\s@CreatePlatformVersion' {} a -> s {platformDefinitionBundle = a} :: CreatePlatformVersion)

instance Core.AWSRequest CreatePlatformVersion where
  type
    AWSResponse CreatePlatformVersion =
      CreatePlatformVersionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreatePlatformVersionResult"
      ( \s h x ->
          CreatePlatformVersionResponse'
            Prelude.<$> (x Core..@? "PlatformSummary")
            Prelude.<*> (x Core..@? "Builder")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePlatformVersion

instance Prelude.NFData CreatePlatformVersion

instance Core.ToHeaders CreatePlatformVersion where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreatePlatformVersion where
  toPath = Prelude.const "/"

instance Core.ToQuery CreatePlatformVersion where
  toQuery CreatePlatformVersion' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreatePlatformVersion" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-12-01" :: Prelude.ByteString),
        "EnvironmentName" Core.=: environmentName,
        "Tags"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> tags),
        "OptionSettings"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> optionSettings
            ),
        "PlatformName" Core.=: platformName,
        "PlatformVersion" Core.=: platformVersion,
        "PlatformDefinitionBundle"
          Core.=: platformDefinitionBundle
      ]

-- | /See:/ 'newCreatePlatformVersionResponse' smart constructor.
data CreatePlatformVersionResponse = CreatePlatformVersionResponse'
  { -- | Detailed information about the new version of the custom platform.
    platformSummary :: Prelude.Maybe PlatformSummary,
    -- | The builder used to create the custom platform.
    builder :: Prelude.Maybe Builder,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePlatformVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'platformSummary', 'createPlatformVersionResponse_platformSummary' - Detailed information about the new version of the custom platform.
--
-- 'builder', 'createPlatformVersionResponse_builder' - The builder used to create the custom platform.
--
-- 'httpStatus', 'createPlatformVersionResponse_httpStatus' - The response's http status code.
newCreatePlatformVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreatePlatformVersionResponse
newCreatePlatformVersionResponse pHttpStatus_ =
  CreatePlatformVersionResponse'
    { platformSummary =
        Prelude.Nothing,
      builder = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Detailed information about the new version of the custom platform.
createPlatformVersionResponse_platformSummary :: Lens.Lens' CreatePlatformVersionResponse (Prelude.Maybe PlatformSummary)
createPlatformVersionResponse_platformSummary = Lens.lens (\CreatePlatformVersionResponse' {platformSummary} -> platformSummary) (\s@CreatePlatformVersionResponse' {} a -> s {platformSummary = a} :: CreatePlatformVersionResponse)

-- | The builder used to create the custom platform.
createPlatformVersionResponse_builder :: Lens.Lens' CreatePlatformVersionResponse (Prelude.Maybe Builder)
createPlatformVersionResponse_builder = Lens.lens (\CreatePlatformVersionResponse' {builder} -> builder) (\s@CreatePlatformVersionResponse' {} a -> s {builder = a} :: CreatePlatformVersionResponse)

-- | The response's http status code.
createPlatformVersionResponse_httpStatus :: Lens.Lens' CreatePlatformVersionResponse Prelude.Int
createPlatformVersionResponse_httpStatus = Lens.lens (\CreatePlatformVersionResponse' {httpStatus} -> httpStatus) (\s@CreatePlatformVersionResponse' {} a -> s {httpStatus = a} :: CreatePlatformVersionResponse)

instance Prelude.NFData CreatePlatformVersionResponse
