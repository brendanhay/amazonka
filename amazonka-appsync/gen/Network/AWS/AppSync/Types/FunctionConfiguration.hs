{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.FunctionConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.FunctionConfiguration where

import Network.AWS.AppSync.Types.SyncConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A function is a reusable entity. Multiple functions can be used to
-- compose the resolver logic.
--
-- /See:/ 'newFunctionConfiguration' smart constructor.
data FunctionConfiguration = FunctionConfiguration'
  { -- | The @Function@ response mapping template.
    responseMappingTemplate :: Prelude.Maybe Prelude.Text,
    -- | The version of the request mapping template. Currently only the
    -- 2018-05-29 version of the template is supported.
    functionVersion :: Prelude.Maybe Prelude.Text,
    syncConfig :: Prelude.Maybe SyncConfig,
    -- | The name of the @DataSource@.
    dataSourceName :: Prelude.Maybe Prelude.Text,
    -- | The name of the @Function@ object.
    name :: Prelude.Maybe Prelude.Text,
    -- | A unique ID representing the @Function@ object.
    functionId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the @Function@ object.
    functionArn :: Prelude.Maybe Prelude.Text,
    -- | The @Function@ description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The @Function@ request mapping template. Functions support only the
    -- 2018-05-29 version of the request mapping template.
    requestMappingTemplate :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FunctionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'responseMappingTemplate', 'functionConfiguration_responseMappingTemplate' - The @Function@ response mapping template.
--
-- 'functionVersion', 'functionConfiguration_functionVersion' - The version of the request mapping template. Currently only the
-- 2018-05-29 version of the template is supported.
--
-- 'syncConfig', 'functionConfiguration_syncConfig' - Undocumented member.
--
-- 'dataSourceName', 'functionConfiguration_dataSourceName' - The name of the @DataSource@.
--
-- 'name', 'functionConfiguration_name' - The name of the @Function@ object.
--
-- 'functionId', 'functionConfiguration_functionId' - A unique ID representing the @Function@ object.
--
-- 'functionArn', 'functionConfiguration_functionArn' - The ARN of the @Function@ object.
--
-- 'description', 'functionConfiguration_description' - The @Function@ description.
--
-- 'requestMappingTemplate', 'functionConfiguration_requestMappingTemplate' - The @Function@ request mapping template. Functions support only the
-- 2018-05-29 version of the request mapping template.
newFunctionConfiguration ::
  FunctionConfiguration
newFunctionConfiguration =
  FunctionConfiguration'
    { responseMappingTemplate =
        Prelude.Nothing,
      functionVersion = Prelude.Nothing,
      syncConfig = Prelude.Nothing,
      dataSourceName = Prelude.Nothing,
      name = Prelude.Nothing,
      functionId = Prelude.Nothing,
      functionArn = Prelude.Nothing,
      description = Prelude.Nothing,
      requestMappingTemplate = Prelude.Nothing
    }

-- | The @Function@ response mapping template.
functionConfiguration_responseMappingTemplate :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_responseMappingTemplate = Lens.lens (\FunctionConfiguration' {responseMappingTemplate} -> responseMappingTemplate) (\s@FunctionConfiguration' {} a -> s {responseMappingTemplate = a} :: FunctionConfiguration)

-- | The version of the request mapping template. Currently only the
-- 2018-05-29 version of the template is supported.
functionConfiguration_functionVersion :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_functionVersion = Lens.lens (\FunctionConfiguration' {functionVersion} -> functionVersion) (\s@FunctionConfiguration' {} a -> s {functionVersion = a} :: FunctionConfiguration)

-- | Undocumented member.
functionConfiguration_syncConfig :: Lens.Lens' FunctionConfiguration (Prelude.Maybe SyncConfig)
functionConfiguration_syncConfig = Lens.lens (\FunctionConfiguration' {syncConfig} -> syncConfig) (\s@FunctionConfiguration' {} a -> s {syncConfig = a} :: FunctionConfiguration)

-- | The name of the @DataSource@.
functionConfiguration_dataSourceName :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_dataSourceName = Lens.lens (\FunctionConfiguration' {dataSourceName} -> dataSourceName) (\s@FunctionConfiguration' {} a -> s {dataSourceName = a} :: FunctionConfiguration)

-- | The name of the @Function@ object.
functionConfiguration_name :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_name = Lens.lens (\FunctionConfiguration' {name} -> name) (\s@FunctionConfiguration' {} a -> s {name = a} :: FunctionConfiguration)

-- | A unique ID representing the @Function@ object.
functionConfiguration_functionId :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_functionId = Lens.lens (\FunctionConfiguration' {functionId} -> functionId) (\s@FunctionConfiguration' {} a -> s {functionId = a} :: FunctionConfiguration)

-- | The ARN of the @Function@ object.
functionConfiguration_functionArn :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_functionArn = Lens.lens (\FunctionConfiguration' {functionArn} -> functionArn) (\s@FunctionConfiguration' {} a -> s {functionArn = a} :: FunctionConfiguration)

-- | The @Function@ description.
functionConfiguration_description :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_description = Lens.lens (\FunctionConfiguration' {description} -> description) (\s@FunctionConfiguration' {} a -> s {description = a} :: FunctionConfiguration)

-- | The @Function@ request mapping template. Functions support only the
-- 2018-05-29 version of the request mapping template.
functionConfiguration_requestMappingTemplate :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_requestMappingTemplate = Lens.lens (\FunctionConfiguration' {requestMappingTemplate} -> requestMappingTemplate) (\s@FunctionConfiguration' {} a -> s {requestMappingTemplate = a} :: FunctionConfiguration)

instance Prelude.FromJSON FunctionConfiguration where
  parseJSON =
    Prelude.withObject
      "FunctionConfiguration"
      ( \x ->
          FunctionConfiguration'
            Prelude.<$> (x Prelude..:? "responseMappingTemplate")
            Prelude.<*> (x Prelude..:? "functionVersion")
            Prelude.<*> (x Prelude..:? "syncConfig")
            Prelude.<*> (x Prelude..:? "dataSourceName")
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> (x Prelude..:? "functionId")
            Prelude.<*> (x Prelude..:? "functionArn")
            Prelude.<*> (x Prelude..:? "description")
            Prelude.<*> (x Prelude..:? "requestMappingTemplate")
      )

instance Prelude.Hashable FunctionConfiguration

instance Prelude.NFData FunctionConfiguration
