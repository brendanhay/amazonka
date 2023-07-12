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
-- Module      : Amazonka.AppSync.Types.FunctionConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Types.FunctionConfiguration where

import Amazonka.AppSync.Types.AppSyncRuntime
import Amazonka.AppSync.Types.SyncConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A function is a reusable entity. You can use multiple functions to
-- compose the resolver logic.
--
-- /See:/ 'newFunctionConfiguration' smart constructor.
data FunctionConfiguration = FunctionConfiguration'
  { -- | The @function@ code that contains the request and response functions.
    -- When code is used, the @runtime@ is required. The @runtime@ value must
    -- be @APPSYNC_JS@.
    code :: Prelude.Maybe Prelude.Text,
    -- | The name of the @DataSource@.
    dataSourceName :: Prelude.Maybe Prelude.Text,
    -- | The @Function@ description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the @Function@ object.
    functionArn :: Prelude.Maybe Prelude.Text,
    -- | A unique ID representing the @Function@ object.
    functionId :: Prelude.Maybe Prelude.Text,
    -- | The version of the request mapping template. Currently, only the
    -- 2018-05-29 version of the template is supported.
    functionVersion :: Prelude.Maybe Prelude.Text,
    -- | The maximum batching size for a resolver.
    maxBatchSize :: Prelude.Maybe Prelude.Natural,
    -- | The name of the @Function@ object.
    name :: Prelude.Maybe Prelude.Text,
    -- | The @Function@ request mapping template. Functions support only the
    -- 2018-05-29 version of the request mapping template.
    requestMappingTemplate :: Prelude.Maybe Prelude.Text,
    -- | The @Function@ response mapping template.
    responseMappingTemplate :: Prelude.Maybe Prelude.Text,
    runtime :: Prelude.Maybe AppSyncRuntime,
    syncConfig :: Prelude.Maybe SyncConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FunctionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'functionConfiguration_code' - The @function@ code that contains the request and response functions.
-- When code is used, the @runtime@ is required. The @runtime@ value must
-- be @APPSYNC_JS@.
--
-- 'dataSourceName', 'functionConfiguration_dataSourceName' - The name of the @DataSource@.
--
-- 'description', 'functionConfiguration_description' - The @Function@ description.
--
-- 'functionArn', 'functionConfiguration_functionArn' - The Amazon Resource Name (ARN) of the @Function@ object.
--
-- 'functionId', 'functionConfiguration_functionId' - A unique ID representing the @Function@ object.
--
-- 'functionVersion', 'functionConfiguration_functionVersion' - The version of the request mapping template. Currently, only the
-- 2018-05-29 version of the template is supported.
--
-- 'maxBatchSize', 'functionConfiguration_maxBatchSize' - The maximum batching size for a resolver.
--
-- 'name', 'functionConfiguration_name' - The name of the @Function@ object.
--
-- 'requestMappingTemplate', 'functionConfiguration_requestMappingTemplate' - The @Function@ request mapping template. Functions support only the
-- 2018-05-29 version of the request mapping template.
--
-- 'responseMappingTemplate', 'functionConfiguration_responseMappingTemplate' - The @Function@ response mapping template.
--
-- 'runtime', 'functionConfiguration_runtime' - Undocumented member.
--
-- 'syncConfig', 'functionConfiguration_syncConfig' - Undocumented member.
newFunctionConfiguration ::
  FunctionConfiguration
newFunctionConfiguration =
  FunctionConfiguration'
    { code = Prelude.Nothing,
      dataSourceName = Prelude.Nothing,
      description = Prelude.Nothing,
      functionArn = Prelude.Nothing,
      functionId = Prelude.Nothing,
      functionVersion = Prelude.Nothing,
      maxBatchSize = Prelude.Nothing,
      name = Prelude.Nothing,
      requestMappingTemplate = Prelude.Nothing,
      responseMappingTemplate = Prelude.Nothing,
      runtime = Prelude.Nothing,
      syncConfig = Prelude.Nothing
    }

-- | The @function@ code that contains the request and response functions.
-- When code is used, the @runtime@ is required. The @runtime@ value must
-- be @APPSYNC_JS@.
functionConfiguration_code :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_code = Lens.lens (\FunctionConfiguration' {code} -> code) (\s@FunctionConfiguration' {} a -> s {code = a} :: FunctionConfiguration)

-- | The name of the @DataSource@.
functionConfiguration_dataSourceName :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_dataSourceName = Lens.lens (\FunctionConfiguration' {dataSourceName} -> dataSourceName) (\s@FunctionConfiguration' {} a -> s {dataSourceName = a} :: FunctionConfiguration)

-- | The @Function@ description.
functionConfiguration_description :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_description = Lens.lens (\FunctionConfiguration' {description} -> description) (\s@FunctionConfiguration' {} a -> s {description = a} :: FunctionConfiguration)

-- | The Amazon Resource Name (ARN) of the @Function@ object.
functionConfiguration_functionArn :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_functionArn = Lens.lens (\FunctionConfiguration' {functionArn} -> functionArn) (\s@FunctionConfiguration' {} a -> s {functionArn = a} :: FunctionConfiguration)

-- | A unique ID representing the @Function@ object.
functionConfiguration_functionId :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_functionId = Lens.lens (\FunctionConfiguration' {functionId} -> functionId) (\s@FunctionConfiguration' {} a -> s {functionId = a} :: FunctionConfiguration)

-- | The version of the request mapping template. Currently, only the
-- 2018-05-29 version of the template is supported.
functionConfiguration_functionVersion :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_functionVersion = Lens.lens (\FunctionConfiguration' {functionVersion} -> functionVersion) (\s@FunctionConfiguration' {} a -> s {functionVersion = a} :: FunctionConfiguration)

-- | The maximum batching size for a resolver.
functionConfiguration_maxBatchSize :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Natural)
functionConfiguration_maxBatchSize = Lens.lens (\FunctionConfiguration' {maxBatchSize} -> maxBatchSize) (\s@FunctionConfiguration' {} a -> s {maxBatchSize = a} :: FunctionConfiguration)

-- | The name of the @Function@ object.
functionConfiguration_name :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_name = Lens.lens (\FunctionConfiguration' {name} -> name) (\s@FunctionConfiguration' {} a -> s {name = a} :: FunctionConfiguration)

-- | The @Function@ request mapping template. Functions support only the
-- 2018-05-29 version of the request mapping template.
functionConfiguration_requestMappingTemplate :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_requestMappingTemplate = Lens.lens (\FunctionConfiguration' {requestMappingTemplate} -> requestMappingTemplate) (\s@FunctionConfiguration' {} a -> s {requestMappingTemplate = a} :: FunctionConfiguration)

-- | The @Function@ response mapping template.
functionConfiguration_responseMappingTemplate :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_responseMappingTemplate = Lens.lens (\FunctionConfiguration' {responseMappingTemplate} -> responseMappingTemplate) (\s@FunctionConfiguration' {} a -> s {responseMappingTemplate = a} :: FunctionConfiguration)

-- | Undocumented member.
functionConfiguration_runtime :: Lens.Lens' FunctionConfiguration (Prelude.Maybe AppSyncRuntime)
functionConfiguration_runtime = Lens.lens (\FunctionConfiguration' {runtime} -> runtime) (\s@FunctionConfiguration' {} a -> s {runtime = a} :: FunctionConfiguration)

-- | Undocumented member.
functionConfiguration_syncConfig :: Lens.Lens' FunctionConfiguration (Prelude.Maybe SyncConfig)
functionConfiguration_syncConfig = Lens.lens (\FunctionConfiguration' {syncConfig} -> syncConfig) (\s@FunctionConfiguration' {} a -> s {syncConfig = a} :: FunctionConfiguration)

instance Data.FromJSON FunctionConfiguration where
  parseJSON =
    Data.withObject
      "FunctionConfiguration"
      ( \x ->
          FunctionConfiguration'
            Prelude.<$> (x Data..:? "code")
            Prelude.<*> (x Data..:? "dataSourceName")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "functionArn")
            Prelude.<*> (x Data..:? "functionId")
            Prelude.<*> (x Data..:? "functionVersion")
            Prelude.<*> (x Data..:? "maxBatchSize")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "requestMappingTemplate")
            Prelude.<*> (x Data..:? "responseMappingTemplate")
            Prelude.<*> (x Data..:? "runtime")
            Prelude.<*> (x Data..:? "syncConfig")
      )

instance Prelude.Hashable FunctionConfiguration where
  hashWithSalt _salt FunctionConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` dataSourceName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` functionArn
      `Prelude.hashWithSalt` functionId
      `Prelude.hashWithSalt` functionVersion
      `Prelude.hashWithSalt` maxBatchSize
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` requestMappingTemplate
      `Prelude.hashWithSalt` responseMappingTemplate
      `Prelude.hashWithSalt` runtime
      `Prelude.hashWithSalt` syncConfig

instance Prelude.NFData FunctionConfiguration where
  rnf FunctionConfiguration' {..} =
    Prelude.rnf code
      `Prelude.seq` Prelude.rnf dataSourceName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf functionArn
      `Prelude.seq` Prelude.rnf functionId
      `Prelude.seq` Prelude.rnf functionVersion
      `Prelude.seq` Prelude.rnf maxBatchSize
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf requestMappingTemplate
      `Prelude.seq` Prelude.rnf responseMappingTemplate
      `Prelude.seq` Prelude.rnf runtime
      `Prelude.seq` Prelude.rnf syncConfig
