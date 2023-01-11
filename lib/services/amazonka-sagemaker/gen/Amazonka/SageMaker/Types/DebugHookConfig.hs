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
-- Module      : Amazonka.SageMaker.Types.DebugHookConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.DebugHookConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.CollectionConfiguration

-- | Configuration information for the Amazon SageMaker Debugger hook
-- parameters, metric and tensor collections, and storage paths. To learn
-- more about how to configure the @DebugHookConfig@ parameter, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/debugger-createtrainingjob-api.html Use the SageMaker and Debugger Configuration API Operations to Create, Update, and Debug Your Training Job>.
--
-- /See:/ 'newDebugHookConfig' smart constructor.
data DebugHookConfig = DebugHookConfig'
  { -- | Configuration information for Amazon SageMaker Debugger tensor
    -- collections. To learn more about how to configure the
    -- @CollectionConfiguration@ parameter, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/debugger-createtrainingjob-api.html Use the SageMaker and Debugger Configuration API Operations to Create, Update, and Debug Your Training Job>.
    collectionConfigurations :: Prelude.Maybe [CollectionConfiguration],
    -- | Configuration information for the Amazon SageMaker Debugger hook
    -- parameters.
    hookParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Path to local storage location for metrics and tensors. Defaults to
    -- @\/opt\/ml\/output\/tensors\/@.
    localPath :: Prelude.Maybe Prelude.Text,
    -- | Path to Amazon S3 storage location for metrics and tensors.
    s3OutputPath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DebugHookConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collectionConfigurations', 'debugHookConfig_collectionConfigurations' - Configuration information for Amazon SageMaker Debugger tensor
-- collections. To learn more about how to configure the
-- @CollectionConfiguration@ parameter, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/debugger-createtrainingjob-api.html Use the SageMaker and Debugger Configuration API Operations to Create, Update, and Debug Your Training Job>.
--
-- 'hookParameters', 'debugHookConfig_hookParameters' - Configuration information for the Amazon SageMaker Debugger hook
-- parameters.
--
-- 'localPath', 'debugHookConfig_localPath' - Path to local storage location for metrics and tensors. Defaults to
-- @\/opt\/ml\/output\/tensors\/@.
--
-- 's3OutputPath', 'debugHookConfig_s3OutputPath' - Path to Amazon S3 storage location for metrics and tensors.
newDebugHookConfig ::
  -- | 's3OutputPath'
  Prelude.Text ->
  DebugHookConfig
newDebugHookConfig pS3OutputPath_ =
  DebugHookConfig'
    { collectionConfigurations =
        Prelude.Nothing,
      hookParameters = Prelude.Nothing,
      localPath = Prelude.Nothing,
      s3OutputPath = pS3OutputPath_
    }

-- | Configuration information for Amazon SageMaker Debugger tensor
-- collections. To learn more about how to configure the
-- @CollectionConfiguration@ parameter, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/debugger-createtrainingjob-api.html Use the SageMaker and Debugger Configuration API Operations to Create, Update, and Debug Your Training Job>.
debugHookConfig_collectionConfigurations :: Lens.Lens' DebugHookConfig (Prelude.Maybe [CollectionConfiguration])
debugHookConfig_collectionConfigurations = Lens.lens (\DebugHookConfig' {collectionConfigurations} -> collectionConfigurations) (\s@DebugHookConfig' {} a -> s {collectionConfigurations = a} :: DebugHookConfig) Prelude.. Lens.mapping Lens.coerced

-- | Configuration information for the Amazon SageMaker Debugger hook
-- parameters.
debugHookConfig_hookParameters :: Lens.Lens' DebugHookConfig (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
debugHookConfig_hookParameters = Lens.lens (\DebugHookConfig' {hookParameters} -> hookParameters) (\s@DebugHookConfig' {} a -> s {hookParameters = a} :: DebugHookConfig) Prelude.. Lens.mapping Lens.coerced

-- | Path to local storage location for metrics and tensors. Defaults to
-- @\/opt\/ml\/output\/tensors\/@.
debugHookConfig_localPath :: Lens.Lens' DebugHookConfig (Prelude.Maybe Prelude.Text)
debugHookConfig_localPath = Lens.lens (\DebugHookConfig' {localPath} -> localPath) (\s@DebugHookConfig' {} a -> s {localPath = a} :: DebugHookConfig)

-- | Path to Amazon S3 storage location for metrics and tensors.
debugHookConfig_s3OutputPath :: Lens.Lens' DebugHookConfig Prelude.Text
debugHookConfig_s3OutputPath = Lens.lens (\DebugHookConfig' {s3OutputPath} -> s3OutputPath) (\s@DebugHookConfig' {} a -> s {s3OutputPath = a} :: DebugHookConfig)

instance Data.FromJSON DebugHookConfig where
  parseJSON =
    Data.withObject
      "DebugHookConfig"
      ( \x ->
          DebugHookConfig'
            Prelude.<$> ( x Data..:? "CollectionConfigurations"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "HookParameters" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "LocalPath")
            Prelude.<*> (x Data..: "S3OutputPath")
      )

instance Prelude.Hashable DebugHookConfig where
  hashWithSalt _salt DebugHookConfig' {..} =
    _salt
      `Prelude.hashWithSalt` collectionConfigurations
      `Prelude.hashWithSalt` hookParameters
      `Prelude.hashWithSalt` localPath
      `Prelude.hashWithSalt` s3OutputPath

instance Prelude.NFData DebugHookConfig where
  rnf DebugHookConfig' {..} =
    Prelude.rnf collectionConfigurations
      `Prelude.seq` Prelude.rnf hookParameters
      `Prelude.seq` Prelude.rnf localPath
      `Prelude.seq` Prelude.rnf s3OutputPath

instance Data.ToJSON DebugHookConfig where
  toJSON DebugHookConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CollectionConfigurations" Data..=)
              Prelude.<$> collectionConfigurations,
            ("HookParameters" Data..=)
              Prelude.<$> hookParameters,
            ("LocalPath" Data..=) Prelude.<$> localPath,
            Prelude.Just ("S3OutputPath" Data..= s3OutputPath)
          ]
      )
