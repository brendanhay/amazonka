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
-- Module      : Network.AWS.SageMaker.Types.DebugHookConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DebugHookConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.CollectionConfiguration

-- | Configuration information for the Debugger hook parameters, metric and
-- tensor collections, and storage paths. To learn more about how to
-- configure the @DebugHookConfig@ parameter, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/debugger-createtrainingjob-api.html Use the SageMaker and Debugger Configuration API Operations to Create, Update, and Debug Your Training Job>.
--
-- /See:/ 'newDebugHookConfig' smart constructor.
data DebugHookConfig = DebugHookConfig'
  { -- | Configuration information for Debugger tensor collections. To learn more
    -- about how to configure the @CollectionConfiguration@ parameter, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/debugger-createtrainingjob-api.html Use the SageMaker and Debugger Configuration API Operations to Create, Update, and Debug Your Training Job>.
    collectionConfigurations :: Core.Maybe [CollectionConfiguration],
    -- | Configuration information for the Debugger hook parameters.
    hookParameters :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | Path to local storage location for metrics and tensors. Defaults to
    -- @\/opt\/ml\/output\/tensors\/@.
    localPath :: Core.Maybe Core.Text,
    -- | Path to Amazon S3 storage location for metrics and tensors.
    s3OutputPath :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DebugHookConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collectionConfigurations', 'debugHookConfig_collectionConfigurations' - Configuration information for Debugger tensor collections. To learn more
-- about how to configure the @CollectionConfiguration@ parameter, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/debugger-createtrainingjob-api.html Use the SageMaker and Debugger Configuration API Operations to Create, Update, and Debug Your Training Job>.
--
-- 'hookParameters', 'debugHookConfig_hookParameters' - Configuration information for the Debugger hook parameters.
--
-- 'localPath', 'debugHookConfig_localPath' - Path to local storage location for metrics and tensors. Defaults to
-- @\/opt\/ml\/output\/tensors\/@.
--
-- 's3OutputPath', 'debugHookConfig_s3OutputPath' - Path to Amazon S3 storage location for metrics and tensors.
newDebugHookConfig ::
  -- | 's3OutputPath'
  Core.Text ->
  DebugHookConfig
newDebugHookConfig pS3OutputPath_ =
  DebugHookConfig'
    { collectionConfigurations =
        Core.Nothing,
      hookParameters = Core.Nothing,
      localPath = Core.Nothing,
      s3OutputPath = pS3OutputPath_
    }

-- | Configuration information for Debugger tensor collections. To learn more
-- about how to configure the @CollectionConfiguration@ parameter, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/debugger-createtrainingjob-api.html Use the SageMaker and Debugger Configuration API Operations to Create, Update, and Debug Your Training Job>.
debugHookConfig_collectionConfigurations :: Lens.Lens' DebugHookConfig (Core.Maybe [CollectionConfiguration])
debugHookConfig_collectionConfigurations = Lens.lens (\DebugHookConfig' {collectionConfigurations} -> collectionConfigurations) (\s@DebugHookConfig' {} a -> s {collectionConfigurations = a} :: DebugHookConfig) Core.. Lens.mapping Lens._Coerce

-- | Configuration information for the Debugger hook parameters.
debugHookConfig_hookParameters :: Lens.Lens' DebugHookConfig (Core.Maybe (Core.HashMap Core.Text Core.Text))
debugHookConfig_hookParameters = Lens.lens (\DebugHookConfig' {hookParameters} -> hookParameters) (\s@DebugHookConfig' {} a -> s {hookParameters = a} :: DebugHookConfig) Core.. Lens.mapping Lens._Coerce

-- | Path to local storage location for metrics and tensors. Defaults to
-- @\/opt\/ml\/output\/tensors\/@.
debugHookConfig_localPath :: Lens.Lens' DebugHookConfig (Core.Maybe Core.Text)
debugHookConfig_localPath = Lens.lens (\DebugHookConfig' {localPath} -> localPath) (\s@DebugHookConfig' {} a -> s {localPath = a} :: DebugHookConfig)

-- | Path to Amazon S3 storage location for metrics and tensors.
debugHookConfig_s3OutputPath :: Lens.Lens' DebugHookConfig Core.Text
debugHookConfig_s3OutputPath = Lens.lens (\DebugHookConfig' {s3OutputPath} -> s3OutputPath) (\s@DebugHookConfig' {} a -> s {s3OutputPath = a} :: DebugHookConfig)

instance Core.FromJSON DebugHookConfig where
  parseJSON =
    Core.withObject
      "DebugHookConfig"
      ( \x ->
          DebugHookConfig'
            Core.<$> ( x Core..:? "CollectionConfigurations"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "HookParameters" Core..!= Core.mempty)
            Core.<*> (x Core..:? "LocalPath")
            Core.<*> (x Core..: "S3OutputPath")
      )

instance Core.Hashable DebugHookConfig

instance Core.NFData DebugHookConfig

instance Core.ToJSON DebugHookConfig where
  toJSON DebugHookConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CollectionConfigurations" Core..=)
              Core.<$> collectionConfigurations,
            ("HookParameters" Core..=) Core.<$> hookParameters,
            ("LocalPath" Core..=) Core.<$> localPath,
            Core.Just ("S3OutputPath" Core..= s3OutputPath)
          ]
      )
