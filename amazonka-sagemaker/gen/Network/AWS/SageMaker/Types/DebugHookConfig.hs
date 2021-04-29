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
-- Module      : Network.AWS.SageMaker.Types.DebugHookConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DebugHookConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    collectionConfigurations :: Prelude.Maybe [CollectionConfiguration],
    -- | Configuration information for the Debugger hook parameters.
    hookParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Path to local storage location for metrics and tensors. Defaults to
    -- @\/opt\/ml\/output\/tensors\/@.
    localPath :: Prelude.Maybe Prelude.Text,
    -- | Path to Amazon S3 storage location for metrics and tensors.
    s3OutputPath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

-- | Configuration information for Debugger tensor collections. To learn more
-- about how to configure the @CollectionConfiguration@ parameter, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/debugger-createtrainingjob-api.html Use the SageMaker and Debugger Configuration API Operations to Create, Update, and Debug Your Training Job>.
debugHookConfig_collectionConfigurations :: Lens.Lens' DebugHookConfig (Prelude.Maybe [CollectionConfiguration])
debugHookConfig_collectionConfigurations = Lens.lens (\DebugHookConfig' {collectionConfigurations} -> collectionConfigurations) (\s@DebugHookConfig' {} a -> s {collectionConfigurations = a} :: DebugHookConfig) Prelude.. Lens.mapping Prelude._Coerce

-- | Configuration information for the Debugger hook parameters.
debugHookConfig_hookParameters :: Lens.Lens' DebugHookConfig (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
debugHookConfig_hookParameters = Lens.lens (\DebugHookConfig' {hookParameters} -> hookParameters) (\s@DebugHookConfig' {} a -> s {hookParameters = a} :: DebugHookConfig) Prelude.. Lens.mapping Prelude._Coerce

-- | Path to local storage location for metrics and tensors. Defaults to
-- @\/opt\/ml\/output\/tensors\/@.
debugHookConfig_localPath :: Lens.Lens' DebugHookConfig (Prelude.Maybe Prelude.Text)
debugHookConfig_localPath = Lens.lens (\DebugHookConfig' {localPath} -> localPath) (\s@DebugHookConfig' {} a -> s {localPath = a} :: DebugHookConfig)

-- | Path to Amazon S3 storage location for metrics and tensors.
debugHookConfig_s3OutputPath :: Lens.Lens' DebugHookConfig Prelude.Text
debugHookConfig_s3OutputPath = Lens.lens (\DebugHookConfig' {s3OutputPath} -> s3OutputPath) (\s@DebugHookConfig' {} a -> s {s3OutputPath = a} :: DebugHookConfig)

instance Prelude.FromJSON DebugHookConfig where
  parseJSON =
    Prelude.withObject
      "DebugHookConfig"
      ( \x ->
          DebugHookConfig'
            Prelude.<$> ( x Prelude..:? "CollectionConfigurations"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "HookParameters"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "LocalPath")
            Prelude.<*> (x Prelude..: "S3OutputPath")
      )

instance Prelude.Hashable DebugHookConfig

instance Prelude.NFData DebugHookConfig

instance Prelude.ToJSON DebugHookConfig where
  toJSON DebugHookConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CollectionConfigurations" Prelude..=)
              Prelude.<$> collectionConfigurations,
            ("HookParameters" Prelude..=)
              Prelude.<$> hookParameters,
            ("LocalPath" Prelude..=) Prelude.<$> localPath,
            Prelude.Just
              ("S3OutputPath" Prelude..= s3OutputPath)
          ]
      )
