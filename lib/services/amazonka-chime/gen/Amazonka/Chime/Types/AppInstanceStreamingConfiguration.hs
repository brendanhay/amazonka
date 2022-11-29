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
-- Module      : Amazonka.Chime.Types.AppInstanceStreamingConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.AppInstanceStreamingConfiguration where

import Amazonka.Chime.Types.AppInstanceDataType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The details of the streaming configuration of an @AppInstance@.
--
-- /See:/ 'newAppInstanceStreamingConfiguration' smart constructor.
data AppInstanceStreamingConfiguration = AppInstanceStreamingConfiguration'
  { -- | The type of data to be streamed.
    appInstanceDataType :: AppInstanceDataType,
    -- | The resource ARN.
    resourceArn :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AppInstanceStreamingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appInstanceDataType', 'appInstanceStreamingConfiguration_appInstanceDataType' - The type of data to be streamed.
--
-- 'resourceArn', 'appInstanceStreamingConfiguration_resourceArn' - The resource ARN.
newAppInstanceStreamingConfiguration ::
  -- | 'appInstanceDataType'
  AppInstanceDataType ->
  -- | 'resourceArn'
  Prelude.Text ->
  AppInstanceStreamingConfiguration
newAppInstanceStreamingConfiguration
  pAppInstanceDataType_
  pResourceArn_ =
    AppInstanceStreamingConfiguration'
      { appInstanceDataType =
          pAppInstanceDataType_,
        resourceArn =
          Core._Sensitive Lens.# pResourceArn_
      }

-- | The type of data to be streamed.
appInstanceStreamingConfiguration_appInstanceDataType :: Lens.Lens' AppInstanceStreamingConfiguration AppInstanceDataType
appInstanceStreamingConfiguration_appInstanceDataType = Lens.lens (\AppInstanceStreamingConfiguration' {appInstanceDataType} -> appInstanceDataType) (\s@AppInstanceStreamingConfiguration' {} a -> s {appInstanceDataType = a} :: AppInstanceStreamingConfiguration)

-- | The resource ARN.
appInstanceStreamingConfiguration_resourceArn :: Lens.Lens' AppInstanceStreamingConfiguration Prelude.Text
appInstanceStreamingConfiguration_resourceArn = Lens.lens (\AppInstanceStreamingConfiguration' {resourceArn} -> resourceArn) (\s@AppInstanceStreamingConfiguration' {} a -> s {resourceArn = a} :: AppInstanceStreamingConfiguration) Prelude.. Core._Sensitive

instance
  Core.FromJSON
    AppInstanceStreamingConfiguration
  where
  parseJSON =
    Core.withObject
      "AppInstanceStreamingConfiguration"
      ( \x ->
          AppInstanceStreamingConfiguration'
            Prelude.<$> (x Core..: "AppInstanceDataType")
            Prelude.<*> (x Core..: "ResourceArn")
      )

instance
  Prelude.Hashable
    AppInstanceStreamingConfiguration
  where
  hashWithSalt
    _salt
    AppInstanceStreamingConfiguration' {..} =
      _salt `Prelude.hashWithSalt` appInstanceDataType
        `Prelude.hashWithSalt` resourceArn

instance
  Prelude.NFData
    AppInstanceStreamingConfiguration
  where
  rnf AppInstanceStreamingConfiguration' {..} =
    Prelude.rnf appInstanceDataType
      `Prelude.seq` Prelude.rnf resourceArn

instance
  Core.ToJSON
    AppInstanceStreamingConfiguration
  where
  toJSON AppInstanceStreamingConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AppInstanceDataType" Core..= appInstanceDataType),
            Prelude.Just ("ResourceArn" Core..= resourceArn)
          ]
      )
