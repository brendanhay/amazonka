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
-- Module      : Amazonka.ChimeSDKMessaging.Types.StreamingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKMessaging.Types.StreamingConfiguration where

import Amazonka.ChimeSDKMessaging.Types.MessagingDataType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration for connecting a messaging stream to Amazon Kinesis.
--
-- /See:/ 'newStreamingConfiguration' smart constructor.
data StreamingConfiguration = StreamingConfiguration'
  { -- | The data type of the configuration.
    dataType :: MessagingDataType,
    -- | The ARN of the resource in the configuration.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StreamingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataType', 'streamingConfiguration_dataType' - The data type of the configuration.
--
-- 'resourceArn', 'streamingConfiguration_resourceArn' - The ARN of the resource in the configuration.
newStreamingConfiguration ::
  -- | 'dataType'
  MessagingDataType ->
  -- | 'resourceArn'
  Prelude.Text ->
  StreamingConfiguration
newStreamingConfiguration pDataType_ pResourceArn_ =
  StreamingConfiguration'
    { dataType = pDataType_,
      resourceArn = pResourceArn_
    }

-- | The data type of the configuration.
streamingConfiguration_dataType :: Lens.Lens' StreamingConfiguration MessagingDataType
streamingConfiguration_dataType = Lens.lens (\StreamingConfiguration' {dataType} -> dataType) (\s@StreamingConfiguration' {} a -> s {dataType = a} :: StreamingConfiguration)

-- | The ARN of the resource in the configuration.
streamingConfiguration_resourceArn :: Lens.Lens' StreamingConfiguration Prelude.Text
streamingConfiguration_resourceArn = Lens.lens (\StreamingConfiguration' {resourceArn} -> resourceArn) (\s@StreamingConfiguration' {} a -> s {resourceArn = a} :: StreamingConfiguration)

instance Data.FromJSON StreamingConfiguration where
  parseJSON =
    Data.withObject
      "StreamingConfiguration"
      ( \x ->
          StreamingConfiguration'
            Prelude.<$> (x Data..: "DataType")
            Prelude.<*> (x Data..: "ResourceArn")
      )

instance Prelude.Hashable StreamingConfiguration where
  hashWithSalt _salt StreamingConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` dataType
      `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData StreamingConfiguration where
  rnf StreamingConfiguration' {..} =
    Prelude.rnf dataType
      `Prelude.seq` Prelude.rnf resourceArn

instance Data.ToJSON StreamingConfiguration where
  toJSON StreamingConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DataType" Data..= dataType),
            Prelude.Just ("ResourceArn" Data..= resourceArn)
          ]
      )
