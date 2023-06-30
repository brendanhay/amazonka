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
-- Module      : Amazonka.CognitoSync.Types.CognitoStreams
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoSync.Types.CognitoStreams where

import Amazonka.CognitoSync.Types.StreamingStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configuration options for configure Cognito streams.
--
-- /See:/ 'newCognitoStreams' smart constructor.
data CognitoStreams = CognitoStreams'
  { -- | The ARN of the role Amazon Cognito can assume in order to publish to the
    -- stream. This role must grant access to Amazon Cognito (cognito-sync) to
    -- invoke PutRecord on your Cognito stream.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the Cognito stream to receive updates. This stream must be
    -- in the developers account and in the same region as the identity pool.
    streamName :: Prelude.Maybe Prelude.Text,
    -- | Status of the Cognito streams. Valid values are:
    --
    -- ENABLED - Streaming of updates to identity pool is enabled.
    --
    -- DISABLED - Streaming of updates to identity pool is disabled. Bulk
    -- publish will also fail if StreamingStatus is DISABLED.
    streamingStatus :: Prelude.Maybe StreamingStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CognitoStreams' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'cognitoStreams_roleArn' - The ARN of the role Amazon Cognito can assume in order to publish to the
-- stream. This role must grant access to Amazon Cognito (cognito-sync) to
-- invoke PutRecord on your Cognito stream.
--
-- 'streamName', 'cognitoStreams_streamName' - The name of the Cognito stream to receive updates. This stream must be
-- in the developers account and in the same region as the identity pool.
--
-- 'streamingStatus', 'cognitoStreams_streamingStatus' - Status of the Cognito streams. Valid values are:
--
-- ENABLED - Streaming of updates to identity pool is enabled.
--
-- DISABLED - Streaming of updates to identity pool is disabled. Bulk
-- publish will also fail if StreamingStatus is DISABLED.
newCognitoStreams ::
  CognitoStreams
newCognitoStreams =
  CognitoStreams'
    { roleArn = Prelude.Nothing,
      streamName = Prelude.Nothing,
      streamingStatus = Prelude.Nothing
    }

-- | The ARN of the role Amazon Cognito can assume in order to publish to the
-- stream. This role must grant access to Amazon Cognito (cognito-sync) to
-- invoke PutRecord on your Cognito stream.
cognitoStreams_roleArn :: Lens.Lens' CognitoStreams (Prelude.Maybe Prelude.Text)
cognitoStreams_roleArn = Lens.lens (\CognitoStreams' {roleArn} -> roleArn) (\s@CognitoStreams' {} a -> s {roleArn = a} :: CognitoStreams)

-- | The name of the Cognito stream to receive updates. This stream must be
-- in the developers account and in the same region as the identity pool.
cognitoStreams_streamName :: Lens.Lens' CognitoStreams (Prelude.Maybe Prelude.Text)
cognitoStreams_streamName = Lens.lens (\CognitoStreams' {streamName} -> streamName) (\s@CognitoStreams' {} a -> s {streamName = a} :: CognitoStreams)

-- | Status of the Cognito streams. Valid values are:
--
-- ENABLED - Streaming of updates to identity pool is enabled.
--
-- DISABLED - Streaming of updates to identity pool is disabled. Bulk
-- publish will also fail if StreamingStatus is DISABLED.
cognitoStreams_streamingStatus :: Lens.Lens' CognitoStreams (Prelude.Maybe StreamingStatus)
cognitoStreams_streamingStatus = Lens.lens (\CognitoStreams' {streamingStatus} -> streamingStatus) (\s@CognitoStreams' {} a -> s {streamingStatus = a} :: CognitoStreams)

instance Data.FromJSON CognitoStreams where
  parseJSON =
    Data.withObject
      "CognitoStreams"
      ( \x ->
          CognitoStreams'
            Prelude.<$> (x Data..:? "RoleArn")
            Prelude.<*> (x Data..:? "StreamName")
            Prelude.<*> (x Data..:? "StreamingStatus")
      )

instance Prelude.Hashable CognitoStreams where
  hashWithSalt _salt CognitoStreams' {..} =
    _salt
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` streamName
      `Prelude.hashWithSalt` streamingStatus

instance Prelude.NFData CognitoStreams where
  rnf CognitoStreams' {..} =
    Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf streamName
      `Prelude.seq` Prelude.rnf streamingStatus

instance Data.ToJSON CognitoStreams where
  toJSON CognitoStreams' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RoleArn" Data..=) Prelude.<$> roleArn,
            ("StreamName" Data..=) Prelude.<$> streamName,
            ("StreamingStatus" Data..=)
              Prelude.<$> streamingStatus
          ]
      )
