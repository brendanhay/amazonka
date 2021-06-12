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
-- Module      : Network.AWS.CognitoSync.Types.CognitoStreams
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoSync.Types.CognitoStreams where

import Network.AWS.CognitoSync.Types.StreamingStatus
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Configuration options for configure Cognito streams.
--
-- /See:/ 'newCognitoStreams' smart constructor.
data CognitoStreams = CognitoStreams'
  { -- | The ARN of the role Amazon Cognito can assume in order to publish to the
    -- stream. This role must grant access to Amazon Cognito (cognito-sync) to
    -- invoke PutRecord on your Cognito stream.
    roleArn :: Core.Maybe Core.Text,
    -- | The name of the Cognito stream to receive updates. This stream must be
    -- in the developers account and in the same region as the identity pool.
    streamName :: Core.Maybe Core.Text,
    -- | Status of the Cognito streams. Valid values are:
    --
    -- ENABLED - Streaming of updates to identity pool is enabled.
    --
    -- DISABLED - Streaming of updates to identity pool is disabled. Bulk
    -- publish will also fail if StreamingStatus is DISABLED.
    streamingStatus :: Core.Maybe StreamingStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { roleArn = Core.Nothing,
      streamName = Core.Nothing,
      streamingStatus = Core.Nothing
    }

-- | The ARN of the role Amazon Cognito can assume in order to publish to the
-- stream. This role must grant access to Amazon Cognito (cognito-sync) to
-- invoke PutRecord on your Cognito stream.
cognitoStreams_roleArn :: Lens.Lens' CognitoStreams (Core.Maybe Core.Text)
cognitoStreams_roleArn = Lens.lens (\CognitoStreams' {roleArn} -> roleArn) (\s@CognitoStreams' {} a -> s {roleArn = a} :: CognitoStreams)

-- | The name of the Cognito stream to receive updates. This stream must be
-- in the developers account and in the same region as the identity pool.
cognitoStreams_streamName :: Lens.Lens' CognitoStreams (Core.Maybe Core.Text)
cognitoStreams_streamName = Lens.lens (\CognitoStreams' {streamName} -> streamName) (\s@CognitoStreams' {} a -> s {streamName = a} :: CognitoStreams)

-- | Status of the Cognito streams. Valid values are:
--
-- ENABLED - Streaming of updates to identity pool is enabled.
--
-- DISABLED - Streaming of updates to identity pool is disabled. Bulk
-- publish will also fail if StreamingStatus is DISABLED.
cognitoStreams_streamingStatus :: Lens.Lens' CognitoStreams (Core.Maybe StreamingStatus)
cognitoStreams_streamingStatus = Lens.lens (\CognitoStreams' {streamingStatus} -> streamingStatus) (\s@CognitoStreams' {} a -> s {streamingStatus = a} :: CognitoStreams)

instance Core.FromJSON CognitoStreams where
  parseJSON =
    Core.withObject
      "CognitoStreams"
      ( \x ->
          CognitoStreams'
            Core.<$> (x Core..:? "RoleArn")
            Core.<*> (x Core..:? "StreamName")
            Core.<*> (x Core..:? "StreamingStatus")
      )

instance Core.Hashable CognitoStreams

instance Core.NFData CognitoStreams

instance Core.ToJSON CognitoStreams where
  toJSON CognitoStreams' {..} =
    Core.object
      ( Core.catMaybes
          [ ("RoleArn" Core..=) Core.<$> roleArn,
            ("StreamName" Core..=) Core.<$> streamName,
            ("StreamingStatus" Core..=)
              Core.<$> streamingStatus
          ]
      )
