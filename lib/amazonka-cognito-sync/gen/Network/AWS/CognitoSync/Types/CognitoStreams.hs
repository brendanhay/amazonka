{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.Types.CognitoStreams
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoSync.Types.CognitoStreams
  ( CognitoStreams (..),

    -- * Smart constructor
    mkCognitoStreams,

    -- * Lenses
    csStreamingStatus,
    csStreamName,
    csRoleARN,
  )
where

import Network.AWS.CognitoSync.Types.StreamingStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configuration options for configure Cognito streams.
--
-- /See:/ 'mkCognitoStreams' smart constructor.
data CognitoStreams = CognitoStreams'
  { streamingStatus ::
      Lude.Maybe StreamingStatus,
    streamName :: Lude.Maybe Lude.Text,
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CognitoStreams' with the minimum fields required to make a request.
--
-- * 'roleARN' - The ARN of the role Amazon Cognito can assume in order to publish to the stream. This role must grant access to Amazon Cognito (cognito-sync) to invoke PutRecord on your Cognito stream.
-- * 'streamName' - The name of the Cognito stream to receive updates. This stream must be in the developers account and in the same region as the identity pool.
-- * 'streamingStatus' - Status of the Cognito streams. Valid values are: ENABLED - Streaming of updates to identity pool is enabled.
--
-- DISABLED - Streaming of updates to identity pool is disabled. Bulk publish will also fail if StreamingStatus is DISABLED.
mkCognitoStreams ::
  CognitoStreams
mkCognitoStreams =
  CognitoStreams'
    { streamingStatus = Lude.Nothing,
      streamName = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | Status of the Cognito streams. Valid values are: ENABLED - Streaming of updates to identity pool is enabled.
--
-- DISABLED - Streaming of updates to identity pool is disabled. Bulk publish will also fail if StreamingStatus is DISABLED.
--
-- /Note:/ Consider using 'streamingStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csStreamingStatus :: Lens.Lens' CognitoStreams (Lude.Maybe StreamingStatus)
csStreamingStatus = Lens.lens (streamingStatus :: CognitoStreams -> Lude.Maybe StreamingStatus) (\s a -> s {streamingStatus = a} :: CognitoStreams)
{-# DEPRECATED csStreamingStatus "Use generic-lens or generic-optics with 'streamingStatus' instead." #-}

-- | The name of the Cognito stream to receive updates. This stream must be in the developers account and in the same region as the identity pool.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csStreamName :: Lens.Lens' CognitoStreams (Lude.Maybe Lude.Text)
csStreamName = Lens.lens (streamName :: CognitoStreams -> Lude.Maybe Lude.Text) (\s a -> s {streamName = a} :: CognitoStreams)
{-# DEPRECATED csStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

-- | The ARN of the role Amazon Cognito can assume in order to publish to the stream. This role must grant access to Amazon Cognito (cognito-sync) to invoke PutRecord on your Cognito stream.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csRoleARN :: Lens.Lens' CognitoStreams (Lude.Maybe Lude.Text)
csRoleARN = Lens.lens (roleARN :: CognitoStreams -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: CognitoStreams)
{-# DEPRECATED csRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON CognitoStreams where
  parseJSON =
    Lude.withObject
      "CognitoStreams"
      ( \x ->
          CognitoStreams'
            Lude.<$> (x Lude..:? "StreamingStatus")
            Lude.<*> (x Lude..:? "StreamName")
            Lude.<*> (x Lude..:? "RoleArn")
      )

instance Lude.ToJSON CognitoStreams where
  toJSON CognitoStreams' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("StreamingStatus" Lude..=) Lude.<$> streamingStatus,
            ("StreamName" Lude..=) Lude.<$> streamName,
            ("RoleArn" Lude..=) Lude.<$> roleARN
          ]
      )
