{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.Types.CognitoStreams
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoSync.Types.CognitoStreams
  ( CognitoStreams (..)
  -- * Smart constructor
  , mkCognitoStreams
  -- * Lenses
  , csRoleArn
  , csStreamName
  , csStreamingStatus
  ) where

import qualified Network.AWS.CognitoSync.Types.AssumeRoleArn as Types
import qualified Network.AWS.CognitoSync.Types.StreamName as Types
import qualified Network.AWS.CognitoSync.Types.StreamingStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Configuration options for configure Cognito streams.
--
-- /See:/ 'mkCognitoStreams' smart constructor.
data CognitoStreams = CognitoStreams'
  { roleArn :: Core.Maybe Types.AssumeRoleArn
    -- ^ The ARN of the role Amazon Cognito can assume in order to publish to the stream. This role must grant access to Amazon Cognito (cognito-sync) to invoke PutRecord on your Cognito stream.
  , streamName :: Core.Maybe Types.StreamName
    -- ^ The name of the Cognito stream to receive updates. This stream must be in the developers account and in the same region as the identity pool.
  , streamingStatus :: Core.Maybe Types.StreamingStatus
    -- ^ Status of the Cognito streams. Valid values are: ENABLED - Streaming of updates to identity pool is enabled.
--
-- DISABLED - Streaming of updates to identity pool is disabled. Bulk publish will also fail if StreamingStatus is DISABLED.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CognitoStreams' value with any optional fields omitted.
mkCognitoStreams
    :: CognitoStreams
mkCognitoStreams
  = CognitoStreams'{roleArn = Core.Nothing,
                    streamName = Core.Nothing, streamingStatus = Core.Nothing}

-- | The ARN of the role Amazon Cognito can assume in order to publish to the stream. This role must grant access to Amazon Cognito (cognito-sync) to invoke PutRecord on your Cognito stream.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csRoleArn :: Lens.Lens' CognitoStreams (Core.Maybe Types.AssumeRoleArn)
csRoleArn = Lens.field @"roleArn"
{-# INLINEABLE csRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | The name of the Cognito stream to receive updates. This stream must be in the developers account and in the same region as the identity pool.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csStreamName :: Lens.Lens' CognitoStreams (Core.Maybe Types.StreamName)
csStreamName = Lens.field @"streamName"
{-# INLINEABLE csStreamName #-}
{-# DEPRECATED streamName "Use generic-lens or generic-optics with 'streamName' instead"  #-}

-- | Status of the Cognito streams. Valid values are: ENABLED - Streaming of updates to identity pool is enabled.
--
-- DISABLED - Streaming of updates to identity pool is disabled. Bulk publish will also fail if StreamingStatus is DISABLED.
--
-- /Note:/ Consider using 'streamingStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csStreamingStatus :: Lens.Lens' CognitoStreams (Core.Maybe Types.StreamingStatus)
csStreamingStatus = Lens.field @"streamingStatus"
{-# INLINEABLE csStreamingStatus #-}
{-# DEPRECATED streamingStatus "Use generic-lens or generic-optics with 'streamingStatus' instead"  #-}

instance Core.FromJSON CognitoStreams where
        toJSON CognitoStreams{..}
          = Core.object
              (Core.catMaybes
                 [("RoleArn" Core..=) Core.<$> roleArn,
                  ("StreamName" Core..=) Core.<$> streamName,
                  ("StreamingStatus" Core..=) Core.<$> streamingStatus])

instance Core.FromJSON CognitoStreams where
        parseJSON
          = Core.withObject "CognitoStreams" Core.$
              \ x ->
                CognitoStreams' Core.<$>
                  (x Core..:? "RoleArn") Core.<*> x Core..:? "StreamName" Core.<*>
                    x Core..:? "StreamingStatus"
