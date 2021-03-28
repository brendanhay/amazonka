{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.Types.PushSync
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoSync.Types.PushSync
  ( PushSync (..)
  -- * Smart constructor
  , mkPushSync
  -- * Lenses
  , psApplicationArns
  , psRoleArn
  ) where

import qualified Network.AWS.CognitoSync.Types.ApplicationArn as Types
import qualified Network.AWS.CognitoSync.Types.AssumeRoleArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Configuration options to be applied to the identity pool.
--
-- /See:/ 'mkPushSync' smart constructor.
data PushSync = PushSync'
  { applicationArns :: Core.Maybe [Types.ApplicationArn]
    -- ^ List of SNS platform application ARNs that could be used by clients.
  , roleArn :: Core.Maybe Types.AssumeRoleArn
    -- ^ A role configured to allow Cognito to call SNS on behalf of the developer.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PushSync' value with any optional fields omitted.
mkPushSync
    :: PushSync
mkPushSync
  = PushSync'{applicationArns = Core.Nothing, roleArn = Core.Nothing}

-- | List of SNS platform application ARNs that could be used by clients.
--
-- /Note:/ Consider using 'applicationArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psApplicationArns :: Lens.Lens' PushSync (Core.Maybe [Types.ApplicationArn])
psApplicationArns = Lens.field @"applicationArns"
{-# INLINEABLE psApplicationArns #-}
{-# DEPRECATED applicationArns "Use generic-lens or generic-optics with 'applicationArns' instead"  #-}

-- | A role configured to allow Cognito to call SNS on behalf of the developer.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psRoleArn :: Lens.Lens' PushSync (Core.Maybe Types.AssumeRoleArn)
psRoleArn = Lens.field @"roleArn"
{-# INLINEABLE psRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

instance Core.FromJSON PushSync where
        toJSON PushSync{..}
          = Core.object
              (Core.catMaybes
                 [("ApplicationArns" Core..=) Core.<$> applicationArns,
                  ("RoleArn" Core..=) Core.<$> roleArn])

instance Core.FromJSON PushSync where
        parseJSON
          = Core.withObject "PushSync" Core.$
              \ x ->
                PushSync' Core.<$>
                  (x Core..:? "ApplicationArns") Core.<*> x Core..:? "RoleArn"
