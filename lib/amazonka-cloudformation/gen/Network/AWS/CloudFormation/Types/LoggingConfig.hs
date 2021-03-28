{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.LoggingConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.LoggingConfig
  ( LoggingConfig (..)
  -- * Smart constructor
  , mkLoggingConfig
  -- * Lenses
  , lcLogRoleArn
  , lcLogGroupName
  ) where

import qualified Network.AWS.CloudFormation.Types.LogGroupName as Types
import qualified Network.AWS.CloudFormation.Types.LogRoleArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains logging configuration information for a type.
--
-- /See:/ 'mkLoggingConfig' smart constructor.
data LoggingConfig = LoggingConfig'
  { logRoleArn :: Types.LogRoleArn
    -- ^ The ARN of the role that CloudFormation should assume when sending log entries to CloudWatch logs.
  , logGroupName :: Types.LogGroupName
    -- ^ The Amazon CloudWatch log group to which CloudFormation sends error logging information when invoking the type's handlers.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LoggingConfig' value with any optional fields omitted.
mkLoggingConfig
    :: Types.LogRoleArn -- ^ 'logRoleArn'
    -> Types.LogGroupName -- ^ 'logGroupName'
    -> LoggingConfig
mkLoggingConfig logRoleArn logGroupName
  = LoggingConfig'{logRoleArn, logGroupName}

-- | The ARN of the role that CloudFormation should assume when sending log entries to CloudWatch logs.
--
-- /Note:/ Consider using 'logRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcLogRoleArn :: Lens.Lens' LoggingConfig Types.LogRoleArn
lcLogRoleArn = Lens.field @"logRoleArn"
{-# INLINEABLE lcLogRoleArn #-}
{-# DEPRECATED logRoleArn "Use generic-lens or generic-optics with 'logRoleArn' instead"  #-}

-- | The Amazon CloudWatch log group to which CloudFormation sends error logging information when invoking the type's handlers.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcLogGroupName :: Lens.Lens' LoggingConfig Types.LogGroupName
lcLogGroupName = Lens.field @"logGroupName"
{-# INLINEABLE lcLogGroupName #-}
{-# DEPRECATED logGroupName "Use generic-lens or generic-optics with 'logGroupName' instead"  #-}

instance Core.ToQuery LoggingConfig where
        toQuery LoggingConfig{..}
          = Core.toQueryPair "LogRoleArn" logRoleArn Core.<>
              Core.toQueryPair "LogGroupName" logGroupName

instance Core.FromXML LoggingConfig where
        parseXML x
          = LoggingConfig' Core.<$>
              (x Core..@ "LogRoleArn") Core.<*> x Core..@ "LogGroupName"
