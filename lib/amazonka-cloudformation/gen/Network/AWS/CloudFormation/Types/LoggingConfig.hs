-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.LoggingConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.LoggingConfig
  ( LoggingConfig (..),

    -- * Smart constructor
    mkLoggingConfig,

    -- * Lenses
    lcLogRoleARN,
    lcLogGroupName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains logging configuration information for a type.
--
-- /See:/ 'mkLoggingConfig' smart constructor.
data LoggingConfig = LoggingConfig'
  { logRoleARN :: Lude.Text,
    logGroupName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LoggingConfig' with the minimum fields required to make a request.
--
-- * 'logGroupName' - The Amazon CloudWatch log group to which CloudFormation sends error logging information when invoking the type's handlers.
-- * 'logRoleARN' - The ARN of the role that CloudFormation should assume when sending log entries to CloudWatch logs.
mkLoggingConfig ::
  -- | 'logRoleARN'
  Lude.Text ->
  -- | 'logGroupName'
  Lude.Text ->
  LoggingConfig
mkLoggingConfig pLogRoleARN_ pLogGroupName_ =
  LoggingConfig'
    { logRoleARN = pLogRoleARN_,
      logGroupName = pLogGroupName_
    }

-- | The ARN of the role that CloudFormation should assume when sending log entries to CloudWatch logs.
--
-- /Note:/ Consider using 'logRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcLogRoleARN :: Lens.Lens' LoggingConfig Lude.Text
lcLogRoleARN = Lens.lens (logRoleARN :: LoggingConfig -> Lude.Text) (\s a -> s {logRoleARN = a} :: LoggingConfig)
{-# DEPRECATED lcLogRoleARN "Use generic-lens or generic-optics with 'logRoleARN' instead." #-}

-- | The Amazon CloudWatch log group to which CloudFormation sends error logging information when invoking the type's handlers.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcLogGroupName :: Lens.Lens' LoggingConfig Lude.Text
lcLogGroupName = Lens.lens (logGroupName :: LoggingConfig -> Lude.Text) (\s a -> s {logGroupName = a} :: LoggingConfig)
{-# DEPRECATED lcLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

instance Lude.FromXML LoggingConfig where
  parseXML x =
    LoggingConfig'
      Lude.<$> (x Lude..@ "LogRoleArn") Lude.<*> (x Lude..@ "LogGroupName")

instance Lude.ToQuery LoggingConfig where
  toQuery LoggingConfig' {..} =
    Lude.mconcat
      [ "LogRoleArn" Lude.=: logRoleARN,
        "LogGroupName" Lude.=: logGroupName
      ]
