-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.LoggingOptionsPayload
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.LoggingOptionsPayload
  ( LoggingOptionsPayload (..),

    -- * Smart constructor
    mkLoggingOptionsPayload,

    -- * Lenses
    lopLogLevel,
    lopRoleARN,
  )
where

import Network.AWS.IoT.Types.LogLevel
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the logging options payload.
--
-- /See:/ 'mkLoggingOptionsPayload' smart constructor.
data LoggingOptionsPayload = LoggingOptionsPayload'
  { logLevel ::
      Lude.Maybe LogLevel,
    roleARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LoggingOptionsPayload' with the minimum fields required to make a request.
--
-- * 'logLevel' - The log level.
-- * 'roleARN' - The ARN of the IAM role that grants access.
mkLoggingOptionsPayload ::
  -- | 'roleARN'
  Lude.Text ->
  LoggingOptionsPayload
mkLoggingOptionsPayload pRoleARN_ =
  LoggingOptionsPayload'
    { logLevel = Lude.Nothing,
      roleARN = pRoleARN_
    }

-- | The log level.
--
-- /Note:/ Consider using 'logLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lopLogLevel :: Lens.Lens' LoggingOptionsPayload (Lude.Maybe LogLevel)
lopLogLevel = Lens.lens (logLevel :: LoggingOptionsPayload -> Lude.Maybe LogLevel) (\s a -> s {logLevel = a} :: LoggingOptionsPayload)
{-# DEPRECATED lopLogLevel "Use generic-lens or generic-optics with 'logLevel' instead." #-}

-- | The ARN of the IAM role that grants access.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lopRoleARN :: Lens.Lens' LoggingOptionsPayload Lude.Text
lopRoleARN = Lens.lens (roleARN :: LoggingOptionsPayload -> Lude.Text) (\s a -> s {roleARN = a} :: LoggingOptionsPayload)
{-# DEPRECATED lopRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.ToJSON LoggingOptionsPayload where
  toJSON LoggingOptionsPayload' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("logLevel" Lude..=) Lude.<$> logLevel,
            Lude.Just ("roleArn" Lude..= roleARN)
          ]
      )
