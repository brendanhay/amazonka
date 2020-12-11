{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.SetV2LoggingOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the logging options for the V2 logging service.
module Network.AWS.IoT.SetV2LoggingOptions
  ( -- * Creating a request
    SetV2LoggingOptions (..),
    mkSetV2LoggingOptions,

    -- ** Request lenses
    svloDisableAllLogs,
    svloDefaultLogLevel,
    svloRoleARN,

    -- * Destructuring the response
    SetV2LoggingOptionsResponse (..),
    mkSetV2LoggingOptionsResponse,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSetV2LoggingOptions' smart constructor.
data SetV2LoggingOptions = SetV2LoggingOptions'
  { disableAllLogs ::
      Lude.Maybe Lude.Bool,
    defaultLogLevel :: Lude.Maybe LogLevel,
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

-- | Creates a value of 'SetV2LoggingOptions' with the minimum fields required to make a request.
--
-- * 'defaultLogLevel' - The default logging level.
-- * 'disableAllLogs' - If true all logs are disabled. The default is false.
-- * 'roleARN' - The ARN of the role that allows IoT to write to Cloudwatch logs.
mkSetV2LoggingOptions ::
  SetV2LoggingOptions
mkSetV2LoggingOptions =
  SetV2LoggingOptions'
    { disableAllLogs = Lude.Nothing,
      defaultLogLevel = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | If true all logs are disabled. The default is false.
--
-- /Note:/ Consider using 'disableAllLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svloDisableAllLogs :: Lens.Lens' SetV2LoggingOptions (Lude.Maybe Lude.Bool)
svloDisableAllLogs = Lens.lens (disableAllLogs :: SetV2LoggingOptions -> Lude.Maybe Lude.Bool) (\s a -> s {disableAllLogs = a} :: SetV2LoggingOptions)
{-# DEPRECATED svloDisableAllLogs "Use generic-lens or generic-optics with 'disableAllLogs' instead." #-}

-- | The default logging level.
--
-- /Note:/ Consider using 'defaultLogLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svloDefaultLogLevel :: Lens.Lens' SetV2LoggingOptions (Lude.Maybe LogLevel)
svloDefaultLogLevel = Lens.lens (defaultLogLevel :: SetV2LoggingOptions -> Lude.Maybe LogLevel) (\s a -> s {defaultLogLevel = a} :: SetV2LoggingOptions)
{-# DEPRECATED svloDefaultLogLevel "Use generic-lens or generic-optics with 'defaultLogLevel' instead." #-}

-- | The ARN of the role that allows IoT to write to Cloudwatch logs.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svloRoleARN :: Lens.Lens' SetV2LoggingOptions (Lude.Maybe Lude.Text)
svloRoleARN = Lens.lens (roleARN :: SetV2LoggingOptions -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: SetV2LoggingOptions)
{-# DEPRECATED svloRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest SetV2LoggingOptions where
  type Rs SetV2LoggingOptions = SetV2LoggingOptionsResponse
  request = Req.postJSON ioTService
  response = Res.receiveNull SetV2LoggingOptionsResponse'

instance Lude.ToHeaders SetV2LoggingOptions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON SetV2LoggingOptions where
  toJSON SetV2LoggingOptions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("disableAllLogs" Lude..=) Lude.<$> disableAllLogs,
            ("defaultLogLevel" Lude..=) Lude.<$> defaultLogLevel,
            ("roleArn" Lude..=) Lude.<$> roleARN
          ]
      )

instance Lude.ToPath SetV2LoggingOptions where
  toPath = Lude.const "/v2LoggingOptions"

instance Lude.ToQuery SetV2LoggingOptions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSetV2LoggingOptionsResponse' smart constructor.
data SetV2LoggingOptionsResponse = SetV2LoggingOptionsResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetV2LoggingOptionsResponse' with the minimum fields required to make a request.
mkSetV2LoggingOptionsResponse ::
  SetV2LoggingOptionsResponse
mkSetV2LoggingOptionsResponse = SetV2LoggingOptionsResponse'
