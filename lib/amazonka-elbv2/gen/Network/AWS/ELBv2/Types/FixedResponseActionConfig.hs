{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.FixedResponseActionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.FixedResponseActionConfig
  ( FixedResponseActionConfig (..),

    -- * Smart constructor
    mkFixedResponseActionConfig,

    -- * Lenses
    fracMessageBody,
    fracContentType,
    fracStatusCode,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about an action that returns a custom HTTP response.
--
-- /See:/ 'mkFixedResponseActionConfig' smart constructor.
data FixedResponseActionConfig = FixedResponseActionConfig'
  { messageBody ::
      Lude.Maybe Lude.Text,
    contentType :: Lude.Maybe Lude.Text,
    statusCode :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FixedResponseActionConfig' with the minimum fields required to make a request.
--
-- * 'contentType' - The content type.
--
-- Valid Values: text/plain | text/css | text/html | application/javascript | application/json
-- * 'messageBody' - The message.
-- * 'statusCode' - The HTTP response code (2XX, 4XX, or 5XX).
mkFixedResponseActionConfig ::
  -- | 'statusCode'
  Lude.Text ->
  FixedResponseActionConfig
mkFixedResponseActionConfig pStatusCode_ =
  FixedResponseActionConfig'
    { messageBody = Lude.Nothing,
      contentType = Lude.Nothing,
      statusCode = pStatusCode_
    }

-- | The message.
--
-- /Note:/ Consider using 'messageBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fracMessageBody :: Lens.Lens' FixedResponseActionConfig (Lude.Maybe Lude.Text)
fracMessageBody = Lens.lens (messageBody :: FixedResponseActionConfig -> Lude.Maybe Lude.Text) (\s a -> s {messageBody = a} :: FixedResponseActionConfig)
{-# DEPRECATED fracMessageBody "Use generic-lens or generic-optics with 'messageBody' instead." #-}

-- | The content type.
--
-- Valid Values: text/plain | text/css | text/html | application/javascript | application/json
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fracContentType :: Lens.Lens' FixedResponseActionConfig (Lude.Maybe Lude.Text)
fracContentType = Lens.lens (contentType :: FixedResponseActionConfig -> Lude.Maybe Lude.Text) (\s a -> s {contentType = a} :: FixedResponseActionConfig)
{-# DEPRECATED fracContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

-- | The HTTP response code (2XX, 4XX, or 5XX).
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fracStatusCode :: Lens.Lens' FixedResponseActionConfig Lude.Text
fracStatusCode = Lens.lens (statusCode :: FixedResponseActionConfig -> Lude.Text) (\s a -> s {statusCode = a} :: FixedResponseActionConfig)
{-# DEPRECATED fracStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

instance Lude.FromXML FixedResponseActionConfig where
  parseXML x =
    FixedResponseActionConfig'
      Lude.<$> (x Lude..@? "MessageBody")
      Lude.<*> (x Lude..@? "ContentType")
      Lude.<*> (x Lude..@ "StatusCode")

instance Lude.ToQuery FixedResponseActionConfig where
  toQuery FixedResponseActionConfig' {..} =
    Lude.mconcat
      [ "MessageBody" Lude.=: messageBody,
        "ContentType" Lude.=: contentType,
        "StatusCode" Lude.=: statusCode
      ]
