{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.MessageData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.MessageData
  ( MessageData (..),

    -- * Smart constructor
    mkMessageData,

    -- * Lenses
    mValue,
    mCode,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A message returned by the @GetMetricData@ API, including a code and a description.
--
-- /See:/ 'mkMessageData' smart constructor.
data MessageData = MessageData'
  { value :: Lude.Maybe Lude.Text,
    code :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MessageData' with the minimum fields required to make a request.
--
-- * 'code' - The error code or status code associated with the message.
-- * 'value' - The message text.
mkMessageData ::
  MessageData
mkMessageData =
  MessageData' {value = Lude.Nothing, code = Lude.Nothing}

-- | The message text.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mValue :: Lens.Lens' MessageData (Lude.Maybe Lude.Text)
mValue = Lens.lens (value :: MessageData -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: MessageData)
{-# DEPRECATED mValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The error code or status code associated with the message.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mCode :: Lens.Lens' MessageData (Lude.Maybe Lude.Text)
mCode = Lens.lens (code :: MessageData -> Lude.Maybe Lude.Text) (\s a -> s {code = a} :: MessageData)
{-# DEPRECATED mCode "Use generic-lens or generic-optics with 'code' instead." #-}

instance Lude.FromXML MessageData where
  parseXML x =
    MessageData'
      Lude.<$> (x Lude..@? "Value") Lude.<*> (x Lude..@? "Code")
