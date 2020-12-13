{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.Message
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.Message
  ( Message (..),

    -- * Smart constructor
    mkMessage,

    -- * Lenses
    mSubject,
    mBody,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SES.Types.Body
import Network.AWS.SES.Types.Content

-- | Represents the message to be sent, composed of a subject and a body.
--
-- /See:/ 'mkMessage' smart constructor.
data Message = Message'
  { -- | The subject of the message: A short summary of the content, which will appear in the recipient's inbox.
    subject :: Content,
    -- | The message body.
    body :: Body
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Message' with the minimum fields required to make a request.
--
-- * 'subject' - The subject of the message: A short summary of the content, which will appear in the recipient's inbox.
-- * 'body' - The message body.
mkMessage ::
  -- | 'subject'
  Content ->
  -- | 'body'
  Body ->
  Message
mkMessage pSubject_ pBody_ =
  Message' {subject = pSubject_, body = pBody_}

-- | The subject of the message: A short summary of the content, which will appear in the recipient's inbox.
--
-- /Note:/ Consider using 'subject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mSubject :: Lens.Lens' Message Content
mSubject = Lens.lens (subject :: Message -> Content) (\s a -> s {subject = a} :: Message)
{-# DEPRECATED mSubject "Use generic-lens or generic-optics with 'subject' instead." #-}

-- | The message body.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mBody :: Lens.Lens' Message Body
mBody = Lens.lens (body :: Message -> Body) (\s a -> s {body = a} :: Message)
{-# DEPRECATED mBody "Use generic-lens or generic-optics with 'body' instead." #-}

instance Lude.ToQuery Message where
  toQuery Message' {..} =
    Lude.mconcat ["Subject" Lude.=: subject, "Body" Lude.=: body]
