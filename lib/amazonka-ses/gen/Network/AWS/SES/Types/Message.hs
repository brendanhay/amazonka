{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.Message
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SES.Types.Message
  ( Message (..)
  -- * Smart constructor
  , mkMessage
  -- * Lenses
  , mSubject
  , mBody
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SES.Types.Body as Types
import qualified Network.AWS.SES.Types.Content as Types

-- | Represents the message to be sent, composed of a subject and a body.
--
-- /See:/ 'mkMessage' smart constructor.
data Message = Message'
  { subject :: Types.Content
    -- ^ The subject of the message: A short summary of the content, which will appear in the recipient's inbox.
  , body :: Types.Body
    -- ^ The message body.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Message' value with any optional fields omitted.
mkMessage
    :: Types.Content -- ^ 'subject'
    -> Types.Body -- ^ 'body'
    -> Message
mkMessage subject body = Message'{subject, body}

-- | The subject of the message: A short summary of the content, which will appear in the recipient's inbox.
--
-- /Note:/ Consider using 'subject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mSubject :: Lens.Lens' Message Types.Content
mSubject = Lens.field @"subject"
{-# INLINEABLE mSubject #-}
{-# DEPRECATED subject "Use generic-lens or generic-optics with 'subject' instead"  #-}

-- | The message body.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mBody :: Lens.Lens' Message Types.Body
mBody = Lens.field @"body"
{-# INLINEABLE mBody #-}
{-# DEPRECATED body "Use generic-lens or generic-optics with 'body' instead"  #-}

instance Core.ToQuery Message where
        toQuery Message{..}
          = Core.toQueryPair "Subject" subject Core.<>
              Core.toQueryPair "Body" body
