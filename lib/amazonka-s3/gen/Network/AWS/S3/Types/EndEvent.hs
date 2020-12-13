{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.EndEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.EndEvent
  ( EndEvent (..),

    -- * Smart constructor
    mkEndEvent,

    -- * Lenses
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal

-- | A message that indicates the request is complete and no more messages will be sent. You should not assume that the request is complete until the client receives an @EndEvent@ .
--
-- /See:/ 'mkEndEvent' smart constructor.
data EndEvent = EndEvent'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EndEvent' with the minimum fields required to make a request.
mkEndEvent ::
  EndEvent
mkEndEvent = EndEvent'

instance Lude.FromXML EndEvent where
  parseXML = Lude.const (Lude.pure EndEvent')
