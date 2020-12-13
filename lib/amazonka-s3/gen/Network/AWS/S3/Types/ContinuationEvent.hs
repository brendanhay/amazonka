{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ContinuationEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ContinuationEvent
  ( ContinuationEvent (..),

    -- * Smart constructor
    mkContinuationEvent,

    -- * Lenses
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal

-- |
--
-- /See:/ 'mkContinuationEvent' smart constructor.
data ContinuationEvent = ContinuationEvent'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ContinuationEvent' with the minimum fields required to make a request.
mkContinuationEvent ::
  ContinuationEvent
mkContinuationEvent = ContinuationEvent'

instance Lude.FromXML ContinuationEvent where
  parseXML = Lude.const (Lude.pure ContinuationEvent')
