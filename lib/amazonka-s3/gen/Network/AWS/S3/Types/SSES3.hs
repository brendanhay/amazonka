-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.SSES3
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.SSES3
  ( SSES3 (..),

    -- * Smart constructor
    mkSSES3,

    -- * Lenses
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal

-- | Specifies the use of SSE-S3 to encrypt delivered inventory reports.
--
-- /See:/ 'mkSSES3' smart constructor.
data SSES3 = SSES3'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SSES3' with the minimum fields required to make a request.
mkSSES3 ::
  SSES3
mkSSES3 = SSES3'

instance Lude.FromXML SSES3 where
  parseXML = Lude.const (Lude.pure SSES3')

instance Lude.ToXML SSES3 where
  toXML = Lude.const Lude.mempty
