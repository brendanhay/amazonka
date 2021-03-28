{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.SSES3
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.SSES3
  ( SSES3 (..)
  -- * Smart constructor
  , mkSSES3
  -- * Lenses
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types

-- | Specifies the use of SSE-S3 to encrypt delivered inventory reports.
--
-- /See:/ 'mkSSES3' smart constructor.
data SSES3 = SSES3'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SSES3' value with any optional fields omitted.
mkSSES3
    :: SSES3
mkSSES3 = SSES3'

instance Core.ToXML SSES3 where
        toXML _ = Core.mempty

instance Core.FromXML SSES3 where
        parseXML x = Core.pure SSES3'
