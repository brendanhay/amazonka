{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.UserData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.UserData
  ( UserData (..)
  -- * Smart constructor
  , mkUserData
  -- * Lenses
  , udData
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the user data for an instance.
--
-- /See:/ 'mkUserData' smart constructor.
newtype UserData = UserData'
  { data' :: Core.Maybe Core.Text
    -- ^ The user data. If you are using an AWS SDK or command line tool, Base64-encoding is performed for you, and you can load the text from a file. Otherwise, you must provide Base64-encoded text.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UserData' value with any optional fields omitted.
mkUserData
    :: UserData
mkUserData = UserData'{data' = Core.Nothing}

-- | The user data. If you are using an AWS SDK or command line tool, Base64-encoding is performed for you, and you can load the text from a file. Otherwise, you must provide Base64-encoded text.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udData :: Lens.Lens' UserData (Core.Maybe Core.Text)
udData = Lens.field @"data'"
{-# INLINEABLE udData #-}
{-# DEPRECATED data' "Use generic-lens or generic-optics with 'data'' instead"  #-}

instance Core.ToQuery UserData where
        toQuery UserData{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Data") data'
