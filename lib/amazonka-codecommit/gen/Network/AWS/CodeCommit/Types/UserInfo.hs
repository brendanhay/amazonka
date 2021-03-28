{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.UserInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeCommit.Types.UserInfo
  ( UserInfo (..)
  -- * Smart constructor
  , mkUserInfo
  -- * Lenses
  , uiDate
  , uiEmail
  , uiName
  ) where

import qualified Network.AWS.CodeCommit.Types.Date as Types
import qualified Network.AWS.CodeCommit.Types.Email as Types
import qualified Network.AWS.CodeCommit.Types.Name as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the user who made a specified commit.
--
-- /See:/ 'mkUserInfo' smart constructor.
data UserInfo = UserInfo'
  { date :: Core.Maybe Types.Date
    -- ^ The date when the specified commit was commited, in timestamp format with GMT offset.
  , email :: Core.Maybe Types.Email
    -- ^ The email address associated with the user who made the commit, if any.
  , name :: Core.Maybe Types.Name
    -- ^ The name of the user who made the specified commit.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UserInfo' value with any optional fields omitted.
mkUserInfo
    :: UserInfo
mkUserInfo
  = UserInfo'{date = Core.Nothing, email = Core.Nothing,
              name = Core.Nothing}

-- | The date when the specified commit was commited, in timestamp format with GMT offset.
--
-- /Note:/ Consider using 'date' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiDate :: Lens.Lens' UserInfo (Core.Maybe Types.Date)
uiDate = Lens.field @"date"
{-# INLINEABLE uiDate #-}
{-# DEPRECATED date "Use generic-lens or generic-optics with 'date' instead"  #-}

-- | The email address associated with the user who made the commit, if any.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiEmail :: Lens.Lens' UserInfo (Core.Maybe Types.Email)
uiEmail = Lens.field @"email"
{-# INLINEABLE uiEmail #-}
{-# DEPRECATED email "Use generic-lens or generic-optics with 'email' instead"  #-}

-- | The name of the user who made the specified commit.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiName :: Lens.Lens' UserInfo (Core.Maybe Types.Name)
uiName = Lens.field @"name"
{-# INLINEABLE uiName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON UserInfo where
        parseJSON
          = Core.withObject "UserInfo" Core.$
              \ x ->
                UserInfo' Core.<$>
                  (x Core..:? "date") Core.<*> x Core..:? "email" Core.<*>
                    x Core..:? "name"
