{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.ModifyUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes user password(s) and/or access string.
module Network.AWS.ElastiCache.ModifyUser
  ( -- * Creating a request
    ModifyUser (..),
    mkModifyUser,

    -- ** Request lenses
    muUserId,
    muAccessString,
    muAppendAccessString,
    muNoPasswordRequired,
    muPasswords,

    -- * Destructuring the response
    Types.User (..),
    Types.mkUser,

    -- ** Response lenses
    Types.uARN,
    Types.uAccessString,
    Types.uAuthentication,
    Types.uEngine,
    Types.uStatus,
    Types.uUserGroupIds,
    Types.uUserId,
    Types.uUserName,
  )
where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyUser' smart constructor.
data ModifyUser = ModifyUser'
  { -- | The ID of the user.
    userId :: Types.UserId,
    -- | Access permissions string used for this user account.
    accessString :: Core.Maybe Types.AccessString,
    -- | Adds additional user permissions to the access string.
    appendAccessString :: Core.Maybe Types.AccessString,
    -- | Indicates no password is required for the user account.
    noPasswordRequired :: Core.Maybe Core.Bool,
    -- | The passwords belonging to the user account. You are allowed up to two.
    passwords :: Core.Maybe (Core.NonEmpty Types.String)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyUser' value with any optional fields omitted.
mkModifyUser ::
  -- | 'userId'
  Types.UserId ->
  ModifyUser
mkModifyUser userId =
  ModifyUser'
    { userId,
      accessString = Core.Nothing,
      appendAccessString = Core.Nothing,
      noPasswordRequired = Core.Nothing,
      passwords = Core.Nothing
    }

-- | The ID of the user.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
muUserId :: Lens.Lens' ModifyUser Types.UserId
muUserId = Lens.field @"userId"
{-# DEPRECATED muUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | Access permissions string used for this user account.
--
-- /Note:/ Consider using 'accessString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
muAccessString :: Lens.Lens' ModifyUser (Core.Maybe Types.AccessString)
muAccessString = Lens.field @"accessString"
{-# DEPRECATED muAccessString "Use generic-lens or generic-optics with 'accessString' instead." #-}

-- | Adds additional user permissions to the access string.
--
-- /Note:/ Consider using 'appendAccessString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
muAppendAccessString :: Lens.Lens' ModifyUser (Core.Maybe Types.AccessString)
muAppendAccessString = Lens.field @"appendAccessString"
{-# DEPRECATED muAppendAccessString "Use generic-lens or generic-optics with 'appendAccessString' instead." #-}

-- | Indicates no password is required for the user account.
--
-- /Note:/ Consider using 'noPasswordRequired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
muNoPasswordRequired :: Lens.Lens' ModifyUser (Core.Maybe Core.Bool)
muNoPasswordRequired = Lens.field @"noPasswordRequired"
{-# DEPRECATED muNoPasswordRequired "Use generic-lens or generic-optics with 'noPasswordRequired' instead." #-}

-- | The passwords belonging to the user account. You are allowed up to two.
--
-- /Note:/ Consider using 'passwords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
muPasswords :: Lens.Lens' ModifyUser (Core.Maybe (Core.NonEmpty Types.String))
muPasswords = Lens.field @"passwords"
{-# DEPRECATED muPasswords "Use generic-lens or generic-optics with 'passwords' instead." #-}

instance Core.AWSRequest ModifyUser where
  type Rs ModifyUser = Types.User
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ModifyUser")
                Core.<> (Core.pure ("Version", "2015-02-02"))
                Core.<> (Core.toQueryValue "UserId" userId)
                Core.<> (Core.toQueryValue "AccessString" Core.<$> accessString)
                Core.<> ( Core.toQueryValue "AppendAccessString"
                            Core.<$> appendAccessString
                        )
                Core.<> ( Core.toQueryValue "NoPasswordRequired"
                            Core.<$> noPasswordRequired
                        )
                Core.<> ( Core.toQueryValue
                            "Passwords"
                            (Core.toQueryList "member" Core.<$> passwords)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "ModifyUserResult"
      (\s h x -> Core.parseXML x)
