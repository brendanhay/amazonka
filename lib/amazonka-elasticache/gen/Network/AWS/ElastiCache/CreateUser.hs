{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.CreateUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For Redis engine version 6.x onwards: Creates a Redis user. For more information, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Clusters.RBAC.html Using Role Based Access Control (RBAC)> .
module Network.AWS.ElastiCache.CreateUser
  ( -- * Creating a request
    CreateUser (..),
    mkCreateUser,

    -- ** Request lenses
    cuUserId,
    cuUserName,
    cuEngine,
    cuAccessString,
    cuNoPasswordRequired,
    cuPasswords,

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

-- | /See:/ 'mkCreateUser' smart constructor.
data CreateUser = CreateUser'
  { -- | The ID of the user.
    userId :: Types.UserId,
    -- | The username of the user.
    userName :: Types.UserName,
    -- | Must be Redis.
    engine :: Types.Engine,
    -- | Access permissions string used for this user account.
    accessString :: Types.AccessString,
    -- | Indicates a password is not required for this user account.
    noPasswordRequired :: Core.Maybe Core.Bool,
    -- | Passwords used for this user account. You can create up to two passwords for each user.
    passwords :: Core.Maybe (Core.NonEmpty Types.String)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateUser' value with any optional fields omitted.
mkCreateUser ::
  -- | 'userId'
  Types.UserId ->
  -- | 'userName'
  Types.UserName ->
  -- | 'engine'
  Types.Engine ->
  -- | 'accessString'
  Types.AccessString ->
  CreateUser
mkCreateUser userId userName engine accessString =
  CreateUser'
    { userId,
      userName,
      engine,
      accessString,
      noPasswordRequired = Core.Nothing,
      passwords = Core.Nothing
    }

-- | The ID of the user.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuUserId :: Lens.Lens' CreateUser Types.UserId
cuUserId = Lens.field @"userId"
{-# DEPRECATED cuUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The username of the user.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuUserName :: Lens.Lens' CreateUser Types.UserName
cuUserName = Lens.field @"userName"
{-# DEPRECATED cuUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | Must be Redis.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuEngine :: Lens.Lens' CreateUser Types.Engine
cuEngine = Lens.field @"engine"
{-# DEPRECATED cuEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | Access permissions string used for this user account.
--
-- /Note:/ Consider using 'accessString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuAccessString :: Lens.Lens' CreateUser Types.AccessString
cuAccessString = Lens.field @"accessString"
{-# DEPRECATED cuAccessString "Use generic-lens or generic-optics with 'accessString' instead." #-}

-- | Indicates a password is not required for this user account.
--
-- /Note:/ Consider using 'noPasswordRequired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuNoPasswordRequired :: Lens.Lens' CreateUser (Core.Maybe Core.Bool)
cuNoPasswordRequired = Lens.field @"noPasswordRequired"
{-# DEPRECATED cuNoPasswordRequired "Use generic-lens or generic-optics with 'noPasswordRequired' instead." #-}

-- | Passwords used for this user account. You can create up to two passwords for each user.
--
-- /Note:/ Consider using 'passwords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuPasswords :: Lens.Lens' CreateUser (Core.Maybe (Core.NonEmpty Types.String))
cuPasswords = Lens.field @"passwords"
{-# DEPRECATED cuPasswords "Use generic-lens or generic-optics with 'passwords' instead." #-}

instance Core.AWSRequest CreateUser where
  type Rs CreateUser = Types.User
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
            ( Core.pure ("Action", "CreateUser")
                Core.<> (Core.pure ("Version", "2015-02-02"))
                Core.<> (Core.toQueryValue "UserId" userId)
                Core.<> (Core.toQueryValue "UserName" userName)
                Core.<> (Core.toQueryValue "Engine" engine)
                Core.<> (Core.toQueryValue "AccessString" accessString)
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
      "CreateUserResult"
      (\s h x -> Core.parseXML x)
