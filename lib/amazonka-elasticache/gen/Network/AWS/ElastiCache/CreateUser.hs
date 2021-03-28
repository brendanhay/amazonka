{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateUser (..)
    , mkCreateUser
    -- ** Request lenses
    , cuUserId
    , cuUserName
    , cuEngine
    , cuAccessString
    , cuNoPasswordRequired
    , cuPasswords

     -- * Destructuring the response
    , Types.User (..)
    , Types.mkUser
    -- ** Response lenses
    , Types.uARN
    , Types.uAccessString
    , Types.uAuthentication
    , Types.uEngine
    , Types.uStatus
    , Types.uUserGroupIds
    , Types.uUserId
    , Types.uUserName
    ) where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateUser' smart constructor.
data CreateUser = CreateUser'
  { userId :: Types.UserId
    -- ^ The ID of the user.
  , userName :: Types.UserName
    -- ^ The username of the user.
  , engine :: Types.Engine
    -- ^ Must be Redis. 
  , accessString :: Types.AccessString
    -- ^ Access permissions string used for this user account.
  , noPasswordRequired :: Core.Maybe Core.Bool
    -- ^ Indicates a password is not required for this user account.
  , passwords :: Core.Maybe (Core.NonEmpty Core.Text)
    -- ^ Passwords used for this user account. You can create up to two passwords for each user.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateUser' value with any optional fields omitted.
mkCreateUser
    :: Types.UserId -- ^ 'userId'
    -> Types.UserName -- ^ 'userName'
    -> Types.Engine -- ^ 'engine'
    -> Types.AccessString -- ^ 'accessString'
    -> CreateUser
mkCreateUser userId userName engine accessString
  = CreateUser'{userId, userName, engine, accessString,
                noPasswordRequired = Core.Nothing, passwords = Core.Nothing}

-- | The ID of the user.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuUserId :: Lens.Lens' CreateUser Types.UserId
cuUserId = Lens.field @"userId"
{-# INLINEABLE cuUserId #-}
{-# DEPRECATED userId "Use generic-lens or generic-optics with 'userId' instead"  #-}

-- | The username of the user.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuUserName :: Lens.Lens' CreateUser Types.UserName
cuUserName = Lens.field @"userName"
{-# INLINEABLE cuUserName #-}
{-# DEPRECATED userName "Use generic-lens or generic-optics with 'userName' instead"  #-}

-- | Must be Redis. 
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuEngine :: Lens.Lens' CreateUser Types.Engine
cuEngine = Lens.field @"engine"
{-# INLINEABLE cuEngine #-}
{-# DEPRECATED engine "Use generic-lens or generic-optics with 'engine' instead"  #-}

-- | Access permissions string used for this user account.
--
-- /Note:/ Consider using 'accessString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuAccessString :: Lens.Lens' CreateUser Types.AccessString
cuAccessString = Lens.field @"accessString"
{-# INLINEABLE cuAccessString #-}
{-# DEPRECATED accessString "Use generic-lens or generic-optics with 'accessString' instead"  #-}

-- | Indicates a password is not required for this user account.
--
-- /Note:/ Consider using 'noPasswordRequired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuNoPasswordRequired :: Lens.Lens' CreateUser (Core.Maybe Core.Bool)
cuNoPasswordRequired = Lens.field @"noPasswordRequired"
{-# INLINEABLE cuNoPasswordRequired #-}
{-# DEPRECATED noPasswordRequired "Use generic-lens or generic-optics with 'noPasswordRequired' instead"  #-}

-- | Passwords used for this user account. You can create up to two passwords for each user.
--
-- /Note:/ Consider using 'passwords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuPasswords :: Lens.Lens' CreateUser (Core.Maybe (Core.NonEmpty Core.Text))
cuPasswords = Lens.field @"passwords"
{-# INLINEABLE cuPasswords #-}
{-# DEPRECATED passwords "Use generic-lens or generic-optics with 'passwords' instead"  #-}

instance Core.ToQuery CreateUser where
        toQuery CreateUser{..}
          = Core.toQueryPair "Action" ("CreateUser" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2015-02-02" :: Core.Text)
              Core.<> Core.toQueryPair "UserId" userId
              Core.<> Core.toQueryPair "UserName" userName
              Core.<> Core.toQueryPair "Engine" engine
              Core.<> Core.toQueryPair "AccessString" accessString
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NoPasswordRequired")
                noPasswordRequired
              Core.<>
              Core.toQueryPair "Passwords"
                (Core.maybe Core.mempty (Core.toQueryList "member") passwords)

instance Core.ToHeaders CreateUser where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateUser where
        type Rs CreateUser = Types.User
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "CreateUserResult"
              (\ s h x -> Core.parseXML x)
        
        {-# INLINE parseResponse #-}
