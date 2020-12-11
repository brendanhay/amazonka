{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DeleteUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For Redis engine version 6.x onwards: Deletes a user. The user will be removed from all user groups and in turn removed from all replication groups. For more information, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Clusters.RBAC.html Using Role Based Access Control (RBAC)> .
module Network.AWS.ElastiCache.DeleteUser
  ( -- * Creating a request
    DeleteUser (..),
    mkDeleteUser,

    -- ** Request lenses
    dUserId,

    -- * Destructuring the response
    User (..),
    mkUser,

    -- ** Response lenses
    uStatus,
    uARN,
    uUserGroupIds,
    uAuthentication,
    uEngine,
    uUserName,
    uAccessString,
    uUserId,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteUser' smart constructor.
newtype DeleteUser = DeleteUser' {userId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteUser' with the minimum fields required to make a request.
--
-- * 'userId' - The ID of the user.
mkDeleteUser ::
  -- | 'userId'
  Lude.Text ->
  DeleteUser
mkDeleteUser pUserId_ = DeleteUser' {userId = pUserId_}

-- | The ID of the user.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dUserId :: Lens.Lens' DeleteUser Lude.Text
dUserId = Lens.lens (userId :: DeleteUser -> Lude.Text) (\s a -> s {userId = a} :: DeleteUser)
{-# DEPRECATED dUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

instance Lude.AWSRequest DeleteUser where
  type Rs DeleteUser = User
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "DeleteUserResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders DeleteUser where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteUser where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteUser where
  toQuery DeleteUser' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteUser" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "UserId" Lude.=: userId
      ]
