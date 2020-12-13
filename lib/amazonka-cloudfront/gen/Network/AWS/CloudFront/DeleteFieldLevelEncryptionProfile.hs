{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.DeleteFieldLevelEncryptionProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove a field-level encryption profile.
module Network.AWS.CloudFront.DeleteFieldLevelEncryptionProfile
  ( -- * Creating a request
    DeleteFieldLevelEncryptionProfile (..),
    mkDeleteFieldLevelEncryptionProfile,

    -- ** Request lenses
    dflepIfMatch,
    dflepId,

    -- * Destructuring the response
    DeleteFieldLevelEncryptionProfileResponse (..),
    mkDeleteFieldLevelEncryptionProfileResponse,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteFieldLevelEncryptionProfile' smart constructor.
data DeleteFieldLevelEncryptionProfile = DeleteFieldLevelEncryptionProfile'
  { -- | The value of the @ETag@ header that you received when retrieving the profile to delete. For example: @E2QWRUHAPOMQZL@ .
    ifMatch :: Lude.Maybe Lude.Text,
    -- | Request the ID of the profile you want to delete from CloudFront.
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteFieldLevelEncryptionProfile' with the minimum fields required to make a request.
--
-- * 'ifMatch' - The value of the @ETag@ header that you received when retrieving the profile to delete. For example: @E2QWRUHAPOMQZL@ .
-- * 'id' - Request the ID of the profile you want to delete from CloudFront.
mkDeleteFieldLevelEncryptionProfile ::
  -- | 'id'
  Lude.Text ->
  DeleteFieldLevelEncryptionProfile
mkDeleteFieldLevelEncryptionProfile pId_ =
  DeleteFieldLevelEncryptionProfile'
    { ifMatch = Lude.Nothing,
      id = pId_
    }

-- | The value of the @ETag@ header that you received when retrieving the profile to delete. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'ifMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dflepIfMatch :: Lens.Lens' DeleteFieldLevelEncryptionProfile (Lude.Maybe Lude.Text)
dflepIfMatch = Lens.lens (ifMatch :: DeleteFieldLevelEncryptionProfile -> Lude.Maybe Lude.Text) (\s a -> s {ifMatch = a} :: DeleteFieldLevelEncryptionProfile)
{-# DEPRECATED dflepIfMatch "Use generic-lens or generic-optics with 'ifMatch' instead." #-}

-- | Request the ID of the profile you want to delete from CloudFront.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dflepId :: Lens.Lens' DeleteFieldLevelEncryptionProfile Lude.Text
dflepId = Lens.lens (id :: DeleteFieldLevelEncryptionProfile -> Lude.Text) (\s a -> s {id = a} :: DeleteFieldLevelEncryptionProfile)
{-# DEPRECATED dflepId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest DeleteFieldLevelEncryptionProfile where
  type
    Rs DeleteFieldLevelEncryptionProfile =
      DeleteFieldLevelEncryptionProfileResponse
  request = Req.delete cloudFrontService
  response =
    Res.receiveNull DeleteFieldLevelEncryptionProfileResponse'

instance Lude.ToHeaders DeleteFieldLevelEncryptionProfile where
  toHeaders DeleteFieldLevelEncryptionProfile' {..} =
    Lude.mconcat ["If-Match" Lude.=# ifMatch]

instance Lude.ToPath DeleteFieldLevelEncryptionProfile where
  toPath DeleteFieldLevelEncryptionProfile' {..} =
    Lude.mconcat
      ["/2020-05-31/field-level-encryption-profile/", Lude.toBS id]

instance Lude.ToQuery DeleteFieldLevelEncryptionProfile where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteFieldLevelEncryptionProfileResponse' smart constructor.
data DeleteFieldLevelEncryptionProfileResponse = DeleteFieldLevelEncryptionProfileResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteFieldLevelEncryptionProfileResponse' with the minimum fields required to make a request.
mkDeleteFieldLevelEncryptionProfileResponse ::
  DeleteFieldLevelEncryptionProfileResponse
mkDeleteFieldLevelEncryptionProfileResponse =
  DeleteFieldLevelEncryptionProfileResponse'
