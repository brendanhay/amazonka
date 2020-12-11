{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.DeleteFieldLevelEncryptionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove a field-level encryption configuration.
module Network.AWS.CloudFront.DeleteFieldLevelEncryptionConfig
  ( -- * Creating a request
    DeleteFieldLevelEncryptionConfig (..),
    mkDeleteFieldLevelEncryptionConfig,

    -- ** Request lenses
    dflecIfMatch,
    dflecId,

    -- * Destructuring the response
    DeleteFieldLevelEncryptionConfigResponse (..),
    mkDeleteFieldLevelEncryptionConfigResponse,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteFieldLevelEncryptionConfig' smart constructor.
data DeleteFieldLevelEncryptionConfig = DeleteFieldLevelEncryptionConfig'
  { ifMatch ::
      Lude.Maybe Lude.Text,
    id :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteFieldLevelEncryptionConfig' with the minimum fields required to make a request.
--
-- * 'id' - The ID of the configuration you want to delete from CloudFront.
-- * 'ifMatch' - The value of the @ETag@ header that you received when retrieving the configuration identity to delete. For example: @E2QWRUHAPOMQZL@ .
mkDeleteFieldLevelEncryptionConfig ::
  -- | 'id'
  Lude.Text ->
  DeleteFieldLevelEncryptionConfig
mkDeleteFieldLevelEncryptionConfig pId_ =
  DeleteFieldLevelEncryptionConfig'
    { ifMatch = Lude.Nothing,
      id = pId_
    }

-- | The value of the @ETag@ header that you received when retrieving the configuration identity to delete. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'ifMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dflecIfMatch :: Lens.Lens' DeleteFieldLevelEncryptionConfig (Lude.Maybe Lude.Text)
dflecIfMatch = Lens.lens (ifMatch :: DeleteFieldLevelEncryptionConfig -> Lude.Maybe Lude.Text) (\s a -> s {ifMatch = a} :: DeleteFieldLevelEncryptionConfig)
{-# DEPRECATED dflecIfMatch "Use generic-lens or generic-optics with 'ifMatch' instead." #-}

-- | The ID of the configuration you want to delete from CloudFront.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dflecId :: Lens.Lens' DeleteFieldLevelEncryptionConfig Lude.Text
dflecId = Lens.lens (id :: DeleteFieldLevelEncryptionConfig -> Lude.Text) (\s a -> s {id = a} :: DeleteFieldLevelEncryptionConfig)
{-# DEPRECATED dflecId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest DeleteFieldLevelEncryptionConfig where
  type
    Rs DeleteFieldLevelEncryptionConfig =
      DeleteFieldLevelEncryptionConfigResponse
  request = Req.delete cloudFrontService
  response =
    Res.receiveNull DeleteFieldLevelEncryptionConfigResponse'

instance Lude.ToHeaders DeleteFieldLevelEncryptionConfig where
  toHeaders DeleteFieldLevelEncryptionConfig' {..} =
    Lude.mconcat ["If-Match" Lude.=# ifMatch]

instance Lude.ToPath DeleteFieldLevelEncryptionConfig where
  toPath DeleteFieldLevelEncryptionConfig' {..} =
    Lude.mconcat
      ["/2020-05-31/field-level-encryption/", Lude.toBS id]

instance Lude.ToQuery DeleteFieldLevelEncryptionConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteFieldLevelEncryptionConfigResponse' smart constructor.
data DeleteFieldLevelEncryptionConfigResponse = DeleteFieldLevelEncryptionConfigResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteFieldLevelEncryptionConfigResponse' with the minimum fields required to make a request.
mkDeleteFieldLevelEncryptionConfigResponse ::
  DeleteFieldLevelEncryptionConfigResponse
mkDeleteFieldLevelEncryptionConfigResponse =
  DeleteFieldLevelEncryptionConfigResponse'
