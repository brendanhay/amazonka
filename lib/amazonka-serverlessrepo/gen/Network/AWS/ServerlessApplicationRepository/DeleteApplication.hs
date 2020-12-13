{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.DeleteApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified application.
module Network.AWS.ServerlessApplicationRepository.DeleteApplication
  ( -- * Creating a request
    DeleteApplication (..),
    mkDeleteApplication,

    -- ** Request lenses
    daApplicationId,

    -- * Destructuring the response
    DeleteApplicationResponse (..),
    mkDeleteApplicationResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServerlessApplicationRepository.Types

-- | /See:/ 'mkDeleteApplication' smart constructor.
newtype DeleteApplication = DeleteApplication'
  { -- | The Amazon Resource Name (ARN) of the application.
    applicationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteApplication' with the minimum fields required to make a request.
--
-- * 'applicationId' - The Amazon Resource Name (ARN) of the application.
mkDeleteApplication ::
  -- | 'applicationId'
  Lude.Text ->
  DeleteApplication
mkDeleteApplication pApplicationId_ =
  DeleteApplication' {applicationId = pApplicationId_}

-- | The Amazon Resource Name (ARN) of the application.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daApplicationId :: Lens.Lens' DeleteApplication Lude.Text
daApplicationId = Lens.lens (applicationId :: DeleteApplication -> Lude.Text) (\s a -> s {applicationId = a} :: DeleteApplication)
{-# DEPRECATED daApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest DeleteApplication where
  type Rs DeleteApplication = DeleteApplicationResponse
  request = Req.delete serverlessApplicationRepositoryService
  response = Res.receiveNull DeleteApplicationResponse'

instance Lude.ToHeaders DeleteApplication where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteApplication where
  toPath DeleteApplication' {..} =
    Lude.mconcat ["/applications/", Lude.toBS applicationId]

instance Lude.ToQuery DeleteApplication where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteApplicationResponse' smart constructor.
data DeleteApplicationResponse = DeleteApplicationResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteApplicationResponse' with the minimum fields required to make a request.
mkDeleteApplicationResponse ::
  DeleteApplicationResponse
mkDeleteApplicationResponse = DeleteApplicationResponse'
