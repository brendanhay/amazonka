{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.DeleteApplications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a list of applications and their associations with configuration items.
module Network.AWS.Discovery.DeleteApplications
  ( -- * Creating a request
    DeleteApplications (..),
    mkDeleteApplications,

    -- ** Request lenses
    daConfigurationIds,

    -- * Destructuring the response
    DeleteApplicationsResponse (..),
    mkDeleteApplicationsResponse,

    -- ** Response lenses
    darsResponseStatus,
  )
where

import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteApplications' smart constructor.
newtype DeleteApplications = DeleteApplications'
  { -- | Configuration ID of an application to be deleted.
    configurationIds :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteApplications' with the minimum fields required to make a request.
--
-- * 'configurationIds' - Configuration ID of an application to be deleted.
mkDeleteApplications ::
  DeleteApplications
mkDeleteApplications =
  DeleteApplications' {configurationIds = Lude.mempty}

-- | Configuration ID of an application to be deleted.
--
-- /Note:/ Consider using 'configurationIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daConfigurationIds :: Lens.Lens' DeleteApplications [Lude.Text]
daConfigurationIds = Lens.lens (configurationIds :: DeleteApplications -> [Lude.Text]) (\s a -> s {configurationIds = a} :: DeleteApplications)
{-# DEPRECATED daConfigurationIds "Use generic-lens or generic-optics with 'configurationIds' instead." #-}

instance Lude.AWSRequest DeleteApplications where
  type Rs DeleteApplications = DeleteApplicationsResponse
  request = Req.postJSON discoveryService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteApplicationsResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteApplications where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSPoseidonService_V2015_11_01.DeleteApplications" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteApplications where
  toJSON DeleteApplications' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("configurationIds" Lude..= configurationIds)]
      )

instance Lude.ToPath DeleteApplications where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteApplications where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteApplicationsResponse' smart constructor.
newtype DeleteApplicationsResponse = DeleteApplicationsResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteApplicationsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteApplicationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteApplicationsResponse
mkDeleteApplicationsResponse pResponseStatus_ =
  DeleteApplicationsResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsResponseStatus :: Lens.Lens' DeleteApplicationsResponse Lude.Int
darsResponseStatus = Lens.lens (responseStatus :: DeleteApplicationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteApplicationsResponse)
{-# DEPRECATED darsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
