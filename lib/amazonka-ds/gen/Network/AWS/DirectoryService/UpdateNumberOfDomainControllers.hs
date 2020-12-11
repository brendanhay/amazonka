{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.UpdateNumberOfDomainControllers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or removes domain controllers to or from the directory. Based on the difference between current value and new value (provided through this API call), domain controllers will be added or removed. It may take up to 45 minutes for any new domain controllers to become fully active once the requested number of domain controllers is updated. During this time, you cannot make another update request.
module Network.AWS.DirectoryService.UpdateNumberOfDomainControllers
  ( -- * Creating a request
    UpdateNumberOfDomainControllers (..),
    mkUpdateNumberOfDomainControllers,

    -- ** Request lenses
    unodcDirectoryId,
    unodcDesiredNumber,

    -- * Destructuring the response
    UpdateNumberOfDomainControllersResponse (..),
    mkUpdateNumberOfDomainControllersResponse,

    -- ** Response lenses
    unodcrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateNumberOfDomainControllers' smart constructor.
data UpdateNumberOfDomainControllers = UpdateNumberOfDomainControllers'
  { directoryId ::
      Lude.Text,
    desiredNumber ::
      Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateNumberOfDomainControllers' with the minimum fields required to make a request.
--
-- * 'desiredNumber' - The number of domain controllers desired in the directory.
-- * 'directoryId' - Identifier of the directory to which the domain controllers will be added or removed.
mkUpdateNumberOfDomainControllers ::
  -- | 'directoryId'
  Lude.Text ->
  -- | 'desiredNumber'
  Lude.Natural ->
  UpdateNumberOfDomainControllers
mkUpdateNumberOfDomainControllers pDirectoryId_ pDesiredNumber_ =
  UpdateNumberOfDomainControllers'
    { directoryId = pDirectoryId_,
      desiredNumber = pDesiredNumber_
    }

-- | Identifier of the directory to which the domain controllers will be added or removed.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unodcDirectoryId :: Lens.Lens' UpdateNumberOfDomainControllers Lude.Text
unodcDirectoryId = Lens.lens (directoryId :: UpdateNumberOfDomainControllers -> Lude.Text) (\s a -> s {directoryId = a} :: UpdateNumberOfDomainControllers)
{-# DEPRECATED unodcDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The number of domain controllers desired in the directory.
--
-- /Note:/ Consider using 'desiredNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unodcDesiredNumber :: Lens.Lens' UpdateNumberOfDomainControllers Lude.Natural
unodcDesiredNumber = Lens.lens (desiredNumber :: UpdateNumberOfDomainControllers -> Lude.Natural) (\s a -> s {desiredNumber = a} :: UpdateNumberOfDomainControllers)
{-# DEPRECATED unodcDesiredNumber "Use generic-lens or generic-optics with 'desiredNumber' instead." #-}

instance Lude.AWSRequest UpdateNumberOfDomainControllers where
  type
    Rs UpdateNumberOfDomainControllers =
      UpdateNumberOfDomainControllersResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateNumberOfDomainControllersResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateNumberOfDomainControllers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "DirectoryService_20150416.UpdateNumberOfDomainControllers" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateNumberOfDomainControllers where
  toJSON UpdateNumberOfDomainControllers' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DirectoryId" Lude..= directoryId),
            Lude.Just ("DesiredNumber" Lude..= desiredNumber)
          ]
      )

instance Lude.ToPath UpdateNumberOfDomainControllers where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateNumberOfDomainControllers where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateNumberOfDomainControllersResponse' smart constructor.
newtype UpdateNumberOfDomainControllersResponse = UpdateNumberOfDomainControllersResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateNumberOfDomainControllersResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateNumberOfDomainControllersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateNumberOfDomainControllersResponse
mkUpdateNumberOfDomainControllersResponse pResponseStatus_ =
  UpdateNumberOfDomainControllersResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unodcrsResponseStatus :: Lens.Lens' UpdateNumberOfDomainControllersResponse Lude.Int
unodcrsResponseStatus = Lens.lens (responseStatus :: UpdateNumberOfDomainControllersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateNumberOfDomainControllersResponse)
{-# DEPRECATED unodcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
