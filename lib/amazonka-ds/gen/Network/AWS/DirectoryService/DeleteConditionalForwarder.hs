{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DeleteConditionalForwarder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a conditional forwarder that has been set up for your AWS directory.
module Network.AWS.DirectoryService.DeleteConditionalForwarder
  ( -- * Creating a request
    DeleteConditionalForwarder (..),
    mkDeleteConditionalForwarder,

    -- ** Request lenses
    dcfDirectoryId,
    dcfRemoteDomainName,

    -- * Destructuring the response
    DeleteConditionalForwarderResponse (..),
    mkDeleteConditionalForwarderResponse,

    -- ** Response lenses
    dcfrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Deletes a conditional forwarder.
--
-- /See:/ 'mkDeleteConditionalForwarder' smart constructor.
data DeleteConditionalForwarder = DeleteConditionalForwarder'
  { -- | The directory ID for which you are deleting the conditional forwarder.
    directoryId :: Lude.Text,
    -- | The fully qualified domain name (FQDN) of the remote domain with which you are deleting the conditional forwarder.
    remoteDomainName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteConditionalForwarder' with the minimum fields required to make a request.
--
-- * 'directoryId' - The directory ID for which you are deleting the conditional forwarder.
-- * 'remoteDomainName' - The fully qualified domain name (FQDN) of the remote domain with which you are deleting the conditional forwarder.
mkDeleteConditionalForwarder ::
  -- | 'directoryId'
  Lude.Text ->
  -- | 'remoteDomainName'
  Lude.Text ->
  DeleteConditionalForwarder
mkDeleteConditionalForwarder pDirectoryId_ pRemoteDomainName_ =
  DeleteConditionalForwarder'
    { directoryId = pDirectoryId_,
      remoteDomainName = pRemoteDomainName_
    }

-- | The directory ID for which you are deleting the conditional forwarder.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfDirectoryId :: Lens.Lens' DeleteConditionalForwarder Lude.Text
dcfDirectoryId = Lens.lens (directoryId :: DeleteConditionalForwarder -> Lude.Text) (\s a -> s {directoryId = a} :: DeleteConditionalForwarder)
{-# DEPRECATED dcfDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The fully qualified domain name (FQDN) of the remote domain with which you are deleting the conditional forwarder.
--
-- /Note:/ Consider using 'remoteDomainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfRemoteDomainName :: Lens.Lens' DeleteConditionalForwarder Lude.Text
dcfRemoteDomainName = Lens.lens (remoteDomainName :: DeleteConditionalForwarder -> Lude.Text) (\s a -> s {remoteDomainName = a} :: DeleteConditionalForwarder)
{-# DEPRECATED dcfRemoteDomainName "Use generic-lens or generic-optics with 'remoteDomainName' instead." #-}

instance Lude.AWSRequest DeleteConditionalForwarder where
  type
    Rs DeleteConditionalForwarder =
      DeleteConditionalForwarderResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteConditionalForwarderResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteConditionalForwarder where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "DirectoryService_20150416.DeleteConditionalForwarder" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteConditionalForwarder where
  toJSON DeleteConditionalForwarder' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DirectoryId" Lude..= directoryId),
            Lude.Just ("RemoteDomainName" Lude..= remoteDomainName)
          ]
      )

instance Lude.ToPath DeleteConditionalForwarder where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteConditionalForwarder where
  toQuery = Lude.const Lude.mempty

-- | The result of a DeleteConditionalForwarder request.
--
-- /See:/ 'mkDeleteConditionalForwarderResponse' smart constructor.
newtype DeleteConditionalForwarderResponse = DeleteConditionalForwarderResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteConditionalForwarderResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteConditionalForwarderResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteConditionalForwarderResponse
mkDeleteConditionalForwarderResponse pResponseStatus_ =
  DeleteConditionalForwarderResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfrsResponseStatus :: Lens.Lens' DeleteConditionalForwarderResponse Lude.Int
dcfrsResponseStatus = Lens.lens (responseStatus :: DeleteConditionalForwarderResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteConditionalForwarderResponse)
{-# DEPRECATED dcfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
