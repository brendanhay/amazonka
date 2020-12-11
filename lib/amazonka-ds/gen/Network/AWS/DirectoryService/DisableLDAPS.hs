{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DisableLDAPS
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deactivates LDAP secure calls for the specified directory.
module Network.AWS.DirectoryService.DisableLDAPS
  ( -- * Creating a request
    DisableLDAPS (..),
    mkDisableLDAPS,

    -- ** Request lenses
    dldapsDirectoryId,
    dldapsType,

    -- * Destructuring the response
    DisableLDAPSResponse (..),
    mkDisableLDAPSResponse,

    -- ** Response lenses
    dldapsrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisableLDAPS' smart constructor.
data DisableLDAPS = DisableLDAPS'
  { directoryId :: Lude.Text,
    type' :: LDAPSType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableLDAPS' with the minimum fields required to make a request.
--
-- * 'directoryId' - The identifier of the directory.
-- * 'type'' - The type of LDAP security to enable. Currently only the value @Client@ is supported.
mkDisableLDAPS ::
  -- | 'directoryId'
  Lude.Text ->
  -- | 'type''
  LDAPSType ->
  DisableLDAPS
mkDisableLDAPS pDirectoryId_ pType_ =
  DisableLDAPS' {directoryId = pDirectoryId_, type' = pType_}

-- | The identifier of the directory.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldapsDirectoryId :: Lens.Lens' DisableLDAPS Lude.Text
dldapsDirectoryId = Lens.lens (directoryId :: DisableLDAPS -> Lude.Text) (\s a -> s {directoryId = a} :: DisableLDAPS)
{-# DEPRECATED dldapsDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The type of LDAP security to enable. Currently only the value @Client@ is supported.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldapsType :: Lens.Lens' DisableLDAPS LDAPSType
dldapsType = Lens.lens (type' :: DisableLDAPS -> LDAPSType) (\s a -> s {type' = a} :: DisableLDAPS)
{-# DEPRECATED dldapsType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.AWSRequest DisableLDAPS where
  type Rs DisableLDAPS = DisableLDAPSResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DisableLDAPSResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisableLDAPS where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DirectoryService_20150416.DisableLDAPS" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisableLDAPS where
  toJSON DisableLDAPS' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DirectoryId" Lude..= directoryId),
            Lude.Just ("Type" Lude..= type')
          ]
      )

instance Lude.ToPath DisableLDAPS where
  toPath = Lude.const "/"

instance Lude.ToQuery DisableLDAPS where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisableLDAPSResponse' smart constructor.
newtype DisableLDAPSResponse = DisableLDAPSResponse'
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

-- | Creates a value of 'DisableLDAPSResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDisableLDAPSResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisableLDAPSResponse
mkDisableLDAPSResponse pResponseStatus_ =
  DisableLDAPSResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldapsrsResponseStatus :: Lens.Lens' DisableLDAPSResponse Lude.Int
dldapsrsResponseStatus = Lens.lens (responseStatus :: DisableLDAPSResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisableLDAPSResponse)
{-# DEPRECATED dldapsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
