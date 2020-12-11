{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.EnableLDAPS
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Activates the switch for the specific directory to always use LDAP secure calls.
module Network.AWS.DirectoryService.EnableLDAPS
  ( -- * Creating a request
    EnableLDAPS (..),
    mkEnableLDAPS,

    -- ** Request lenses
    eldapsDirectoryId,
    eldapsType,

    -- * Destructuring the response
    EnableLDAPSResponse (..),
    mkEnableLDAPSResponse,

    -- ** Response lenses
    eldapsrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkEnableLDAPS' smart constructor.
data EnableLDAPS = EnableLDAPS'
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

-- | Creates a value of 'EnableLDAPS' with the minimum fields required to make a request.
--
-- * 'directoryId' - The identifier of the directory.
-- * 'type'' - The type of LDAP security to enable. Currently only the value @Client@ is supported.
mkEnableLDAPS ::
  -- | 'directoryId'
  Lude.Text ->
  -- | 'type''
  LDAPSType ->
  EnableLDAPS
mkEnableLDAPS pDirectoryId_ pType_ =
  EnableLDAPS' {directoryId = pDirectoryId_, type' = pType_}

-- | The identifier of the directory.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eldapsDirectoryId :: Lens.Lens' EnableLDAPS Lude.Text
eldapsDirectoryId = Lens.lens (directoryId :: EnableLDAPS -> Lude.Text) (\s a -> s {directoryId = a} :: EnableLDAPS)
{-# DEPRECATED eldapsDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The type of LDAP security to enable. Currently only the value @Client@ is supported.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eldapsType :: Lens.Lens' EnableLDAPS LDAPSType
eldapsType = Lens.lens (type' :: EnableLDAPS -> LDAPSType) (\s a -> s {type' = a} :: EnableLDAPS)
{-# DEPRECATED eldapsType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.AWSRequest EnableLDAPS where
  type Rs EnableLDAPS = EnableLDAPSResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveEmpty
      ( \s h x ->
          EnableLDAPSResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders EnableLDAPS where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DirectoryService_20150416.EnableLDAPS" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON EnableLDAPS where
  toJSON EnableLDAPS' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DirectoryId" Lude..= directoryId),
            Lude.Just ("Type" Lude..= type')
          ]
      )

instance Lude.ToPath EnableLDAPS where
  toPath = Lude.const "/"

instance Lude.ToQuery EnableLDAPS where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkEnableLDAPSResponse' smart constructor.
newtype EnableLDAPSResponse = EnableLDAPSResponse'
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

-- | Creates a value of 'EnableLDAPSResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkEnableLDAPSResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  EnableLDAPSResponse
mkEnableLDAPSResponse pResponseStatus_ =
  EnableLDAPSResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eldapsrsResponseStatus :: Lens.Lens' EnableLDAPSResponse Lude.Int
eldapsrsResponseStatus = Lens.lens (responseStatus :: EnableLDAPSResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: EnableLDAPSResponse)
{-# DEPRECATED eldapsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
