{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.CreateBackup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an application-level backup of a server. While the server is in the @BACKING_UP@ state, the server cannot be changed, and no additional backup can be created.
--
-- Backups can be created for servers in @RUNNING@ , @HEALTHY@ , and @UNHEALTHY@ states. By default, you can create a maximum of 50 manual backups.
-- This operation is asynchronous.
-- A @LimitExceededException@ is thrown when the maximum number of manual backups is reached. An @InvalidStateException@ is thrown when the server is not in any of the following states: RUNNING, HEALTHY, or UNHEALTHY. A @ResourceNotFoundException@ is thrown when the server is not found. A @ValidationException@ is thrown when parameters of the request are not valid.
module Network.AWS.OpsWorksCM.CreateBackup
  ( -- * Creating a request
    CreateBackup (..),
    mkCreateBackup,

    -- ** Request lenses
    cbDescription,
    cbTags,
    cbServerName,

    -- * Destructuring the response
    CreateBackupResponse (..),
    mkCreateBackupResponse,

    -- ** Response lenses
    cbrsBackup,
    cbrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorksCM.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateBackup' smart constructor.
data CreateBackup = CreateBackup'
  { description ::
      Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    serverName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateBackup' with the minimum fields required to make a request.
--
-- * 'description' - A user-defined description of the backup.
-- * 'serverName' - The name of the server that you want to back up.
-- * 'tags' - A map that contains tag keys and tag values to attach to an AWS OpsWorks-CM server backup.
--
--
--     * The key cannot be empty.
--
--
--     * The key can be a maximum of 127 characters, and can contain only Unicode letters, numbers, or separators, or the following special characters: @+ - = . _ : /@
--
--
--     * The value can be a maximum 255 characters, and contain only Unicode letters, numbers, or separators, or the following special characters: @+ - = . _ : /@
--
--
--     * Leading and trailing white spaces are trimmed from both the key and value.
--
--
--     * A maximum of 50 user-applied tags is allowed for tag-supported AWS OpsWorks-CM resources.
mkCreateBackup ::
  -- | 'serverName'
  Lude.Text ->
  CreateBackup
mkCreateBackup pServerName_ =
  CreateBackup'
    { description = Lude.Nothing,
      tags = Lude.Nothing,
      serverName = pServerName_
    }

-- | A user-defined description of the backup.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbDescription :: Lens.Lens' CreateBackup (Lude.Maybe Lude.Text)
cbDescription = Lens.lens (description :: CreateBackup -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateBackup)
{-# DEPRECATED cbDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A map that contains tag keys and tag values to attach to an AWS OpsWorks-CM server backup.
--
--
--     * The key cannot be empty.
--
--
--     * The key can be a maximum of 127 characters, and can contain only Unicode letters, numbers, or separators, or the following special characters: @+ - = . _ : /@
--
--
--     * The value can be a maximum 255 characters, and contain only Unicode letters, numbers, or separators, or the following special characters: @+ - = . _ : /@
--
--
--     * Leading and trailing white spaces are trimmed from both the key and value.
--
--
--     * A maximum of 50 user-applied tags is allowed for tag-supported AWS OpsWorks-CM resources.
--
--
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbTags :: Lens.Lens' CreateBackup (Lude.Maybe [Tag])
cbTags = Lens.lens (tags :: CreateBackup -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateBackup)
{-# DEPRECATED cbTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the server that you want to back up.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbServerName :: Lens.Lens' CreateBackup Lude.Text
cbServerName = Lens.lens (serverName :: CreateBackup -> Lude.Text) (\s a -> s {serverName = a} :: CreateBackup)
{-# DEPRECATED cbServerName "Use generic-lens or generic-optics with 'serverName' instead." #-}

instance Lude.AWSRequest CreateBackup where
  type Rs CreateBackup = CreateBackupResponse
  request = Req.postJSON opsWorksCMService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateBackupResponse'
            Lude.<$> (x Lude..?> "Backup") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateBackup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorksCM_V2016_11_01.CreateBackup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateBackup where
  toJSON CreateBackup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Description" Lude..=) Lude.<$> description,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("ServerName" Lude..= serverName)
          ]
      )

instance Lude.ToPath CreateBackup where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateBackup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateBackupResponse' smart constructor.
data CreateBackupResponse = CreateBackupResponse'
  { backup ::
      Lude.Maybe Backup,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateBackupResponse' with the minimum fields required to make a request.
--
-- * 'backup' - Backup created by request.
-- * 'responseStatus' - The response status code.
mkCreateBackupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateBackupResponse
mkCreateBackupResponse pResponseStatus_ =
  CreateBackupResponse'
    { backup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Backup created by request.
--
-- /Note:/ Consider using 'backup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrsBackup :: Lens.Lens' CreateBackupResponse (Lude.Maybe Backup)
cbrsBackup = Lens.lens (backup :: CreateBackupResponse -> Lude.Maybe Backup) (\s a -> s {backup = a} :: CreateBackupResponse)
{-# DEPRECATED cbrsBackup "Use generic-lens or generic-optics with 'backup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrsResponseStatus :: Lens.Lens' CreateBackupResponse Lude.Int
cbrsResponseStatus = Lens.lens (responseStatus :: CreateBackupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateBackupResponse)
{-# DEPRECATED cbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
