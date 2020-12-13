{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetRelationalDatabaseMasterUserPassword
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current, previous, or pending versions of the master user password for a Lightsail database.
--
-- The @GetRelationalDatabaseMasterUserPassword@ operation supports tag-based access control via resource tags applied to the resource identified by relationalDatabaseName.
module Network.AWS.Lightsail.GetRelationalDatabaseMasterUserPassword
  ( -- * Creating a request
    GetRelationalDatabaseMasterUserPassword (..),
    mkGetRelationalDatabaseMasterUserPassword,

    -- ** Request lenses
    grdmupPasswordVersion,
    grdmupRelationalDatabaseName,

    -- * Destructuring the response
    GetRelationalDatabaseMasterUserPasswordResponse (..),
    mkGetRelationalDatabaseMasterUserPasswordResponse,

    -- ** Response lenses
    grdmuprsMasterUserPassword,
    grdmuprsCreatedAt,
    grdmuprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetRelationalDatabaseMasterUserPassword' smart constructor.
data GetRelationalDatabaseMasterUserPassword = GetRelationalDatabaseMasterUserPassword'
  { -- | The password version to return.
    --
    -- Specifying @CURRENT@ or @PREVIOUS@ returns the current or previous passwords respectively. Specifying @PENDING@ returns the newest version of the password that will rotate to @CURRENT@ . After the @PENDING@ password rotates to @CURRENT@ , the @PENDING@ password is no longer available.
    -- Default: @CURRENT@
    passwordVersion :: Lude.Maybe RelationalDatabasePasswordVersion,
    -- | The name of your database for which to get the master user password.
    relationalDatabaseName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRelationalDatabaseMasterUserPassword' with the minimum fields required to make a request.
--
-- * 'passwordVersion' - The password version to return.
--
-- Specifying @CURRENT@ or @PREVIOUS@ returns the current or previous passwords respectively. Specifying @PENDING@ returns the newest version of the password that will rotate to @CURRENT@ . After the @PENDING@ password rotates to @CURRENT@ , the @PENDING@ password is no longer available.
-- Default: @CURRENT@
-- * 'relationalDatabaseName' - The name of your database for which to get the master user password.
mkGetRelationalDatabaseMasterUserPassword ::
  -- | 'relationalDatabaseName'
  Lude.Text ->
  GetRelationalDatabaseMasterUserPassword
mkGetRelationalDatabaseMasterUserPassword pRelationalDatabaseName_ =
  GetRelationalDatabaseMasterUserPassword'
    { passwordVersion =
        Lude.Nothing,
      relationalDatabaseName = pRelationalDatabaseName_
    }

-- | The password version to return.
--
-- Specifying @CURRENT@ or @PREVIOUS@ returns the current or previous passwords respectively. Specifying @PENDING@ returns the newest version of the password that will rotate to @CURRENT@ . After the @PENDING@ password rotates to @CURRENT@ , the @PENDING@ password is no longer available.
-- Default: @CURRENT@
--
-- /Note:/ Consider using 'passwordVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdmupPasswordVersion :: Lens.Lens' GetRelationalDatabaseMasterUserPassword (Lude.Maybe RelationalDatabasePasswordVersion)
grdmupPasswordVersion = Lens.lens (passwordVersion :: GetRelationalDatabaseMasterUserPassword -> Lude.Maybe RelationalDatabasePasswordVersion) (\s a -> s {passwordVersion = a} :: GetRelationalDatabaseMasterUserPassword)
{-# DEPRECATED grdmupPasswordVersion "Use generic-lens or generic-optics with 'passwordVersion' instead." #-}

-- | The name of your database for which to get the master user password.
--
-- /Note:/ Consider using 'relationalDatabaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdmupRelationalDatabaseName :: Lens.Lens' GetRelationalDatabaseMasterUserPassword Lude.Text
grdmupRelationalDatabaseName = Lens.lens (relationalDatabaseName :: GetRelationalDatabaseMasterUserPassword -> Lude.Text) (\s a -> s {relationalDatabaseName = a} :: GetRelationalDatabaseMasterUserPassword)
{-# DEPRECATED grdmupRelationalDatabaseName "Use generic-lens or generic-optics with 'relationalDatabaseName' instead." #-}

instance Lude.AWSRequest GetRelationalDatabaseMasterUserPassword where
  type
    Rs GetRelationalDatabaseMasterUserPassword =
      GetRelationalDatabaseMasterUserPasswordResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetRelationalDatabaseMasterUserPasswordResponse'
            Lude.<$> (x Lude..?> "masterUserPassword")
            Lude.<*> (x Lude..?> "createdAt")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetRelationalDatabaseMasterUserPassword where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Lightsail_20161128.GetRelationalDatabaseMasterUserPassword" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetRelationalDatabaseMasterUserPassword where
  toJSON GetRelationalDatabaseMasterUserPassword' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("passwordVersion" Lude..=) Lude.<$> passwordVersion,
            Lude.Just
              ("relationalDatabaseName" Lude..= relationalDatabaseName)
          ]
      )

instance Lude.ToPath GetRelationalDatabaseMasterUserPassword where
  toPath = Lude.const "/"

instance Lude.ToQuery GetRelationalDatabaseMasterUserPassword where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetRelationalDatabaseMasterUserPasswordResponse' smart constructor.
data GetRelationalDatabaseMasterUserPasswordResponse = GetRelationalDatabaseMasterUserPasswordResponse'
  { -- | The master user password for the @password version@ specified.
    masterUserPassword :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The timestamp when the specified version of the master user password was created.
    createdAt :: Lude.Maybe Lude.Timestamp,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRelationalDatabaseMasterUserPasswordResponse' with the minimum fields required to make a request.
--
-- * 'masterUserPassword' - The master user password for the @password version@ specified.
-- * 'createdAt' - The timestamp when the specified version of the master user password was created.
-- * 'responseStatus' - The response status code.
mkGetRelationalDatabaseMasterUserPasswordResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetRelationalDatabaseMasterUserPasswordResponse
mkGetRelationalDatabaseMasterUserPasswordResponse pResponseStatus_ =
  GetRelationalDatabaseMasterUserPasswordResponse'
    { masterUserPassword =
        Lude.Nothing,
      createdAt = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The master user password for the @password version@ specified.
--
-- /Note:/ Consider using 'masterUserPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdmuprsMasterUserPassword :: Lens.Lens' GetRelationalDatabaseMasterUserPasswordResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
grdmuprsMasterUserPassword = Lens.lens (masterUserPassword :: GetRelationalDatabaseMasterUserPasswordResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {masterUserPassword = a} :: GetRelationalDatabaseMasterUserPasswordResponse)
{-# DEPRECATED grdmuprsMasterUserPassword "Use generic-lens or generic-optics with 'masterUserPassword' instead." #-}

-- | The timestamp when the specified version of the master user password was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdmuprsCreatedAt :: Lens.Lens' GetRelationalDatabaseMasterUserPasswordResponse (Lude.Maybe Lude.Timestamp)
grdmuprsCreatedAt = Lens.lens (createdAt :: GetRelationalDatabaseMasterUserPasswordResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: GetRelationalDatabaseMasterUserPasswordResponse)
{-# DEPRECATED grdmuprsCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdmuprsResponseStatus :: Lens.Lens' GetRelationalDatabaseMasterUserPasswordResponse Lude.Int
grdmuprsResponseStatus = Lens.lens (responseStatus :: GetRelationalDatabaseMasterUserPasswordResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetRelationalDatabaseMasterUserPasswordResponse)
{-# DEPRECATED grdmuprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
