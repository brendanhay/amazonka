{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.GrantAccess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Grants RDP access to a Windows instance for a specified time period.
module Network.AWS.OpsWorks.GrantAccess
  ( -- * Creating a request
    GrantAccess (..),
    mkGrantAccess,

    -- ** Request lenses
    gaValidForInMinutes,
    gaInstanceId,

    -- * Destructuring the response
    GrantAccessResponse (..),
    mkGrantAccessResponse,

    -- ** Response lenses
    garsTemporaryCredential,
    garsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGrantAccess' smart constructor.
data GrantAccess = GrantAccess'
  { validForInMinutes ::
      Lude.Maybe Lude.Natural,
    instanceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GrantAccess' with the minimum fields required to make a request.
--
-- * 'instanceId' - The instance's AWS OpsWorks Stacks ID.
-- * 'validForInMinutes' - The length of time (in minutes) that the grant is valid. When the grant expires at the end of this period, the user will no longer be able to use the credentials to log in. If the user is logged in at the time, he or she automatically will be logged out.
mkGrantAccess ::
  -- | 'instanceId'
  Lude.Text ->
  GrantAccess
mkGrantAccess pInstanceId_ =
  GrantAccess'
    { validForInMinutes = Lude.Nothing,
      instanceId = pInstanceId_
    }

-- | The length of time (in minutes) that the grant is valid. When the grant expires at the end of this period, the user will no longer be able to use the credentials to log in. If the user is logged in at the time, he or she automatically will be logged out.
--
-- /Note:/ Consider using 'validForInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaValidForInMinutes :: Lens.Lens' GrantAccess (Lude.Maybe Lude.Natural)
gaValidForInMinutes = Lens.lens (validForInMinutes :: GrantAccess -> Lude.Maybe Lude.Natural) (\s a -> s {validForInMinutes = a} :: GrantAccess)
{-# DEPRECATED gaValidForInMinutes "Use generic-lens or generic-optics with 'validForInMinutes' instead." #-}

-- | The instance's AWS OpsWorks Stacks ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaInstanceId :: Lens.Lens' GrantAccess Lude.Text
gaInstanceId = Lens.lens (instanceId :: GrantAccess -> Lude.Text) (\s a -> s {instanceId = a} :: GrantAccess)
{-# DEPRECATED gaInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Lude.AWSRequest GrantAccess where
  type Rs GrantAccess = GrantAccessResponse
  request = Req.postJSON opsWorksService
  response =
    Res.receiveJSON
      ( \s h x ->
          GrantAccessResponse'
            Lude.<$> (x Lude..?> "TemporaryCredential")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GrantAccess where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.GrantAccess" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GrantAccess where
  toJSON GrantAccess' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ValidForInMinutes" Lude..=) Lude.<$> validForInMinutes,
            Lude.Just ("InstanceId" Lude..= instanceId)
          ]
      )

instance Lude.ToPath GrantAccess where
  toPath = Lude.const "/"

instance Lude.ToQuery GrantAccess where
  toQuery = Lude.const Lude.mempty

-- | Contains the response to a @GrantAccess@ request.
--
-- /See:/ 'mkGrantAccessResponse' smart constructor.
data GrantAccessResponse = GrantAccessResponse'
  { temporaryCredential ::
      Lude.Maybe TemporaryCredential,
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

-- | Creates a value of 'GrantAccessResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'temporaryCredential' - A @TemporaryCredential@ object that contains the data needed to log in to the instance by RDP clients, such as the Microsoft Remote Desktop Connection.
mkGrantAccessResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GrantAccessResponse
mkGrantAccessResponse pResponseStatus_ =
  GrantAccessResponse'
    { temporaryCredential = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A @TemporaryCredential@ object that contains the data needed to log in to the instance by RDP clients, such as the Microsoft Remote Desktop Connection.
--
-- /Note:/ Consider using 'temporaryCredential' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garsTemporaryCredential :: Lens.Lens' GrantAccessResponse (Lude.Maybe TemporaryCredential)
garsTemporaryCredential = Lens.lens (temporaryCredential :: GrantAccessResponse -> Lude.Maybe TemporaryCredential) (\s a -> s {temporaryCredential = a} :: GrantAccessResponse)
{-# DEPRECATED garsTemporaryCredential "Use generic-lens or generic-optics with 'temporaryCredential' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garsResponseStatus :: Lens.Lens' GrantAccessResponse Lude.Int
garsResponseStatus = Lens.lens (responseStatus :: GrantAccessResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GrantAccessResponse)
{-# DEPRECATED garsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
