{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.UpdateEnvironment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the settings of an existing AWS Cloud9 development environment.
module Network.AWS.Cloud9.UpdateEnvironment
  ( -- * Creating a request
    UpdateEnvironment (..),
    mkUpdateEnvironment,

    -- ** Request lenses
    ueName,
    ueEnvironmentId,
    ueDescription,

    -- * Destructuring the response
    UpdateEnvironmentResponse (..),
    mkUpdateEnvironmentResponse,

    -- ** Response lenses
    uersResponseStatus,
  )
where

import Network.AWS.Cloud9.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateEnvironment' smart constructor.
data UpdateEnvironment = UpdateEnvironment'
  { -- | A replacement name for the environment.
    name :: Lude.Maybe Lude.Text,
    -- | The ID of the environment to change settings.
    environmentId :: Lude.Text,
    -- | Any new or replacement description for the environment.
    description :: Lude.Maybe (Lude.Sensitive Lude.Text)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateEnvironment' with the minimum fields required to make a request.
--
-- * 'name' - A replacement name for the environment.
-- * 'environmentId' - The ID of the environment to change settings.
-- * 'description' - Any new or replacement description for the environment.
mkUpdateEnvironment ::
  -- | 'environmentId'
  Lude.Text ->
  UpdateEnvironment
mkUpdateEnvironment pEnvironmentId_ =
  UpdateEnvironment'
    { name = Lude.Nothing,
      environmentId = pEnvironmentId_,
      description = Lude.Nothing
    }

-- | A replacement name for the environment.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueName :: Lens.Lens' UpdateEnvironment (Lude.Maybe Lude.Text)
ueName = Lens.lens (name :: UpdateEnvironment -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateEnvironment)
{-# DEPRECATED ueName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the environment to change settings.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueEnvironmentId :: Lens.Lens' UpdateEnvironment Lude.Text
ueEnvironmentId = Lens.lens (environmentId :: UpdateEnvironment -> Lude.Text) (\s a -> s {environmentId = a} :: UpdateEnvironment)
{-# DEPRECATED ueEnvironmentId "Use generic-lens or generic-optics with 'environmentId' instead." #-}

-- | Any new or replacement description for the environment.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueDescription :: Lens.Lens' UpdateEnvironment (Lude.Maybe (Lude.Sensitive Lude.Text))
ueDescription = Lens.lens (description :: UpdateEnvironment -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {description = a} :: UpdateEnvironment)
{-# DEPRECATED ueDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest UpdateEnvironment where
  type Rs UpdateEnvironment = UpdateEnvironmentResponse
  request = Req.postJSON cloud9Service
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateEnvironmentResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateEnvironment where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCloud9WorkspaceManagementService.UpdateEnvironment" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateEnvironment where
  toJSON UpdateEnvironment' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("name" Lude..=) Lude.<$> name,
            Lude.Just ("environmentId" Lude..= environmentId),
            ("description" Lude..=) Lude.<$> description
          ]
      )

instance Lude.ToPath UpdateEnvironment where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateEnvironment where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateEnvironmentResponse' smart constructor.
newtype UpdateEnvironmentResponse = UpdateEnvironmentResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateEnvironmentResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateEnvironmentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateEnvironmentResponse
mkUpdateEnvironmentResponse pResponseStatus_ =
  UpdateEnvironmentResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uersResponseStatus :: Lens.Lens' UpdateEnvironmentResponse Lude.Int
uersResponseStatus = Lens.lens (responseStatus :: UpdateEnvironmentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateEnvironmentResponse)
{-# DEPRECATED uersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
