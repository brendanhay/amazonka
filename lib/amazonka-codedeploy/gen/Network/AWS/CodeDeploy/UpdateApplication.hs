{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.UpdateApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the name of an application.
module Network.AWS.CodeDeploy.UpdateApplication
  ( -- * Creating a request
    UpdateApplication (..),
    mkUpdateApplication,

    -- ** Request lenses
    uaNewApplicationName,
    uaApplicationName,

    -- * Destructuring the response
    UpdateApplicationResponse (..),
    mkUpdateApplicationResponse,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of an @UpdateApplication@ operation.
--
-- /See:/ 'mkUpdateApplication' smart constructor.
data UpdateApplication = UpdateApplication'
  { -- | The new name to give the application.
    newApplicationName :: Lude.Maybe Lude.Text,
    -- | The current name of the application you want to change.
    applicationName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateApplication' with the minimum fields required to make a request.
--
-- * 'newApplicationName' - The new name to give the application.
-- * 'applicationName' - The current name of the application you want to change.
mkUpdateApplication ::
  UpdateApplication
mkUpdateApplication =
  UpdateApplication'
    { newApplicationName = Lude.Nothing,
      applicationName = Lude.Nothing
    }

-- | The new name to give the application.
--
-- /Note:/ Consider using 'newApplicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaNewApplicationName :: Lens.Lens' UpdateApplication (Lude.Maybe Lude.Text)
uaNewApplicationName = Lens.lens (newApplicationName :: UpdateApplication -> Lude.Maybe Lude.Text) (\s a -> s {newApplicationName = a} :: UpdateApplication)
{-# DEPRECATED uaNewApplicationName "Use generic-lens or generic-optics with 'newApplicationName' instead." #-}

-- | The current name of the application you want to change.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaApplicationName :: Lens.Lens' UpdateApplication (Lude.Maybe Lude.Text)
uaApplicationName = Lens.lens (applicationName :: UpdateApplication -> Lude.Maybe Lude.Text) (\s a -> s {applicationName = a} :: UpdateApplication)
{-# DEPRECATED uaApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

instance Lude.AWSRequest UpdateApplication where
  type Rs UpdateApplication = UpdateApplicationResponse
  request = Req.postJSON codeDeployService
  response = Res.receiveNull UpdateApplicationResponse'

instance Lude.ToHeaders UpdateApplication where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeDeploy_20141006.UpdateApplication" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateApplication where
  toJSON UpdateApplication' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("newApplicationName" Lude..=) Lude.<$> newApplicationName,
            ("applicationName" Lude..=) Lude.<$> applicationName
          ]
      )

instance Lude.ToPath UpdateApplication where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateApplication where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateApplicationResponse' smart constructor.
data UpdateApplicationResponse = UpdateApplicationResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateApplicationResponse' with the minimum fields required to make a request.
mkUpdateApplicationResponse ::
  UpdateApplicationResponse
mkUpdateApplicationResponse = UpdateApplicationResponse'
