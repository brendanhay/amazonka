{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.UpdateApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates metadata about an application.
module Network.AWS.Discovery.UpdateApplication
  ( -- * Creating a request
    UpdateApplication (..),
    mkUpdateApplication,

    -- ** Request lenses
    uaName,
    uaDescription,
    uaConfigurationId,

    -- * Destructuring the response
    UpdateApplicationResponse (..),
    mkUpdateApplicationResponse,

    -- ** Response lenses
    uarsResponseStatus,
  )
where

import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateApplication' smart constructor.
data UpdateApplication = UpdateApplication'
  { name ::
      Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    configurationId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateApplication' with the minimum fields required to make a request.
--
-- * 'configurationId' - Configuration ID of the application to be updated.
-- * 'description' - New description of the application to be updated.
-- * 'name' - New name of the application to be updated.
mkUpdateApplication ::
  -- | 'configurationId'
  Lude.Text ->
  UpdateApplication
mkUpdateApplication pConfigurationId_ =
  UpdateApplication'
    { name = Lude.Nothing,
      description = Lude.Nothing,
      configurationId = pConfigurationId_
    }

-- | New name of the application to be updated.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaName :: Lens.Lens' UpdateApplication (Lude.Maybe Lude.Text)
uaName = Lens.lens (name :: UpdateApplication -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateApplication)
{-# DEPRECATED uaName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | New description of the application to be updated.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaDescription :: Lens.Lens' UpdateApplication (Lude.Maybe Lude.Text)
uaDescription = Lens.lens (description :: UpdateApplication -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateApplication)
{-# DEPRECATED uaDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Configuration ID of the application to be updated.
--
-- /Note:/ Consider using 'configurationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaConfigurationId :: Lens.Lens' UpdateApplication Lude.Text
uaConfigurationId = Lens.lens (configurationId :: UpdateApplication -> Lude.Text) (\s a -> s {configurationId = a} :: UpdateApplication)
{-# DEPRECATED uaConfigurationId "Use generic-lens or generic-optics with 'configurationId' instead." #-}

instance Lude.AWSRequest UpdateApplication where
  type Rs UpdateApplication = UpdateApplicationResponse
  request = Req.postJSON discoveryService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateApplicationResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateApplication where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSPoseidonService_V2015_11_01.UpdateApplication" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateApplication where
  toJSON UpdateApplication' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("name" Lude..=) Lude.<$> name,
            ("description" Lude..=) Lude.<$> description,
            Lude.Just ("configurationId" Lude..= configurationId)
          ]
      )

instance Lude.ToPath UpdateApplication where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateApplication where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateApplicationResponse' smart constructor.
newtype UpdateApplicationResponse = UpdateApplicationResponse'
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

-- | Creates a value of 'UpdateApplicationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateApplicationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateApplicationResponse
mkUpdateApplicationResponse pResponseStatus_ =
  UpdateApplicationResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarsResponseStatus :: Lens.Lens' UpdateApplicationResponse Lude.Int
uarsResponseStatus = Lens.lens (responseStatus :: UpdateApplicationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateApplicationResponse)
{-# DEPRECATED uarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
