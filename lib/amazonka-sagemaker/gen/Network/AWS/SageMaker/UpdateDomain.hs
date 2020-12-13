{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.UpdateDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the default settings for new user profiles in the domain.
module Network.AWS.SageMaker.UpdateDomain
  ( -- * Creating a request
    UpdateDomain (..),
    mkUpdateDomain,

    -- ** Request lenses
    udDefaultUserSettings,
    udDomainId,

    -- * Destructuring the response
    UpdateDomainResponse (..),
    mkUpdateDomainResponse,

    -- ** Response lenses
    udrsDomainARN,
    udrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkUpdateDomain' smart constructor.
data UpdateDomain = UpdateDomain'
  { -- | A collection of settings.
    defaultUserSettings :: Lude.Maybe UserSettings,
    -- | The ID of the domain to be updated.
    domainId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDomain' with the minimum fields required to make a request.
--
-- * 'defaultUserSettings' - A collection of settings.
-- * 'domainId' - The ID of the domain to be updated.
mkUpdateDomain ::
  -- | 'domainId'
  Lude.Text ->
  UpdateDomain
mkUpdateDomain pDomainId_ =
  UpdateDomain'
    { defaultUserSettings = Lude.Nothing,
      domainId = pDomainId_
    }

-- | A collection of settings.
--
-- /Note:/ Consider using 'defaultUserSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udDefaultUserSettings :: Lens.Lens' UpdateDomain (Lude.Maybe UserSettings)
udDefaultUserSettings = Lens.lens (defaultUserSettings :: UpdateDomain -> Lude.Maybe UserSettings) (\s a -> s {defaultUserSettings = a} :: UpdateDomain)
{-# DEPRECATED udDefaultUserSettings "Use generic-lens or generic-optics with 'defaultUserSettings' instead." #-}

-- | The ID of the domain to be updated.
--
-- /Note:/ Consider using 'domainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udDomainId :: Lens.Lens' UpdateDomain Lude.Text
udDomainId = Lens.lens (domainId :: UpdateDomain -> Lude.Text) (\s a -> s {domainId = a} :: UpdateDomain)
{-# DEPRECATED udDomainId "Use generic-lens or generic-optics with 'domainId' instead." #-}

instance Lude.AWSRequest UpdateDomain where
  type Rs UpdateDomain = UpdateDomainResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateDomainResponse'
            Lude.<$> (x Lude..?> "DomainArn") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateDomain where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.UpdateDomain" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateDomain where
  toJSON UpdateDomain' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DefaultUserSettings" Lude..=) Lude.<$> defaultUserSettings,
            Lude.Just ("DomainId" Lude..= domainId)
          ]
      )

instance Lude.ToPath UpdateDomain where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateDomain where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateDomainResponse' smart constructor.
data UpdateDomainResponse = UpdateDomainResponse'
  { -- | The Amazon Resource Name (ARN) of the domain.
    domainARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDomainResponse' with the minimum fields required to make a request.
--
-- * 'domainARN' - The Amazon Resource Name (ARN) of the domain.
-- * 'responseStatus' - The response status code.
mkUpdateDomainResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateDomainResponse
mkUpdateDomainResponse pResponseStatus_ =
  UpdateDomainResponse'
    { domainARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the domain.
--
-- /Note:/ Consider using 'domainARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrsDomainARN :: Lens.Lens' UpdateDomainResponse (Lude.Maybe Lude.Text)
udrsDomainARN = Lens.lens (domainARN :: UpdateDomainResponse -> Lude.Maybe Lude.Text) (\s a -> s {domainARN = a} :: UpdateDomainResponse)
{-# DEPRECATED udrsDomainARN "Use generic-lens or generic-optics with 'domainARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrsResponseStatus :: Lens.Lens' UpdateDomainResponse Lude.Int
udrsResponseStatus = Lens.lens (responseStatus :: UpdateDomainResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateDomainResponse)
{-# DEPRECATED udrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
