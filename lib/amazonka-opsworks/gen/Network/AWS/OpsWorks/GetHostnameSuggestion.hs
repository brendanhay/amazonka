{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.GetHostnameSuggestion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a generated host name for the specified layer, based on the current host name theme.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.GetHostnameSuggestion
  ( -- * Creating a request
    GetHostnameSuggestion (..),
    mkGetHostnameSuggestion,

    -- ** Request lenses
    ghsLayerId,

    -- * Destructuring the response
    GetHostnameSuggestionResponse (..),
    mkGetHostnameSuggestionResponse,

    -- ** Response lenses
    ghsrsHostname,
    ghsrsLayerId,
    ghsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetHostnameSuggestion' smart constructor.
newtype GetHostnameSuggestion = GetHostnameSuggestion'
  { -- | The layer ID.
    layerId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetHostnameSuggestion' with the minimum fields required to make a request.
--
-- * 'layerId' - The layer ID.
mkGetHostnameSuggestion ::
  -- | 'layerId'
  Lude.Text ->
  GetHostnameSuggestion
mkGetHostnameSuggestion pLayerId_ =
  GetHostnameSuggestion' {layerId = pLayerId_}

-- | The layer ID.
--
-- /Note:/ Consider using 'layerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghsLayerId :: Lens.Lens' GetHostnameSuggestion Lude.Text
ghsLayerId = Lens.lens (layerId :: GetHostnameSuggestion -> Lude.Text) (\s a -> s {layerId = a} :: GetHostnameSuggestion)
{-# DEPRECATED ghsLayerId "Use generic-lens or generic-optics with 'layerId' instead." #-}

instance Lude.AWSRequest GetHostnameSuggestion where
  type Rs GetHostnameSuggestion = GetHostnameSuggestionResponse
  request = Req.postJSON opsWorksService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetHostnameSuggestionResponse'
            Lude.<$> (x Lude..?> "Hostname")
            Lude.<*> (x Lude..?> "LayerId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetHostnameSuggestion where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.GetHostnameSuggestion" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetHostnameSuggestion where
  toJSON GetHostnameSuggestion' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("LayerId" Lude..= layerId)])

instance Lude.ToPath GetHostnameSuggestion where
  toPath = Lude.const "/"

instance Lude.ToQuery GetHostnameSuggestion where
  toQuery = Lude.const Lude.mempty

-- | Contains the response to a @GetHostnameSuggestion@ request.
--
-- /See:/ 'mkGetHostnameSuggestionResponse' smart constructor.
data GetHostnameSuggestionResponse = GetHostnameSuggestionResponse'
  { -- | The generated host name.
    hostname :: Lude.Maybe Lude.Text,
    -- | The layer ID.
    layerId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetHostnameSuggestionResponse' with the minimum fields required to make a request.
--
-- * 'hostname' - The generated host name.
-- * 'layerId' - The layer ID.
-- * 'responseStatus' - The response status code.
mkGetHostnameSuggestionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetHostnameSuggestionResponse
mkGetHostnameSuggestionResponse pResponseStatus_ =
  GetHostnameSuggestionResponse'
    { hostname = Lude.Nothing,
      layerId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The generated host name.
--
-- /Note:/ Consider using 'hostname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghsrsHostname :: Lens.Lens' GetHostnameSuggestionResponse (Lude.Maybe Lude.Text)
ghsrsHostname = Lens.lens (hostname :: GetHostnameSuggestionResponse -> Lude.Maybe Lude.Text) (\s a -> s {hostname = a} :: GetHostnameSuggestionResponse)
{-# DEPRECATED ghsrsHostname "Use generic-lens or generic-optics with 'hostname' instead." #-}

-- | The layer ID.
--
-- /Note:/ Consider using 'layerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghsrsLayerId :: Lens.Lens' GetHostnameSuggestionResponse (Lude.Maybe Lude.Text)
ghsrsLayerId = Lens.lens (layerId :: GetHostnameSuggestionResponse -> Lude.Maybe Lude.Text) (\s a -> s {layerId = a} :: GetHostnameSuggestionResponse)
{-# DEPRECATED ghsrsLayerId "Use generic-lens or generic-optics with 'layerId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghsrsResponseStatus :: Lens.Lens' GetHostnameSuggestionResponse Lude.Int
ghsrsResponseStatus = Lens.lens (responseStatus :: GetHostnameSuggestionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetHostnameSuggestionResponse)
{-# DEPRECATED ghsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
