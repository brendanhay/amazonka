{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.UpdateTrust
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the trust that has been set up between your AWS Managed Microsoft AD directory and an on-premises Active Directory.
module Network.AWS.DirectoryService.UpdateTrust
  ( -- * Creating a request
    UpdateTrust (..),
    mkUpdateTrust,

    -- ** Request lenses
    utSelectiveAuth,
    utTrustId,

    -- * Destructuring the response
    UpdateTrustResponse (..),
    mkUpdateTrustResponse,

    -- ** Response lenses
    utrsRequestId,
    utrsTrustId,
    utrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateTrust' smart constructor.
data UpdateTrust = UpdateTrust'
  { -- | Updates selective authentication for the trust.
    selectiveAuth :: Lude.Maybe SelectiveAuth,
    -- | Identifier of the trust relationship.
    trustId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateTrust' with the minimum fields required to make a request.
--
-- * 'selectiveAuth' - Updates selective authentication for the trust.
-- * 'trustId' - Identifier of the trust relationship.
mkUpdateTrust ::
  -- | 'trustId'
  Lude.Text ->
  UpdateTrust
mkUpdateTrust pTrustId_ =
  UpdateTrust' {selectiveAuth = Lude.Nothing, trustId = pTrustId_}

-- | Updates selective authentication for the trust.
--
-- /Note:/ Consider using 'selectiveAuth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utSelectiveAuth :: Lens.Lens' UpdateTrust (Lude.Maybe SelectiveAuth)
utSelectiveAuth = Lens.lens (selectiveAuth :: UpdateTrust -> Lude.Maybe SelectiveAuth) (\s a -> s {selectiveAuth = a} :: UpdateTrust)
{-# DEPRECATED utSelectiveAuth "Use generic-lens or generic-optics with 'selectiveAuth' instead." #-}

-- | Identifier of the trust relationship.
--
-- /Note:/ Consider using 'trustId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utTrustId :: Lens.Lens' UpdateTrust Lude.Text
utTrustId = Lens.lens (trustId :: UpdateTrust -> Lude.Text) (\s a -> s {trustId = a} :: UpdateTrust)
{-# DEPRECATED utTrustId "Use generic-lens or generic-optics with 'trustId' instead." #-}

instance Lude.AWSRequest UpdateTrust where
  type Rs UpdateTrust = UpdateTrustResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateTrustResponse'
            Lude.<$> (x Lude..?> "RequestId")
            Lude.<*> (x Lude..?> "TrustId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateTrust where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DirectoryService_20150416.UpdateTrust" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateTrust where
  toJSON UpdateTrust' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SelectiveAuth" Lude..=) Lude.<$> selectiveAuth,
            Lude.Just ("TrustId" Lude..= trustId)
          ]
      )

instance Lude.ToPath UpdateTrust where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateTrust where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateTrustResponse' smart constructor.
data UpdateTrustResponse = UpdateTrustResponse'
  { requestId :: Lude.Maybe Lude.Text,
    -- | Identifier of the trust relationship.
    trustId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateTrustResponse' with the minimum fields required to make a request.
--
-- * 'requestId' -
-- * 'trustId' - Identifier of the trust relationship.
-- * 'responseStatus' - The response status code.
mkUpdateTrustResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateTrustResponse
mkUpdateTrustResponse pResponseStatus_ =
  UpdateTrustResponse'
    { requestId = Lude.Nothing,
      trustId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrsRequestId :: Lens.Lens' UpdateTrustResponse (Lude.Maybe Lude.Text)
utrsRequestId = Lens.lens (requestId :: UpdateTrustResponse -> Lude.Maybe Lude.Text) (\s a -> s {requestId = a} :: UpdateTrustResponse)
{-# DEPRECATED utrsRequestId "Use generic-lens or generic-optics with 'requestId' instead." #-}

-- | Identifier of the trust relationship.
--
-- /Note:/ Consider using 'trustId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrsTrustId :: Lens.Lens' UpdateTrustResponse (Lude.Maybe Lude.Text)
utrsTrustId = Lens.lens (trustId :: UpdateTrustResponse -> Lude.Maybe Lude.Text) (\s a -> s {trustId = a} :: UpdateTrustResponse)
{-# DEPRECATED utrsTrustId "Use generic-lens or generic-optics with 'trustId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrsResponseStatus :: Lens.Lens' UpdateTrustResponse Lude.Int
utrsResponseStatus = Lens.lens (responseStatus :: UpdateTrustResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateTrustResponse)
{-# DEPRECATED utrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
