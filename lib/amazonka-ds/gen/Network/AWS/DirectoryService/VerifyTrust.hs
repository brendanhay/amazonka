{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.VerifyTrust
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Directory Service for Microsoft Active Directory allows you to configure and verify trust relationships.
--
-- This action verifies a trust relationship between your AWS Managed Microsoft AD directory and an external domain.
module Network.AWS.DirectoryService.VerifyTrust
  ( -- * Creating a request
    VerifyTrust (..),
    mkVerifyTrust,

    -- ** Request lenses
    vtTrustId,

    -- * Destructuring the response
    VerifyTrustResponse (..),
    mkVerifyTrustResponse,

    -- ** Response lenses
    vtrsTrustId,
    vtrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Initiates the verification of an existing trust relationship between an AWS Managed Microsoft AD directory and an external domain.
--
-- /See:/ 'mkVerifyTrust' smart constructor.
newtype VerifyTrust = VerifyTrust' {trustId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VerifyTrust' with the minimum fields required to make a request.
--
-- * 'trustId' - The unique Trust ID of the trust relationship to verify.
mkVerifyTrust ::
  -- | 'trustId'
  Lude.Text ->
  VerifyTrust
mkVerifyTrust pTrustId_ = VerifyTrust' {trustId = pTrustId_}

-- | The unique Trust ID of the trust relationship to verify.
--
-- /Note:/ Consider using 'trustId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtTrustId :: Lens.Lens' VerifyTrust Lude.Text
vtTrustId = Lens.lens (trustId :: VerifyTrust -> Lude.Text) (\s a -> s {trustId = a} :: VerifyTrust)
{-# DEPRECATED vtTrustId "Use generic-lens or generic-optics with 'trustId' instead." #-}

instance Lude.AWSRequest VerifyTrust where
  type Rs VerifyTrust = VerifyTrustResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveJSON
      ( \s h x ->
          VerifyTrustResponse'
            Lude.<$> (x Lude..?> "TrustId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders VerifyTrust where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DirectoryService_20150416.VerifyTrust" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON VerifyTrust where
  toJSON VerifyTrust' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("TrustId" Lude..= trustId)])

instance Lude.ToPath VerifyTrust where
  toPath = Lude.const "/"

instance Lude.ToQuery VerifyTrust where
  toQuery = Lude.const Lude.mempty

-- | Result of a VerifyTrust request.
--
-- /See:/ 'mkVerifyTrustResponse' smart constructor.
data VerifyTrustResponse = VerifyTrustResponse'
  { trustId ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'VerifyTrustResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'trustId' - The unique Trust ID of the trust relationship that was verified.
mkVerifyTrustResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  VerifyTrustResponse
mkVerifyTrustResponse pResponseStatus_ =
  VerifyTrustResponse'
    { trustId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique Trust ID of the trust relationship that was verified.
--
-- /Note:/ Consider using 'trustId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtrsTrustId :: Lens.Lens' VerifyTrustResponse (Lude.Maybe Lude.Text)
vtrsTrustId = Lens.lens (trustId :: VerifyTrustResponse -> Lude.Maybe Lude.Text) (\s a -> s {trustId = a} :: VerifyTrustResponse)
{-# DEPRECATED vtrsTrustId "Use generic-lens or generic-optics with 'trustId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtrsResponseStatus :: Lens.Lens' VerifyTrustResponse Lude.Int
vtrsResponseStatus = Lens.lens (responseStatus :: VerifyTrustResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: VerifyTrustResponse)
{-# DEPRECATED vtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
