{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.ResolveCase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resolves a support case. This operation takes a @caseId@ and returns the initial and final state of the case.
module Network.AWS.Support.ResolveCase
  ( -- * Creating a request
    ResolveCase (..),
    mkResolveCase,

    -- ** Request lenses
    rcCaseId,

    -- * Destructuring the response
    ResolveCaseResponse (..),
    mkResolveCaseResponse,

    -- ** Response lenses
    rcrsInitialCaseStatus,
    rcrsFinalCaseStatus,
    rcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Support.Types

-- | /See:/ 'mkResolveCase' smart constructor.
newtype ResolveCase = ResolveCase' {caseId :: Lude.Maybe Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResolveCase' with the minimum fields required to make a request.
--
-- * 'caseId' - The AWS Support case ID requested or returned in the call. The case ID is an alphanumeric string formatted as shown in this example: case-/12345678910-2013-c4c1d2bf33c5cf47/
mkResolveCase ::
  ResolveCase
mkResolveCase = ResolveCase' {caseId = Lude.Nothing}

-- | The AWS Support case ID requested or returned in the call. The case ID is an alphanumeric string formatted as shown in this example: case-/12345678910-2013-c4c1d2bf33c5cf47/
--
-- /Note:/ Consider using 'caseId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcCaseId :: Lens.Lens' ResolveCase (Lude.Maybe Lude.Text)
rcCaseId = Lens.lens (caseId :: ResolveCase -> Lude.Maybe Lude.Text) (\s a -> s {caseId = a} :: ResolveCase)
{-# DEPRECATED rcCaseId "Use generic-lens or generic-optics with 'caseId' instead." #-}

instance Lude.AWSRequest ResolveCase where
  type Rs ResolveCase = ResolveCaseResponse
  request = Req.postJSON supportService
  response =
    Res.receiveJSON
      ( \s h x ->
          ResolveCaseResponse'
            Lude.<$> (x Lude..?> "initialCaseStatus")
            Lude.<*> (x Lude..?> "finalCaseStatus")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ResolveCase where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSSupport_20130415.ResolveCase" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ResolveCase where
  toJSON ResolveCase' {..} =
    Lude.object (Lude.catMaybes [("caseId" Lude..=) Lude.<$> caseId])

instance Lude.ToPath ResolveCase where
  toPath = Lude.const "/"

instance Lude.ToQuery ResolveCase where
  toQuery = Lude.const Lude.mempty

-- | The status of the case returned by the 'ResolveCase' operation.
--
-- /See:/ 'mkResolveCaseResponse' smart constructor.
data ResolveCaseResponse = ResolveCaseResponse'
  { initialCaseStatus ::
      Lude.Maybe Lude.Text,
    finalCaseStatus :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ResolveCaseResponse' with the minimum fields required to make a request.
--
-- * 'finalCaseStatus' - The status of the case after the 'ResolveCase' request was processed.
-- * 'initialCaseStatus' - The status of the case when the 'ResolveCase' request was sent.
-- * 'responseStatus' - The response status code.
mkResolveCaseResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ResolveCaseResponse
mkResolveCaseResponse pResponseStatus_ =
  ResolveCaseResponse'
    { initialCaseStatus = Lude.Nothing,
      finalCaseStatus = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the case when the 'ResolveCase' request was sent.
--
-- /Note:/ Consider using 'initialCaseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrsInitialCaseStatus :: Lens.Lens' ResolveCaseResponse (Lude.Maybe Lude.Text)
rcrsInitialCaseStatus = Lens.lens (initialCaseStatus :: ResolveCaseResponse -> Lude.Maybe Lude.Text) (\s a -> s {initialCaseStatus = a} :: ResolveCaseResponse)
{-# DEPRECATED rcrsInitialCaseStatus "Use generic-lens or generic-optics with 'initialCaseStatus' instead." #-}

-- | The status of the case after the 'ResolveCase' request was processed.
--
-- /Note:/ Consider using 'finalCaseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrsFinalCaseStatus :: Lens.Lens' ResolveCaseResponse (Lude.Maybe Lude.Text)
rcrsFinalCaseStatus = Lens.lens (finalCaseStatus :: ResolveCaseResponse -> Lude.Maybe Lude.Text) (\s a -> s {finalCaseStatus = a} :: ResolveCaseResponse)
{-# DEPRECATED rcrsFinalCaseStatus "Use generic-lens or generic-optics with 'finalCaseStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrsResponseStatus :: Lens.Lens' ResolveCaseResponse Lude.Int
rcrsResponseStatus = Lens.lens (responseStatus :: ResolveCaseResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ResolveCaseResponse)
{-# DEPRECATED rcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
