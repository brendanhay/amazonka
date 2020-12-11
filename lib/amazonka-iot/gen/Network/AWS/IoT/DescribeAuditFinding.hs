{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeAuditFinding
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a single audit finding. Properties include the reason for noncompliance, the severity of the issue, and when the audit that returned the finding was started.
module Network.AWS.IoT.DescribeAuditFinding
  ( -- * Creating a request
    DescribeAuditFinding (..),
    mkDescribeAuditFinding,

    -- ** Request lenses
    dafFindingId,

    -- * Destructuring the response
    DescribeAuditFindingResponse (..),
    mkDescribeAuditFindingResponse,

    -- ** Response lenses
    dafrsFinding,
    dafrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeAuditFinding' smart constructor.
newtype DescribeAuditFinding = DescribeAuditFinding'
  { findingId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAuditFinding' with the minimum fields required to make a request.
--
-- * 'findingId' - A unique identifier for a single audit finding. You can use this identifier to apply mitigation actions to the finding.
mkDescribeAuditFinding ::
  -- | 'findingId'
  Lude.Text ->
  DescribeAuditFinding
mkDescribeAuditFinding pFindingId_ =
  DescribeAuditFinding' {findingId = pFindingId_}

-- | A unique identifier for a single audit finding. You can use this identifier to apply mitigation actions to the finding.
--
-- /Note:/ Consider using 'findingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafFindingId :: Lens.Lens' DescribeAuditFinding Lude.Text
dafFindingId = Lens.lens (findingId :: DescribeAuditFinding -> Lude.Text) (\s a -> s {findingId = a} :: DescribeAuditFinding)
{-# DEPRECATED dafFindingId "Use generic-lens or generic-optics with 'findingId' instead." #-}

instance Lude.AWSRequest DescribeAuditFinding where
  type Rs DescribeAuditFinding = DescribeAuditFindingResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeAuditFindingResponse'
            Lude.<$> (x Lude..?> "finding") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAuditFinding where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeAuditFinding where
  toPath DescribeAuditFinding' {..} =
    Lude.mconcat ["/audit/findings/", Lude.toBS findingId]

instance Lude.ToQuery DescribeAuditFinding where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeAuditFindingResponse' smart constructor.
data DescribeAuditFindingResponse = DescribeAuditFindingResponse'
  { finding ::
      Lude.Maybe AuditFinding,
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

-- | Creates a value of 'DescribeAuditFindingResponse' with the minimum fields required to make a request.
--
-- * 'finding' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkDescribeAuditFindingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAuditFindingResponse
mkDescribeAuditFindingResponse pResponseStatus_ =
  DescribeAuditFindingResponse'
    { finding = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'finding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafrsFinding :: Lens.Lens' DescribeAuditFindingResponse (Lude.Maybe AuditFinding)
dafrsFinding = Lens.lens (finding :: DescribeAuditFindingResponse -> Lude.Maybe AuditFinding) (\s a -> s {finding = a} :: DescribeAuditFindingResponse)
{-# DEPRECATED dafrsFinding "Use generic-lens or generic-optics with 'finding' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafrsResponseStatus :: Lens.Lens' DescribeAuditFindingResponse Lude.Int
dafrsResponseStatus = Lens.lens (responseStatus :: DescribeAuditFindingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAuditFindingResponse)
{-# DEPRECATED dafrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
