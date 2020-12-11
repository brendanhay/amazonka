{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostAndUsageReport.PutReportDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new report using the description that you provide.
module Network.AWS.CostAndUsageReport.PutReportDefinition
  ( -- * Creating a request
    PutReportDefinition (..),
    mkPutReportDefinition,

    -- ** Request lenses
    prdReportDefinition,

    -- * Destructuring the response
    PutReportDefinitionResponse (..),
    mkPutReportDefinitionResponse,

    -- ** Response lenses
    prdrsResponseStatus,
  )
where

import Network.AWS.CostAndUsageReport.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Creates a Cost and Usage Report.
--
-- /See:/ 'mkPutReportDefinition' smart constructor.
newtype PutReportDefinition = PutReportDefinition'
  { reportDefinition ::
      ReportDefinition
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutReportDefinition' with the minimum fields required to make a request.
--
-- * 'reportDefinition' - Represents the output of the PutReportDefinition operation. The content consists of the detailed metadata and data file information.
mkPutReportDefinition ::
  -- | 'reportDefinition'
  ReportDefinition ->
  PutReportDefinition
mkPutReportDefinition pReportDefinition_ =
  PutReportDefinition' {reportDefinition = pReportDefinition_}

-- | Represents the output of the PutReportDefinition operation. The content consists of the detailed metadata and data file information.
--
-- /Note:/ Consider using 'reportDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prdReportDefinition :: Lens.Lens' PutReportDefinition ReportDefinition
prdReportDefinition = Lens.lens (reportDefinition :: PutReportDefinition -> ReportDefinition) (\s a -> s {reportDefinition = a} :: PutReportDefinition)
{-# DEPRECATED prdReportDefinition "Use generic-lens or generic-optics with 'reportDefinition' instead." #-}

instance Lude.AWSRequest PutReportDefinition where
  type Rs PutReportDefinition = PutReportDefinitionResponse
  request = Req.postJSON costAndUsageReportService
  response =
    Res.receiveEmpty
      ( \s h x ->
          PutReportDefinitionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutReportDefinition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSOrigamiServiceGatewayService.PutReportDefinition" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutReportDefinition where
  toJSON PutReportDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ReportDefinition" Lude..= reportDefinition)]
      )

instance Lude.ToPath PutReportDefinition where
  toPath = Lude.const "/"

instance Lude.ToQuery PutReportDefinition where
  toQuery = Lude.const Lude.mempty

-- | If the action is successful, the service sends back an HTTP 200 response with an empty HTTP body.
--
-- /See:/ 'mkPutReportDefinitionResponse' smart constructor.
newtype PutReportDefinitionResponse = PutReportDefinitionResponse'
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

-- | Creates a value of 'PutReportDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkPutReportDefinitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutReportDefinitionResponse
mkPutReportDefinitionResponse pResponseStatus_ =
  PutReportDefinitionResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prdrsResponseStatus :: Lens.Lens' PutReportDefinitionResponse Lude.Int
prdrsResponseStatus = Lens.lens (responseStatus :: PutReportDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutReportDefinitionResponse)
{-# DEPRECATED prdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
