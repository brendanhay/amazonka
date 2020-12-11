{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostAndUsageReport.DeleteReportDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified report.
module Network.AWS.CostAndUsageReport.DeleteReportDefinition
  ( -- * Creating a request
    DeleteReportDefinition (..),
    mkDeleteReportDefinition,

    -- ** Request lenses
    drdReportName,

    -- * Destructuring the response
    DeleteReportDefinitionResponse (..),
    mkDeleteReportDefinitionResponse,

    -- ** Response lenses
    drsResponseMessage,
    drsResponseStatus,
  )
where

import Network.AWS.CostAndUsageReport.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Deletes the specified report.
--
-- /See:/ 'mkDeleteReportDefinition' smart constructor.
newtype DeleteReportDefinition = DeleteReportDefinition'
  { reportName ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteReportDefinition' with the minimum fields required to make a request.
--
-- * 'reportName' - The name of the report that you want to delete. The name must be unique, is case sensitive, and can't include spaces.
mkDeleteReportDefinition ::
  DeleteReportDefinition
mkDeleteReportDefinition =
  DeleteReportDefinition' {reportName = Lude.Nothing}

-- | The name of the report that you want to delete. The name must be unique, is case sensitive, and can't include spaces.
--
-- /Note:/ Consider using 'reportName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdReportName :: Lens.Lens' DeleteReportDefinition (Lude.Maybe Lude.Text)
drdReportName = Lens.lens (reportName :: DeleteReportDefinition -> Lude.Maybe Lude.Text) (\s a -> s {reportName = a} :: DeleteReportDefinition)
{-# DEPRECATED drdReportName "Use generic-lens or generic-optics with 'reportName' instead." #-}

instance Lude.AWSRequest DeleteReportDefinition where
  type Rs DeleteReportDefinition = DeleteReportDefinitionResponse
  request = Req.postJSON costAndUsageReportService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteReportDefinitionResponse'
            Lude.<$> (x Lude..?> "ResponseMessage")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteReportDefinition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSOrigamiServiceGatewayService.DeleteReportDefinition" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteReportDefinition where
  toJSON DeleteReportDefinition' {..} =
    Lude.object
      (Lude.catMaybes [("ReportName" Lude..=) Lude.<$> reportName])

instance Lude.ToPath DeleteReportDefinition where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteReportDefinition where
  toQuery = Lude.const Lude.mempty

-- | If the action is successful, the service sends back an HTTP 200 response.
--
-- /See:/ 'mkDeleteReportDefinitionResponse' smart constructor.
data DeleteReportDefinitionResponse = DeleteReportDefinitionResponse'
  { responseMessage ::
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

-- | Creates a value of 'DeleteReportDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'responseMessage' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkDeleteReportDefinitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteReportDefinitionResponse
mkDeleteReportDefinitionResponse pResponseStatus_ =
  DeleteReportDefinitionResponse'
    { responseMessage = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'responseMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseMessage :: Lens.Lens' DeleteReportDefinitionResponse (Lude.Maybe Lude.Text)
drsResponseMessage = Lens.lens (responseMessage :: DeleteReportDefinitionResponse -> Lude.Maybe Lude.Text) (\s a -> s {responseMessage = a} :: DeleteReportDefinitionResponse)
{-# DEPRECATED drsResponseMessage "Use generic-lens or generic-optics with 'responseMessage' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteReportDefinitionResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DeleteReportDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteReportDefinitionResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
