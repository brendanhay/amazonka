{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostAndUsageReport.ModifyReportDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows you to programatically update your report preferences.
module Network.AWS.CostAndUsageReport.ModifyReportDefinition
  ( -- * Creating a request
    ModifyReportDefinition (..),
    mkModifyReportDefinition,

    -- ** Request lenses
    mrdReportName,
    mrdReportDefinition,

    -- * Destructuring the response
    ModifyReportDefinitionResponse (..),
    mkModifyReportDefinitionResponse,

    -- ** Response lenses
    mrdrsResponseStatus,
  )
where

import Network.AWS.CostAndUsageReport.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyReportDefinition' smart constructor.
data ModifyReportDefinition = ModifyReportDefinition'
  { reportName :: Lude.Text,
    reportDefinition :: ReportDefinition
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyReportDefinition' with the minimum fields required to make a request.
--
-- * 'reportName' -
-- * 'reportDefinition' -
mkModifyReportDefinition ::
  -- | 'reportName'
  Lude.Text ->
  -- | 'reportDefinition'
  ReportDefinition ->
  ModifyReportDefinition
mkModifyReportDefinition pReportName_ pReportDefinition_ =
  ModifyReportDefinition'
    { reportName = pReportName_,
      reportDefinition = pReportDefinition_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'reportName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrdReportName :: Lens.Lens' ModifyReportDefinition Lude.Text
mrdReportName = Lens.lens (reportName :: ModifyReportDefinition -> Lude.Text) (\s a -> s {reportName = a} :: ModifyReportDefinition)
{-# DEPRECATED mrdReportName "Use generic-lens or generic-optics with 'reportName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'reportDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrdReportDefinition :: Lens.Lens' ModifyReportDefinition ReportDefinition
mrdReportDefinition = Lens.lens (reportDefinition :: ModifyReportDefinition -> ReportDefinition) (\s a -> s {reportDefinition = a} :: ModifyReportDefinition)
{-# DEPRECATED mrdReportDefinition "Use generic-lens or generic-optics with 'reportDefinition' instead." #-}

instance Lude.AWSRequest ModifyReportDefinition where
  type Rs ModifyReportDefinition = ModifyReportDefinitionResponse
  request = Req.postJSON costAndUsageReportService
  response =
    Res.receiveEmpty
      ( \s h x ->
          ModifyReportDefinitionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyReportDefinition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSOrigamiServiceGatewayService.ModifyReportDefinition" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ModifyReportDefinition where
  toJSON ModifyReportDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ReportName" Lude..= reportName),
            Lude.Just ("ReportDefinition" Lude..= reportDefinition)
          ]
      )

instance Lude.ToPath ModifyReportDefinition where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyReportDefinition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkModifyReportDefinitionResponse' smart constructor.
newtype ModifyReportDefinitionResponse = ModifyReportDefinitionResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyReportDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkModifyReportDefinitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyReportDefinitionResponse
mkModifyReportDefinitionResponse pResponseStatus_ =
  ModifyReportDefinitionResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrdrsResponseStatus :: Lens.Lens' ModifyReportDefinitionResponse Lude.Int
mrdrsResponseStatus = Lens.lens (responseStatus :: ModifyReportDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyReportDefinitionResponse)
{-# DEPRECATED mrdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
