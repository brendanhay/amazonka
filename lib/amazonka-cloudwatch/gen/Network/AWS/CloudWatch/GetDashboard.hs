{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.GetDashboard
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays the details of the dashboard that you specify.
--
-- To copy an existing dashboard, use @GetDashboard@ , and then use the data returned within @DashboardBody@ as the template for the new dashboard when you call @PutDashboard@ to create the copy.
module Network.AWS.CloudWatch.GetDashboard
  ( -- * Creating a request
    GetDashboard (..),
    mkGetDashboard,

    -- ** Request lenses
    gdDashboardName,

    -- * Destructuring the response
    GetDashboardResponse (..),
    mkGetDashboardResponse,

    -- ** Response lenses
    gdrsDashboardName,
    gdrsDashboardBody,
    gdrsDashboardARN,
    gdrsResponseStatus,
  )
where

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetDashboard' smart constructor.
newtype GetDashboard = GetDashboard' {dashboardName :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDashboard' with the minimum fields required to make a request.
--
-- * 'dashboardName' - The name of the dashboard to be described.
mkGetDashboard ::
  -- | 'dashboardName'
  Lude.Text ->
  GetDashboard
mkGetDashboard pDashboardName_ =
  GetDashboard' {dashboardName = pDashboardName_}

-- | The name of the dashboard to be described.
--
-- /Note:/ Consider using 'dashboardName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdDashboardName :: Lens.Lens' GetDashboard Lude.Text
gdDashboardName = Lens.lens (dashboardName :: GetDashboard -> Lude.Text) (\s a -> s {dashboardName = a} :: GetDashboard)
{-# DEPRECATED gdDashboardName "Use generic-lens or generic-optics with 'dashboardName' instead." #-}

instance Lude.AWSRequest GetDashboard where
  type Rs GetDashboard = GetDashboardResponse
  request = Req.postQuery cloudWatchService
  response =
    Res.receiveXMLWrapper
      "GetDashboardResult"
      ( \s h x ->
          GetDashboardResponse'
            Lude.<$> (x Lude..@? "DashboardName")
            Lude.<*> (x Lude..@? "DashboardBody")
            Lude.<*> (x Lude..@? "DashboardArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDashboard where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetDashboard where
  toPath = Lude.const "/"

instance Lude.ToQuery GetDashboard where
  toQuery GetDashboard' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("GetDashboard" :: Lude.ByteString),
        "Version" Lude.=: ("2010-08-01" :: Lude.ByteString),
        "DashboardName" Lude.=: dashboardName
      ]

-- | /See:/ 'mkGetDashboardResponse' smart constructor.
data GetDashboardResponse = GetDashboardResponse'
  { dashboardName ::
      Lude.Maybe Lude.Text,
    dashboardBody :: Lude.Maybe Lude.Text,
    dashboardARN :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'GetDashboardResponse' with the minimum fields required to make a request.
--
-- * 'dashboardARN' - The Amazon Resource Name (ARN) of the dashboard.
-- * 'dashboardBody' - The detailed information about the dashboard, including what widgets are included and their location on the dashboard. For more information about the @DashboardBody@ syntax, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/CloudWatch-Dashboard-Body-Structure.html Dashboard Body Structure and Syntax> .
-- * 'dashboardName' - The name of the dashboard.
-- * 'responseStatus' - The response status code.
mkGetDashboardResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDashboardResponse
mkGetDashboardResponse pResponseStatus_ =
  GetDashboardResponse'
    { dashboardName = Lude.Nothing,
      dashboardBody = Lude.Nothing,
      dashboardARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The name of the dashboard.
--
-- /Note:/ Consider using 'dashboardName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsDashboardName :: Lens.Lens' GetDashboardResponse (Lude.Maybe Lude.Text)
gdrsDashboardName = Lens.lens (dashboardName :: GetDashboardResponse -> Lude.Maybe Lude.Text) (\s a -> s {dashboardName = a} :: GetDashboardResponse)
{-# DEPRECATED gdrsDashboardName "Use generic-lens or generic-optics with 'dashboardName' instead." #-}

-- | The detailed information about the dashboard, including what widgets are included and their location on the dashboard. For more information about the @DashboardBody@ syntax, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/CloudWatch-Dashboard-Body-Structure.html Dashboard Body Structure and Syntax> .
--
-- /Note:/ Consider using 'dashboardBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsDashboardBody :: Lens.Lens' GetDashboardResponse (Lude.Maybe Lude.Text)
gdrsDashboardBody = Lens.lens (dashboardBody :: GetDashboardResponse -> Lude.Maybe Lude.Text) (\s a -> s {dashboardBody = a} :: GetDashboardResponse)
{-# DEPRECATED gdrsDashboardBody "Use generic-lens or generic-optics with 'dashboardBody' instead." #-}

-- | The Amazon Resource Name (ARN) of the dashboard.
--
-- /Note:/ Consider using 'dashboardARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsDashboardARN :: Lens.Lens' GetDashboardResponse (Lude.Maybe Lude.Text)
gdrsDashboardARN = Lens.lens (dashboardARN :: GetDashboardResponse -> Lude.Maybe Lude.Text) (\s a -> s {dashboardARN = a} :: GetDashboardResponse)
{-# DEPRECATED gdrsDashboardARN "Use generic-lens or generic-optics with 'dashboardARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsResponseStatus :: Lens.Lens' GetDashboardResponse Lude.Int
gdrsResponseStatus = Lens.lens (responseStatus :: GetDashboardResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDashboardResponse)
{-# DEPRECATED gdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
