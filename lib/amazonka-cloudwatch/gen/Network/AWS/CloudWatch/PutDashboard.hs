{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.PutDashboard
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a dashboard if it does not already exist, or updates an existing dashboard. If you update a dashboard, the entire contents are replaced with what you specify here.
--
-- All dashboards in your account are global, not region-specific.
-- A simple way to create a dashboard using @PutDashboard@ is to copy an existing dashboard. To copy an existing dashboard using the console, you can load the dashboard and then use the View/edit source command in the Actions menu to display the JSON block for that dashboard. Another way to copy a dashboard is to use @GetDashboard@ , and then use the data returned within @DashboardBody@ as the template for the new dashboard when you call @PutDashboard@ .
-- When you create a dashboard with @PutDashboard@ , a good practice is to add a text widget at the top of the dashboard with a message that the dashboard was created by script and should not be changed in the console. This message could also point console users to the location of the @DashboardBody@ script or the CloudFormation template used to create the dashboard.
module Network.AWS.CloudWatch.PutDashboard
  ( -- * Creating a request
    PutDashboard (..),
    mkPutDashboard,

    -- ** Request lenses
    pdDashboardName,
    pdDashboardBody,

    -- * Destructuring the response
    PutDashboardResponse (..),
    mkPutDashboardResponse,

    -- ** Response lenses
    pdrsDashboardValidationMessages,
    pdrsResponseStatus,
  )
where

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutDashboard' smart constructor.
data PutDashboard = PutDashboard'
  { -- | The name of the dashboard. If a dashboard with this name already exists, this call modifies that dashboard, replacing its current contents. Otherwise, a new dashboard is created. The maximum length is 255, and valid characters are A-Z, a-z, 0-9, "-", and "_". This parameter is required.
    dashboardName :: Lude.Text,
    -- | The detailed information about the dashboard in JSON format, including the widgets to include and their location on the dashboard. This parameter is required.
    --
    -- For more information about the syntax, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/CloudWatch-Dashboard-Body-Structure.html Dashboard Body Structure and Syntax> .
    dashboardBody :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutDashboard' with the minimum fields required to make a request.
--
-- * 'dashboardName' - The name of the dashboard. If a dashboard with this name already exists, this call modifies that dashboard, replacing its current contents. Otherwise, a new dashboard is created. The maximum length is 255, and valid characters are A-Z, a-z, 0-9, "-", and "_". This parameter is required.
-- * 'dashboardBody' - The detailed information about the dashboard in JSON format, including the widgets to include and their location on the dashboard. This parameter is required.
--
-- For more information about the syntax, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/CloudWatch-Dashboard-Body-Structure.html Dashboard Body Structure and Syntax> .
mkPutDashboard ::
  -- | 'dashboardName'
  Lude.Text ->
  -- | 'dashboardBody'
  Lude.Text ->
  PutDashboard
mkPutDashboard pDashboardName_ pDashboardBody_ =
  PutDashboard'
    { dashboardName = pDashboardName_,
      dashboardBody = pDashboardBody_
    }

-- | The name of the dashboard. If a dashboard with this name already exists, this call modifies that dashboard, replacing its current contents. Otherwise, a new dashboard is created. The maximum length is 255, and valid characters are A-Z, a-z, 0-9, "-", and "_". This parameter is required.
--
-- /Note:/ Consider using 'dashboardName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdDashboardName :: Lens.Lens' PutDashboard Lude.Text
pdDashboardName = Lens.lens (dashboardName :: PutDashboard -> Lude.Text) (\s a -> s {dashboardName = a} :: PutDashboard)
{-# DEPRECATED pdDashboardName "Use generic-lens or generic-optics with 'dashboardName' instead." #-}

-- | The detailed information about the dashboard in JSON format, including the widgets to include and their location on the dashboard. This parameter is required.
--
-- For more information about the syntax, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/CloudWatch-Dashboard-Body-Structure.html Dashboard Body Structure and Syntax> .
--
-- /Note:/ Consider using 'dashboardBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdDashboardBody :: Lens.Lens' PutDashboard Lude.Text
pdDashboardBody = Lens.lens (dashboardBody :: PutDashboard -> Lude.Text) (\s a -> s {dashboardBody = a} :: PutDashboard)
{-# DEPRECATED pdDashboardBody "Use generic-lens or generic-optics with 'dashboardBody' instead." #-}

instance Lude.AWSRequest PutDashboard where
  type Rs PutDashboard = PutDashboardResponse
  request = Req.postQuery cloudWatchService
  response =
    Res.receiveXMLWrapper
      "PutDashboardResult"
      ( \s h x ->
          PutDashboardResponse'
            Lude.<$> ( x Lude..@? "DashboardValidationMessages" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutDashboard where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath PutDashboard where
  toPath = Lude.const "/"

instance Lude.ToQuery PutDashboard where
  toQuery PutDashboard' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("PutDashboard" :: Lude.ByteString),
        "Version" Lude.=: ("2010-08-01" :: Lude.ByteString),
        "DashboardName" Lude.=: dashboardName,
        "DashboardBody" Lude.=: dashboardBody
      ]

-- | /See:/ 'mkPutDashboardResponse' smart constructor.
data PutDashboardResponse = PutDashboardResponse'
  { -- | If the input for @PutDashboard@ was correct and the dashboard was successfully created or modified, this result is empty.
    --
    -- If this result includes only warning messages, then the input was valid enough for the dashboard to be created or modified, but some elements of the dashboard might not render.
    -- If this result includes error messages, the input was not valid and the operation failed.
    dashboardValidationMessages :: Lude.Maybe [DashboardValidationMessage],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutDashboardResponse' with the minimum fields required to make a request.
--
-- * 'dashboardValidationMessages' - If the input for @PutDashboard@ was correct and the dashboard was successfully created or modified, this result is empty.
--
-- If this result includes only warning messages, then the input was valid enough for the dashboard to be created or modified, but some elements of the dashboard might not render.
-- If this result includes error messages, the input was not valid and the operation failed.
-- * 'responseStatus' - The response status code.
mkPutDashboardResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutDashboardResponse
mkPutDashboardResponse pResponseStatus_ =
  PutDashboardResponse'
    { dashboardValidationMessages = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If the input for @PutDashboard@ was correct and the dashboard was successfully created or modified, this result is empty.
--
-- If this result includes only warning messages, then the input was valid enough for the dashboard to be created or modified, but some elements of the dashboard might not render.
-- If this result includes error messages, the input was not valid and the operation failed.
--
-- /Note:/ Consider using 'dashboardValidationMessages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdrsDashboardValidationMessages :: Lens.Lens' PutDashboardResponse (Lude.Maybe [DashboardValidationMessage])
pdrsDashboardValidationMessages = Lens.lens (dashboardValidationMessages :: PutDashboardResponse -> Lude.Maybe [DashboardValidationMessage]) (\s a -> s {dashboardValidationMessages = a} :: PutDashboardResponse)
{-# DEPRECATED pdrsDashboardValidationMessages "Use generic-lens or generic-optics with 'dashboardValidationMessages' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdrsResponseStatus :: Lens.Lens' PutDashboardResponse Lude.Int
pdrsResponseStatus = Lens.lens (responseStatus :: PutDashboardResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutDashboardResponse)
{-# DEPRECATED pdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
