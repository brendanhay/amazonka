{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeApps
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a description of a specified set of apps.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DescribeApps
  ( -- * Creating a request
    DescribeApps (..),
    mkDescribeApps,

    -- ** Request lenses
    daAppIds,
    daStackId,

    -- * Destructuring the response
    DescribeAppsResponse (..),
    mkDescribeAppsResponse,

    -- ** Response lenses
    darsApps,
    darsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeApps' smart constructor.
data DescribeApps = DescribeApps'
  { appIds :: Lude.Maybe [Lude.Text],
    stackId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeApps' with the minimum fields required to make a request.
--
-- * 'appIds' - An array of app IDs for the apps to be described. If you use this parameter, @DescribeApps@ returns a description of the specified apps. Otherwise, it returns a description of every app.
-- * 'stackId' - The app stack ID. If you use this parameter, @DescribeApps@ returns a description of the apps in the specified stack.
mkDescribeApps ::
  DescribeApps
mkDescribeApps =
  DescribeApps' {appIds = Lude.Nothing, stackId = Lude.Nothing}

-- | An array of app IDs for the apps to be described. If you use this parameter, @DescribeApps@ returns a description of the specified apps. Otherwise, it returns a description of every app.
--
-- /Note:/ Consider using 'appIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAppIds :: Lens.Lens' DescribeApps (Lude.Maybe [Lude.Text])
daAppIds = Lens.lens (appIds :: DescribeApps -> Lude.Maybe [Lude.Text]) (\s a -> s {appIds = a} :: DescribeApps)
{-# DEPRECATED daAppIds "Use generic-lens or generic-optics with 'appIds' instead." #-}

-- | The app stack ID. If you use this parameter, @DescribeApps@ returns a description of the apps in the specified stack.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daStackId :: Lens.Lens' DescribeApps (Lude.Maybe Lude.Text)
daStackId = Lens.lens (stackId :: DescribeApps -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: DescribeApps)
{-# DEPRECATED daStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

instance Lude.AWSRequest DescribeApps where
  type Rs DescribeApps = DescribeAppsResponse
  request = Req.postJSON opsWorksService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeAppsResponse'
            Lude.<$> (x Lude..?> "Apps" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeApps where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.DescribeApps" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeApps where
  toJSON DescribeApps' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AppIds" Lude..=) Lude.<$> appIds,
            ("StackId" Lude..=) Lude.<$> stackId
          ]
      )

instance Lude.ToPath DescribeApps where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeApps where
  toQuery = Lude.const Lude.mempty

-- | Contains the response to a @DescribeApps@ request.
--
-- /See:/ 'mkDescribeAppsResponse' smart constructor.
data DescribeAppsResponse = DescribeAppsResponse'
  { apps ::
      Lude.Maybe [App],
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

-- | Creates a value of 'DescribeAppsResponse' with the minimum fields required to make a request.
--
-- * 'apps' - An array of @App@ objects that describe the specified apps.
-- * 'responseStatus' - The response status code.
mkDescribeAppsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAppsResponse
mkDescribeAppsResponse pResponseStatus_ =
  DescribeAppsResponse'
    { apps = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of @App@ objects that describe the specified apps.
--
-- /Note:/ Consider using 'apps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsApps :: Lens.Lens' DescribeAppsResponse (Lude.Maybe [App])
darsApps = Lens.lens (apps :: DescribeAppsResponse -> Lude.Maybe [App]) (\s a -> s {apps = a} :: DescribeAppsResponse)
{-# DEPRECATED darsApps "Use generic-lens or generic-optics with 'apps' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsResponseStatus :: Lens.Lens' DescribeAppsResponse Lude.Int
darsResponseStatus = Lens.lens (responseStatus :: DescribeAppsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAppsResponse)
{-# DEPRECATED darsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
