{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeApp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the app.
module Network.AWS.SageMaker.DescribeApp
  ( -- * Creating a request
    DescribeApp (..),
    mkDescribeApp,

    -- ** Request lenses
    daDomainId,
    daUserProfileName,
    daAppType,
    daAppName,

    -- * Destructuring the response
    DescribeAppResponse (..),
    mkDescribeAppResponse,

    -- ** Response lenses
    darsCreationTime,
    darsStatus,
    darsFailureReason,
    darsResourceSpec,
    darsUserProfileName,
    darsLastUserActivityTimestamp,
    darsLastHealthCheckTimestamp,
    darsAppName,
    darsAppARN,
    darsDomainId,
    darsAppType,
    darsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDescribeApp' smart constructor.
data DescribeApp = DescribeApp'
  { domainId :: Lude.Text,
    userProfileName :: Lude.Text,
    appType :: AppType,
    appName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeApp' with the minimum fields required to make a request.
--
-- * 'appName' - The name of the app.
-- * 'appType' - The type of app.
-- * 'domainId' - The domain ID.
-- * 'userProfileName' - The user profile name.
mkDescribeApp ::
  -- | 'domainId'
  Lude.Text ->
  -- | 'userProfileName'
  Lude.Text ->
  -- | 'appType'
  AppType ->
  -- | 'appName'
  Lude.Text ->
  DescribeApp
mkDescribeApp pDomainId_ pUserProfileName_ pAppType_ pAppName_ =
  DescribeApp'
    { domainId = pDomainId_,
      userProfileName = pUserProfileName_,
      appType = pAppType_,
      appName = pAppName_
    }

-- | The domain ID.
--
-- /Note:/ Consider using 'domainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daDomainId :: Lens.Lens' DescribeApp Lude.Text
daDomainId = Lens.lens (domainId :: DescribeApp -> Lude.Text) (\s a -> s {domainId = a} :: DescribeApp)
{-# DEPRECATED daDomainId "Use generic-lens or generic-optics with 'domainId' instead." #-}

-- | The user profile name.
--
-- /Note:/ Consider using 'userProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daUserProfileName :: Lens.Lens' DescribeApp Lude.Text
daUserProfileName = Lens.lens (userProfileName :: DescribeApp -> Lude.Text) (\s a -> s {userProfileName = a} :: DescribeApp)
{-# DEPRECATED daUserProfileName "Use generic-lens or generic-optics with 'userProfileName' instead." #-}

-- | The type of app.
--
-- /Note:/ Consider using 'appType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAppType :: Lens.Lens' DescribeApp AppType
daAppType = Lens.lens (appType :: DescribeApp -> AppType) (\s a -> s {appType = a} :: DescribeApp)
{-# DEPRECATED daAppType "Use generic-lens or generic-optics with 'appType' instead." #-}

-- | The name of the app.
--
-- /Note:/ Consider using 'appName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAppName :: Lens.Lens' DescribeApp Lude.Text
daAppName = Lens.lens (appName :: DescribeApp -> Lude.Text) (\s a -> s {appName = a} :: DescribeApp)
{-# DEPRECATED daAppName "Use generic-lens or generic-optics with 'appName' instead." #-}

instance Lude.AWSRequest DescribeApp where
  type Rs DescribeApp = DescribeAppResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeAppResponse'
            Lude.<$> (x Lude..?> "CreationTime")
            Lude.<*> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "FailureReason")
            Lude.<*> (x Lude..?> "ResourceSpec")
            Lude.<*> (x Lude..?> "UserProfileName")
            Lude.<*> (x Lude..?> "LastUserActivityTimestamp")
            Lude.<*> (x Lude..?> "LastHealthCheckTimestamp")
            Lude.<*> (x Lude..?> "AppName")
            Lude.<*> (x Lude..?> "AppArn")
            Lude.<*> (x Lude..?> "DomainId")
            Lude.<*> (x Lude..?> "AppType")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeApp where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DescribeApp" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeApp where
  toJSON DescribeApp' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DomainId" Lude..= domainId),
            Lude.Just ("UserProfileName" Lude..= userProfileName),
            Lude.Just ("AppType" Lude..= appType),
            Lude.Just ("AppName" Lude..= appName)
          ]
      )

instance Lude.ToPath DescribeApp where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeApp where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeAppResponse' smart constructor.
data DescribeAppResponse = DescribeAppResponse'
  { creationTime ::
      Lude.Maybe Lude.Timestamp,
    status :: Lude.Maybe AppStatus,
    failureReason :: Lude.Maybe Lude.Text,
    resourceSpec :: Lude.Maybe ResourceSpec,
    userProfileName :: Lude.Maybe Lude.Text,
    lastUserActivityTimestamp ::
      Lude.Maybe Lude.Timestamp,
    lastHealthCheckTimestamp ::
      Lude.Maybe Lude.Timestamp,
    appName :: Lude.Maybe Lude.Text,
    appARN :: Lude.Maybe Lude.Text,
    domainId :: Lude.Maybe Lude.Text,
    appType :: Lude.Maybe AppType,
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

-- | Creates a value of 'DescribeAppResponse' with the minimum fields required to make a request.
--
-- * 'appARN' - The Amazon Resource Name (ARN) of the app.
-- * 'appName' - The name of the app.
-- * 'appType' - The type of app.
-- * 'creationTime' - The creation time.
-- * 'domainId' - The domain ID.
-- * 'failureReason' - The failure reason.
-- * 'lastHealthCheckTimestamp' - The timestamp of the last health check.
-- * 'lastUserActivityTimestamp' - The timestamp of the last user's activity.
-- * 'resourceSpec' - The instance type and the Amazon Resource Name (ARN) of the SageMaker image created on the instance.
-- * 'responseStatus' - The response status code.
-- * 'status' - The status.
-- * 'userProfileName' - The user profile name.
mkDescribeAppResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAppResponse
mkDescribeAppResponse pResponseStatus_ =
  DescribeAppResponse'
    { creationTime = Lude.Nothing,
      status = Lude.Nothing,
      failureReason = Lude.Nothing,
      resourceSpec = Lude.Nothing,
      userProfileName = Lude.Nothing,
      lastUserActivityTimestamp = Lude.Nothing,
      lastHealthCheckTimestamp = Lude.Nothing,
      appName = Lude.Nothing,
      appARN = Lude.Nothing,
      domainId = Lude.Nothing,
      appType = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The creation time.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsCreationTime :: Lens.Lens' DescribeAppResponse (Lude.Maybe Lude.Timestamp)
darsCreationTime = Lens.lens (creationTime :: DescribeAppResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: DescribeAppResponse)
{-# DEPRECATED darsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsStatus :: Lens.Lens' DescribeAppResponse (Lude.Maybe AppStatus)
darsStatus = Lens.lens (status :: DescribeAppResponse -> Lude.Maybe AppStatus) (\s a -> s {status = a} :: DescribeAppResponse)
{-# DEPRECATED darsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The failure reason.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsFailureReason :: Lens.Lens' DescribeAppResponse (Lude.Maybe Lude.Text)
darsFailureReason = Lens.lens (failureReason :: DescribeAppResponse -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: DescribeAppResponse)
{-# DEPRECATED darsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The instance type and the Amazon Resource Name (ARN) of the SageMaker image created on the instance.
--
-- /Note:/ Consider using 'resourceSpec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsResourceSpec :: Lens.Lens' DescribeAppResponse (Lude.Maybe ResourceSpec)
darsResourceSpec = Lens.lens (resourceSpec :: DescribeAppResponse -> Lude.Maybe ResourceSpec) (\s a -> s {resourceSpec = a} :: DescribeAppResponse)
{-# DEPRECATED darsResourceSpec "Use generic-lens or generic-optics with 'resourceSpec' instead." #-}

-- | The user profile name.
--
-- /Note:/ Consider using 'userProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsUserProfileName :: Lens.Lens' DescribeAppResponse (Lude.Maybe Lude.Text)
darsUserProfileName = Lens.lens (userProfileName :: DescribeAppResponse -> Lude.Maybe Lude.Text) (\s a -> s {userProfileName = a} :: DescribeAppResponse)
{-# DEPRECATED darsUserProfileName "Use generic-lens or generic-optics with 'userProfileName' instead." #-}

-- | The timestamp of the last user's activity.
--
-- /Note:/ Consider using 'lastUserActivityTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsLastUserActivityTimestamp :: Lens.Lens' DescribeAppResponse (Lude.Maybe Lude.Timestamp)
darsLastUserActivityTimestamp = Lens.lens (lastUserActivityTimestamp :: DescribeAppResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUserActivityTimestamp = a} :: DescribeAppResponse)
{-# DEPRECATED darsLastUserActivityTimestamp "Use generic-lens or generic-optics with 'lastUserActivityTimestamp' instead." #-}

-- | The timestamp of the last health check.
--
-- /Note:/ Consider using 'lastHealthCheckTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsLastHealthCheckTimestamp :: Lens.Lens' DescribeAppResponse (Lude.Maybe Lude.Timestamp)
darsLastHealthCheckTimestamp = Lens.lens (lastHealthCheckTimestamp :: DescribeAppResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastHealthCheckTimestamp = a} :: DescribeAppResponse)
{-# DEPRECATED darsLastHealthCheckTimestamp "Use generic-lens or generic-optics with 'lastHealthCheckTimestamp' instead." #-}

-- | The name of the app.
--
-- /Note:/ Consider using 'appName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsAppName :: Lens.Lens' DescribeAppResponse (Lude.Maybe Lude.Text)
darsAppName = Lens.lens (appName :: DescribeAppResponse -> Lude.Maybe Lude.Text) (\s a -> s {appName = a} :: DescribeAppResponse)
{-# DEPRECATED darsAppName "Use generic-lens or generic-optics with 'appName' instead." #-}

-- | The Amazon Resource Name (ARN) of the app.
--
-- /Note:/ Consider using 'appARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsAppARN :: Lens.Lens' DescribeAppResponse (Lude.Maybe Lude.Text)
darsAppARN = Lens.lens (appARN :: DescribeAppResponse -> Lude.Maybe Lude.Text) (\s a -> s {appARN = a} :: DescribeAppResponse)
{-# DEPRECATED darsAppARN "Use generic-lens or generic-optics with 'appARN' instead." #-}

-- | The domain ID.
--
-- /Note:/ Consider using 'domainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsDomainId :: Lens.Lens' DescribeAppResponse (Lude.Maybe Lude.Text)
darsDomainId = Lens.lens (domainId :: DescribeAppResponse -> Lude.Maybe Lude.Text) (\s a -> s {domainId = a} :: DescribeAppResponse)
{-# DEPRECATED darsDomainId "Use generic-lens or generic-optics with 'domainId' instead." #-}

-- | The type of app.
--
-- /Note:/ Consider using 'appType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsAppType :: Lens.Lens' DescribeAppResponse (Lude.Maybe AppType)
darsAppType = Lens.lens (appType :: DescribeAppResponse -> Lude.Maybe AppType) (\s a -> s {appType = a} :: DescribeAppResponse)
{-# DEPRECATED darsAppType "Use generic-lens or generic-optics with 'appType' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsResponseStatus :: Lens.Lens' DescribeAppResponse Lude.Int
darsResponseStatus = Lens.lens (responseStatus :: DescribeAppResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAppResponse)
{-# DEPRECATED darsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
