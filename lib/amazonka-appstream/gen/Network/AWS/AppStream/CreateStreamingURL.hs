{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.CreateStreamingURL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a temporary URL to start an AppStream 2.0 streaming session for the specified user. A streaming URL enables application streaming to be tested without user setup.
module Network.AWS.AppStream.CreateStreamingURL
  ( -- * Creating a request
    CreateStreamingURL (..),
    mkCreateStreamingURL,

    -- ** Request lenses
    csuSessionContext,
    csuUserId,
    csuApplicationId,
    csuValidity,
    csuFleetName,
    csuStackName,

    -- * Destructuring the response
    CreateStreamingURLResponse (..),
    mkCreateStreamingURLResponse,

    -- ** Response lenses
    csursStreamingURL,
    csursExpires,
    csursResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateStreamingURL' smart constructor.
data CreateStreamingURL = CreateStreamingURL'
  { -- | The session context. For more information, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/managing-stacks-fleets.html#managing-stacks-fleets-parameters Session Context> in the /Amazon AppStream 2.0 Administration Guide/ .
    sessionContext :: Lude.Maybe Lude.Text,
    -- | The identifier of the user.
    userId :: Lude.Text,
    -- | The name of the application to launch after the session starts. This is the name that you specified as __Name__ in the Image Assistant.
    applicationId :: Lude.Maybe Lude.Text,
    -- | The time that the streaming URL will be valid, in seconds. Specify a value between 1 and 604800 seconds. The default is 60 seconds.
    validity :: Lude.Maybe Lude.Integer,
    -- | The name of the fleet.
    fleetName :: Lude.Text,
    -- | The name of the stack.
    stackName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateStreamingURL' with the minimum fields required to make a request.
--
-- * 'sessionContext' - The session context. For more information, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/managing-stacks-fleets.html#managing-stacks-fleets-parameters Session Context> in the /Amazon AppStream 2.0 Administration Guide/ .
-- * 'userId' - The identifier of the user.
-- * 'applicationId' - The name of the application to launch after the session starts. This is the name that you specified as __Name__ in the Image Assistant.
-- * 'validity' - The time that the streaming URL will be valid, in seconds. Specify a value between 1 and 604800 seconds. The default is 60 seconds.
-- * 'fleetName' - The name of the fleet.
-- * 'stackName' - The name of the stack.
mkCreateStreamingURL ::
  -- | 'userId'
  Lude.Text ->
  -- | 'fleetName'
  Lude.Text ->
  -- | 'stackName'
  Lude.Text ->
  CreateStreamingURL
mkCreateStreamingURL pUserId_ pFleetName_ pStackName_ =
  CreateStreamingURL'
    { sessionContext = Lude.Nothing,
      userId = pUserId_,
      applicationId = Lude.Nothing,
      validity = Lude.Nothing,
      fleetName = pFleetName_,
      stackName = pStackName_
    }

-- | The session context. For more information, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/managing-stacks-fleets.html#managing-stacks-fleets-parameters Session Context> in the /Amazon AppStream 2.0 Administration Guide/ .
--
-- /Note:/ Consider using 'sessionContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csuSessionContext :: Lens.Lens' CreateStreamingURL (Lude.Maybe Lude.Text)
csuSessionContext = Lens.lens (sessionContext :: CreateStreamingURL -> Lude.Maybe Lude.Text) (\s a -> s {sessionContext = a} :: CreateStreamingURL)
{-# DEPRECATED csuSessionContext "Use generic-lens or generic-optics with 'sessionContext' instead." #-}

-- | The identifier of the user.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csuUserId :: Lens.Lens' CreateStreamingURL Lude.Text
csuUserId = Lens.lens (userId :: CreateStreamingURL -> Lude.Text) (\s a -> s {userId = a} :: CreateStreamingURL)
{-# DEPRECATED csuUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The name of the application to launch after the session starts. This is the name that you specified as __Name__ in the Image Assistant.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csuApplicationId :: Lens.Lens' CreateStreamingURL (Lude.Maybe Lude.Text)
csuApplicationId = Lens.lens (applicationId :: CreateStreamingURL -> Lude.Maybe Lude.Text) (\s a -> s {applicationId = a} :: CreateStreamingURL)
{-# DEPRECATED csuApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The time that the streaming URL will be valid, in seconds. Specify a value between 1 and 604800 seconds. The default is 60 seconds.
--
-- /Note:/ Consider using 'validity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csuValidity :: Lens.Lens' CreateStreamingURL (Lude.Maybe Lude.Integer)
csuValidity = Lens.lens (validity :: CreateStreamingURL -> Lude.Maybe Lude.Integer) (\s a -> s {validity = a} :: CreateStreamingURL)
{-# DEPRECATED csuValidity "Use generic-lens or generic-optics with 'validity' instead." #-}

-- | The name of the fleet.
--
-- /Note:/ Consider using 'fleetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csuFleetName :: Lens.Lens' CreateStreamingURL Lude.Text
csuFleetName = Lens.lens (fleetName :: CreateStreamingURL -> Lude.Text) (\s a -> s {fleetName = a} :: CreateStreamingURL)
{-# DEPRECATED csuFleetName "Use generic-lens or generic-optics with 'fleetName' instead." #-}

-- | The name of the stack.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csuStackName :: Lens.Lens' CreateStreamingURL Lude.Text
csuStackName = Lens.lens (stackName :: CreateStreamingURL -> Lude.Text) (\s a -> s {stackName = a} :: CreateStreamingURL)
{-# DEPRECATED csuStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

instance Lude.AWSRequest CreateStreamingURL where
  type Rs CreateStreamingURL = CreateStreamingURLResponse
  request = Req.postJSON appStreamService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateStreamingURLResponse'
            Lude.<$> (x Lude..?> "StreamingURL")
            Lude.<*> (x Lude..?> "Expires")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateStreamingURL where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("PhotonAdminProxyService.CreateStreamingURL" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateStreamingURL where
  toJSON CreateStreamingURL' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SessionContext" Lude..=) Lude.<$> sessionContext,
            Lude.Just ("UserId" Lude..= userId),
            ("ApplicationId" Lude..=) Lude.<$> applicationId,
            ("Validity" Lude..=) Lude.<$> validity,
            Lude.Just ("FleetName" Lude..= fleetName),
            Lude.Just ("StackName" Lude..= stackName)
          ]
      )

instance Lude.ToPath CreateStreamingURL where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateStreamingURL where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateStreamingURLResponse' smart constructor.
data CreateStreamingURLResponse = CreateStreamingURLResponse'
  { -- | The URL to start the AppStream 2.0 streaming session.
    streamingURL :: Lude.Maybe Lude.Text,
    -- | The elapsed time, in seconds after the Unix epoch, when this URL expires.
    expires :: Lude.Maybe Lude.Timestamp,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateStreamingURLResponse' with the minimum fields required to make a request.
--
-- * 'streamingURL' - The URL to start the AppStream 2.0 streaming session.
-- * 'expires' - The elapsed time, in seconds after the Unix epoch, when this URL expires.
-- * 'responseStatus' - The response status code.
mkCreateStreamingURLResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateStreamingURLResponse
mkCreateStreamingURLResponse pResponseStatus_ =
  CreateStreamingURLResponse'
    { streamingURL = Lude.Nothing,
      expires = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The URL to start the AppStream 2.0 streaming session.
--
-- /Note:/ Consider using 'streamingURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csursStreamingURL :: Lens.Lens' CreateStreamingURLResponse (Lude.Maybe Lude.Text)
csursStreamingURL = Lens.lens (streamingURL :: CreateStreamingURLResponse -> Lude.Maybe Lude.Text) (\s a -> s {streamingURL = a} :: CreateStreamingURLResponse)
{-# DEPRECATED csursStreamingURL "Use generic-lens or generic-optics with 'streamingURL' instead." #-}

-- | The elapsed time, in seconds after the Unix epoch, when this URL expires.
--
-- /Note:/ Consider using 'expires' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csursExpires :: Lens.Lens' CreateStreamingURLResponse (Lude.Maybe Lude.Timestamp)
csursExpires = Lens.lens (expires :: CreateStreamingURLResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {expires = a} :: CreateStreamingURLResponse)
{-# DEPRECATED csursExpires "Use generic-lens or generic-optics with 'expires' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csursResponseStatus :: Lens.Lens' CreateStreamingURLResponse Lude.Int
csursResponseStatus = Lens.lens (responseStatus :: CreateStreamingURLResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateStreamingURLResponse)
{-# DEPRECATED csursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
