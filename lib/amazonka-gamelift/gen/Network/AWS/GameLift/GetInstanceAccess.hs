{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.GetInstanceAccess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests remote access to a fleet instance. Remote access is useful for debugging, gathering benchmarking data, or observing activity in real time.
--
-- To remotely access an instance, you need credentials that match the operating system of the instance. For a Windows instance, Amazon GameLift returns a user name and password as strings for use with a Windows Remote Desktop client. For a Linux instance, Amazon GameLift returns a user name and RSA private key, also as strings, for use with an SSH client. The private key must be saved in the proper format to a @.pem@ file before using. If you're making this request using the AWS CLI, saving the secret can be handled as part of the GetInstanceAccess request, as shown in one of the examples for this operation.
-- To request access to a specific instance, specify the IDs of both the instance and the fleet it belongs to. You can retrieve a fleet's instance IDs by calling 'DescribeInstances' . If successful, an 'InstanceAccess' object is returned that contains the instance's IP address and a set of credentials.
-- __Learn more__
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-remote-access.html Remotely Access Fleet Instances>
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-creating-debug.html Debug Fleet Issues>
-- __Related operations__
--
--     * 'DescribeInstances'
--
--
--     * 'GetInstanceAccess'
module Network.AWS.GameLift.GetInstanceAccess
  ( -- * Creating a request
    GetInstanceAccess (..),
    mkGetInstanceAccess,

    -- ** Request lenses
    giaInstanceId,
    giaFleetId,

    -- * Destructuring the response
    GetInstanceAccessResponse (..),
    mkGetInstanceAccessResponse,

    -- ** Response lenses
    giarsInstanceAccess,
    giarsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkGetInstanceAccess' smart constructor.
data GetInstanceAccess = GetInstanceAccess'
  { -- | A unique identifier for an instance you want to get access to. You can access an instance in any status.
    instanceId :: Lude.Text,
    -- | A unique identifier for a fleet that contains the instance you want access to. You can use either the fleet ID or ARN value. The fleet can be in any of the following statuses: @ACTIVATING@ , @ACTIVE@ , or @ERROR@ . Fleets with an @ERROR@ status may be accessible for a short time before they are deleted.
    fleetId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetInstanceAccess' with the minimum fields required to make a request.
--
-- * 'instanceId' - A unique identifier for an instance you want to get access to. You can access an instance in any status.
-- * 'fleetId' - A unique identifier for a fleet that contains the instance you want access to. You can use either the fleet ID or ARN value. The fleet can be in any of the following statuses: @ACTIVATING@ , @ACTIVE@ , or @ERROR@ . Fleets with an @ERROR@ status may be accessible for a short time before they are deleted.
mkGetInstanceAccess ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'fleetId'
  Lude.Text ->
  GetInstanceAccess
mkGetInstanceAccess pInstanceId_ pFleetId_ =
  GetInstanceAccess'
    { instanceId = pInstanceId_,
      fleetId = pFleetId_
    }

-- | A unique identifier for an instance you want to get access to. You can access an instance in any status.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giaInstanceId :: Lens.Lens' GetInstanceAccess Lude.Text
giaInstanceId = Lens.lens (instanceId :: GetInstanceAccess -> Lude.Text) (\s a -> s {instanceId = a} :: GetInstanceAccess)
{-# DEPRECATED giaInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | A unique identifier for a fleet that contains the instance you want access to. You can use either the fleet ID or ARN value. The fleet can be in any of the following statuses: @ACTIVATING@ , @ACTIVE@ , or @ERROR@ . Fleets with an @ERROR@ status may be accessible for a short time before they are deleted.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giaFleetId :: Lens.Lens' GetInstanceAccess Lude.Text
giaFleetId = Lens.lens (fleetId :: GetInstanceAccess -> Lude.Text) (\s a -> s {fleetId = a} :: GetInstanceAccess)
{-# DEPRECATED giaFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

instance Lude.AWSRequest GetInstanceAccess where
  type Rs GetInstanceAccess = GetInstanceAccessResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetInstanceAccessResponse'
            Lude.<$> (x Lude..?> "InstanceAccess")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetInstanceAccess where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.GetInstanceAccess" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetInstanceAccess where
  toJSON GetInstanceAccess' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("InstanceId" Lude..= instanceId),
            Lude.Just ("FleetId" Lude..= fleetId)
          ]
      )

instance Lude.ToPath GetInstanceAccess where
  toPath = Lude.const "/"

instance Lude.ToQuery GetInstanceAccess where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkGetInstanceAccessResponse' smart constructor.
data GetInstanceAccessResponse = GetInstanceAccessResponse'
  { -- | The connection information for a fleet instance, including IP address and access credentials.
    instanceAccess :: Lude.Maybe InstanceAccess,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetInstanceAccessResponse' with the minimum fields required to make a request.
--
-- * 'instanceAccess' - The connection information for a fleet instance, including IP address and access credentials.
-- * 'responseStatus' - The response status code.
mkGetInstanceAccessResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetInstanceAccessResponse
mkGetInstanceAccessResponse pResponseStatus_ =
  GetInstanceAccessResponse'
    { instanceAccess = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The connection information for a fleet instance, including IP address and access credentials.
--
-- /Note:/ Consider using 'instanceAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giarsInstanceAccess :: Lens.Lens' GetInstanceAccessResponse (Lude.Maybe InstanceAccess)
giarsInstanceAccess = Lens.lens (instanceAccess :: GetInstanceAccessResponse -> Lude.Maybe InstanceAccess) (\s a -> s {instanceAccess = a} :: GetInstanceAccessResponse)
{-# DEPRECATED giarsInstanceAccess "Use generic-lens or generic-optics with 'instanceAccess' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giarsResponseStatus :: Lens.Lens' GetInstanceAccessResponse Lude.Int
giarsResponseStatus = Lens.lens (responseStatus :: GetInstanceAccessResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetInstanceAccessResponse)
{-# DEPRECATED giarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
