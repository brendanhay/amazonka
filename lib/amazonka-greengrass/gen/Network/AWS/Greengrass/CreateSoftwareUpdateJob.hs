{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.CreateSoftwareUpdateJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a software update for a core or group of cores (specified as an IoT thing group.) Use this to update the OTA Agent as well as the Greengrass core software. It makes use of the IoT Jobs feature which provides additional commands to manage a Greengrass core software update job.
module Network.AWS.Greengrass.CreateSoftwareUpdateJob
  ( -- * Creating a request
    CreateSoftwareUpdateJob (..),
    mkCreateSoftwareUpdateJob,

    -- ** Request lenses
    csujUpdateAgentLogLevel,
    csujAmznClientToken,
    csujS3URLSignerRole,
    csujUpdateTargetsArchitecture,
    csujSoftwareToUpdate,
    csujUpdateTargets,
    csujUpdateTargetsOperatingSystem,

    -- * Destructuring the response
    CreateSoftwareUpdateJobResponse (..),
    mkCreateSoftwareUpdateJobResponse,

    -- ** Response lenses
    csujrsPlatformSoftwareVersion,
    csujrsIotJobARN,
    csujrsIotJobId,
    csujrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateSoftwareUpdateJob' smart constructor.
data CreateSoftwareUpdateJob = CreateSoftwareUpdateJob'
  { updateAgentLogLevel ::
      Lude.Maybe UpdateAgentLogLevel,
    amznClientToken :: Lude.Maybe Lude.Text,
    s3URLSignerRole :: Lude.Text,
    updateTargetsArchitecture ::
      UpdateTargetsArchitecture,
    softwareToUpdate :: SoftwareToUpdate,
    updateTargets :: [Lude.Text],
    updateTargetsOperatingSystem ::
      UpdateTargetsOperatingSystem
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSoftwareUpdateJob' with the minimum fields required to make a request.
--
-- * 'amznClientToken' - A client token used to correlate requests and responses.
-- * 's3URLSignerRole' - Undocumented field.
-- * 'softwareToUpdate' - Undocumented field.
-- * 'updateAgentLogLevel' - Undocumented field.
-- * 'updateTargets' - Undocumented field.
-- * 'updateTargetsArchitecture' - Undocumented field.
-- * 'updateTargetsOperatingSystem' - Undocumented field.
mkCreateSoftwareUpdateJob ::
  -- | 's3URLSignerRole'
  Lude.Text ->
  -- | 'updateTargetsArchitecture'
  UpdateTargetsArchitecture ->
  -- | 'softwareToUpdate'
  SoftwareToUpdate ->
  -- | 'updateTargetsOperatingSystem'
  UpdateTargetsOperatingSystem ->
  CreateSoftwareUpdateJob
mkCreateSoftwareUpdateJob
  pS3URLSignerRole_
  pUpdateTargetsArchitecture_
  pSoftwareToUpdate_
  pUpdateTargetsOperatingSystem_ =
    CreateSoftwareUpdateJob'
      { updateAgentLogLevel = Lude.Nothing,
        amznClientToken = Lude.Nothing,
        s3URLSignerRole = pS3URLSignerRole_,
        updateTargetsArchitecture = pUpdateTargetsArchitecture_,
        softwareToUpdate = pSoftwareToUpdate_,
        updateTargets = Lude.mempty,
        updateTargetsOperatingSystem = pUpdateTargetsOperatingSystem_
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'updateAgentLogLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csujUpdateAgentLogLevel :: Lens.Lens' CreateSoftwareUpdateJob (Lude.Maybe UpdateAgentLogLevel)
csujUpdateAgentLogLevel = Lens.lens (updateAgentLogLevel :: CreateSoftwareUpdateJob -> Lude.Maybe UpdateAgentLogLevel) (\s a -> s {updateAgentLogLevel = a} :: CreateSoftwareUpdateJob)
{-# DEPRECATED csujUpdateAgentLogLevel "Use generic-lens or generic-optics with 'updateAgentLogLevel' instead." #-}

-- | A client token used to correlate requests and responses.
--
-- /Note:/ Consider using 'amznClientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csujAmznClientToken :: Lens.Lens' CreateSoftwareUpdateJob (Lude.Maybe Lude.Text)
csujAmznClientToken = Lens.lens (amznClientToken :: CreateSoftwareUpdateJob -> Lude.Maybe Lude.Text) (\s a -> s {amznClientToken = a} :: CreateSoftwareUpdateJob)
{-# DEPRECATED csujAmznClientToken "Use generic-lens or generic-optics with 'amznClientToken' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 's3URLSignerRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csujS3URLSignerRole :: Lens.Lens' CreateSoftwareUpdateJob Lude.Text
csujS3URLSignerRole = Lens.lens (s3URLSignerRole :: CreateSoftwareUpdateJob -> Lude.Text) (\s a -> s {s3URLSignerRole = a} :: CreateSoftwareUpdateJob)
{-# DEPRECATED csujS3URLSignerRole "Use generic-lens or generic-optics with 's3URLSignerRole' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'updateTargetsArchitecture' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csujUpdateTargetsArchitecture :: Lens.Lens' CreateSoftwareUpdateJob UpdateTargetsArchitecture
csujUpdateTargetsArchitecture = Lens.lens (updateTargetsArchitecture :: CreateSoftwareUpdateJob -> UpdateTargetsArchitecture) (\s a -> s {updateTargetsArchitecture = a} :: CreateSoftwareUpdateJob)
{-# DEPRECATED csujUpdateTargetsArchitecture "Use generic-lens or generic-optics with 'updateTargetsArchitecture' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'softwareToUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csujSoftwareToUpdate :: Lens.Lens' CreateSoftwareUpdateJob SoftwareToUpdate
csujSoftwareToUpdate = Lens.lens (softwareToUpdate :: CreateSoftwareUpdateJob -> SoftwareToUpdate) (\s a -> s {softwareToUpdate = a} :: CreateSoftwareUpdateJob)
{-# DEPRECATED csujSoftwareToUpdate "Use generic-lens or generic-optics with 'softwareToUpdate' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'updateTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csujUpdateTargets :: Lens.Lens' CreateSoftwareUpdateJob [Lude.Text]
csujUpdateTargets = Lens.lens (updateTargets :: CreateSoftwareUpdateJob -> [Lude.Text]) (\s a -> s {updateTargets = a} :: CreateSoftwareUpdateJob)
{-# DEPRECATED csujUpdateTargets "Use generic-lens or generic-optics with 'updateTargets' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'updateTargetsOperatingSystem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csujUpdateTargetsOperatingSystem :: Lens.Lens' CreateSoftwareUpdateJob UpdateTargetsOperatingSystem
csujUpdateTargetsOperatingSystem = Lens.lens (updateTargetsOperatingSystem :: CreateSoftwareUpdateJob -> UpdateTargetsOperatingSystem) (\s a -> s {updateTargetsOperatingSystem = a} :: CreateSoftwareUpdateJob)
{-# DEPRECATED csujUpdateTargetsOperatingSystem "Use generic-lens or generic-optics with 'updateTargetsOperatingSystem' instead." #-}

instance Lude.AWSRequest CreateSoftwareUpdateJob where
  type Rs CreateSoftwareUpdateJob = CreateSoftwareUpdateJobResponse
  request = Req.postJSON greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateSoftwareUpdateJobResponse'
            Lude.<$> (x Lude..?> "PlatformSoftwareVersion")
            Lude.<*> (x Lude..?> "IotJobArn")
            Lude.<*> (x Lude..?> "IotJobId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateSoftwareUpdateJob where
  toHeaders CreateSoftwareUpdateJob' {..} =
    Lude.mconcat
      [ "X-Amzn-Client-Token" Lude.=# amznClientToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToJSON CreateSoftwareUpdateJob where
  toJSON CreateSoftwareUpdateJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("UpdateAgentLogLevel" Lude..=) Lude.<$> updateAgentLogLevel,
            Lude.Just ("S3UrlSignerRole" Lude..= s3URLSignerRole),
            Lude.Just
              ("UpdateTargetsArchitecture" Lude..= updateTargetsArchitecture),
            Lude.Just ("SoftwareToUpdate" Lude..= softwareToUpdate),
            Lude.Just ("UpdateTargets" Lude..= updateTargets),
            Lude.Just
              ( "UpdateTargetsOperatingSystem"
                  Lude..= updateTargetsOperatingSystem
              )
          ]
      )

instance Lude.ToPath CreateSoftwareUpdateJob where
  toPath = Lude.const "/greengrass/updates"

instance Lude.ToQuery CreateSoftwareUpdateJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateSoftwareUpdateJobResponse' smart constructor.
data CreateSoftwareUpdateJobResponse = CreateSoftwareUpdateJobResponse'
  { platformSoftwareVersion ::
      Lude.Maybe Lude.Text,
    iotJobARN ::
      Lude.Maybe Lude.Text,
    iotJobId ::
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

-- | Creates a value of 'CreateSoftwareUpdateJobResponse' with the minimum fields required to make a request.
--
-- * 'iotJobARN' - The IoT Job ARN corresponding to this update.
-- * 'iotJobId' - The IoT Job Id corresponding to this update.
-- * 'platformSoftwareVersion' - The software version installed on the device or devices after the update.
-- * 'responseStatus' - The response status code.
mkCreateSoftwareUpdateJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateSoftwareUpdateJobResponse
mkCreateSoftwareUpdateJobResponse pResponseStatus_ =
  CreateSoftwareUpdateJobResponse'
    { platformSoftwareVersion =
        Lude.Nothing,
      iotJobARN = Lude.Nothing,
      iotJobId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The software version installed on the device or devices after the update.
--
-- /Note:/ Consider using 'platformSoftwareVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csujrsPlatformSoftwareVersion :: Lens.Lens' CreateSoftwareUpdateJobResponse (Lude.Maybe Lude.Text)
csujrsPlatformSoftwareVersion = Lens.lens (platformSoftwareVersion :: CreateSoftwareUpdateJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {platformSoftwareVersion = a} :: CreateSoftwareUpdateJobResponse)
{-# DEPRECATED csujrsPlatformSoftwareVersion "Use generic-lens or generic-optics with 'platformSoftwareVersion' instead." #-}

-- | The IoT Job ARN corresponding to this update.
--
-- /Note:/ Consider using 'iotJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csujrsIotJobARN :: Lens.Lens' CreateSoftwareUpdateJobResponse (Lude.Maybe Lude.Text)
csujrsIotJobARN = Lens.lens (iotJobARN :: CreateSoftwareUpdateJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {iotJobARN = a} :: CreateSoftwareUpdateJobResponse)
{-# DEPRECATED csujrsIotJobARN "Use generic-lens or generic-optics with 'iotJobARN' instead." #-}

-- | The IoT Job Id corresponding to this update.
--
-- /Note:/ Consider using 'iotJobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csujrsIotJobId :: Lens.Lens' CreateSoftwareUpdateJobResponse (Lude.Maybe Lude.Text)
csujrsIotJobId = Lens.lens (iotJobId :: CreateSoftwareUpdateJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {iotJobId = a} :: CreateSoftwareUpdateJobResponse)
{-# DEPRECATED csujrsIotJobId "Use generic-lens or generic-optics with 'iotJobId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csujrsResponseStatus :: Lens.Lens' CreateSoftwareUpdateJobResponse Lude.Int
csujrsResponseStatus = Lens.lens (responseStatus :: CreateSoftwareUpdateJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateSoftwareUpdateJobResponse)
{-# DEPRECATED csujrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
