{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.GetConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__ . For more information, see <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs> , the <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference> .
--
-- __For information about the current version of AWS CloudHSM__ , see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> , the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference> .
-- Gets the configuration files necessary to connect to all high availability partition groups the client is associated with.
module Network.AWS.CloudHSM.GetConfig
  ( -- * Creating a request
    GetConfig (..),
    mkGetConfig,

    -- ** Request lenses
    gcClientARN,
    gcHAPGList,
    gcClientVersion,

    -- * Destructuring the response
    GetConfigResponse (..),
    mkGetConfigResponse,

    -- ** Response lenses
    gcrsConfigFile,
    gcrsConfigCred,
    gcrsConfigType,
    gcrsResponseStatus,
  )
where

import Network.AWS.CloudHSM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetConfig' smart constructor.
data GetConfig = GetConfig'
  { -- | The ARN of the client.
    clientARN :: Lude.Text,
    -- | A list of ARNs that identify the high-availability partition groups that are associated with the client.
    hapgList :: [Lude.Text],
    -- | The client version.
    clientVersion :: ClientVersion
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetConfig' with the minimum fields required to make a request.
--
-- * 'clientARN' - The ARN of the client.
-- * 'hapgList' - A list of ARNs that identify the high-availability partition groups that are associated with the client.
-- * 'clientVersion' - The client version.
mkGetConfig ::
  -- | 'clientARN'
  Lude.Text ->
  -- | 'clientVersion'
  ClientVersion ->
  GetConfig
mkGetConfig pClientARN_ pClientVersion_ =
  GetConfig'
    { clientARN = pClientARN_,
      hapgList = Lude.mempty,
      clientVersion = pClientVersion_
    }

-- | The ARN of the client.
--
-- /Note:/ Consider using 'clientARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcClientARN :: Lens.Lens' GetConfig Lude.Text
gcClientARN = Lens.lens (clientARN :: GetConfig -> Lude.Text) (\s a -> s {clientARN = a} :: GetConfig)
{-# DEPRECATED gcClientARN "Use generic-lens or generic-optics with 'clientARN' instead." #-}

-- | A list of ARNs that identify the high-availability partition groups that are associated with the client.
--
-- /Note:/ Consider using 'hapgList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcHAPGList :: Lens.Lens' GetConfig [Lude.Text]
gcHAPGList = Lens.lens (hapgList :: GetConfig -> [Lude.Text]) (\s a -> s {hapgList = a} :: GetConfig)
{-# DEPRECATED gcHAPGList "Use generic-lens or generic-optics with 'hapgList' instead." #-}

-- | The client version.
--
-- /Note:/ Consider using 'clientVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcClientVersion :: Lens.Lens' GetConfig ClientVersion
gcClientVersion = Lens.lens (clientVersion :: GetConfig -> ClientVersion) (\s a -> s {clientVersion = a} :: GetConfig)
{-# DEPRECATED gcClientVersion "Use generic-lens or generic-optics with 'clientVersion' instead." #-}

instance Lude.AWSRequest GetConfig where
  type Rs GetConfig = GetConfigResponse
  request = Req.postJSON cloudHSMService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetConfigResponse'
            Lude.<$> (x Lude..?> "ConfigFile")
            Lude.<*> (x Lude..?> "ConfigCred")
            Lude.<*> (x Lude..?> "ConfigType")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetConfig where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CloudHsmFrontendService.GetConfig" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetConfig where
  toJSON GetConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ClientArn" Lude..= clientARN),
            Lude.Just ("HapgList" Lude..= hapgList),
            Lude.Just ("ClientVersion" Lude..= clientVersion)
          ]
      )

instance Lude.ToPath GetConfig where
  toPath = Lude.const "/"

instance Lude.ToQuery GetConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetConfigResponse' smart constructor.
data GetConfigResponse = GetConfigResponse'
  { -- | The chrystoki.conf configuration file.
    configFile :: Lude.Maybe Lude.Text,
    -- | The certificate file containing the server.pem files of the HSMs.
    configCred :: Lude.Maybe Lude.Text,
    -- | The type of credentials.
    configType :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetConfigResponse' with the minimum fields required to make a request.
--
-- * 'configFile' - The chrystoki.conf configuration file.
-- * 'configCred' - The certificate file containing the server.pem files of the HSMs.
-- * 'configType' - The type of credentials.
-- * 'responseStatus' - The response status code.
mkGetConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetConfigResponse
mkGetConfigResponse pResponseStatus_ =
  GetConfigResponse'
    { configFile = Lude.Nothing,
      configCred = Lude.Nothing,
      configType = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The chrystoki.conf configuration file.
--
-- /Note:/ Consider using 'configFile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrsConfigFile :: Lens.Lens' GetConfigResponse (Lude.Maybe Lude.Text)
gcrsConfigFile = Lens.lens (configFile :: GetConfigResponse -> Lude.Maybe Lude.Text) (\s a -> s {configFile = a} :: GetConfigResponse)
{-# DEPRECATED gcrsConfigFile "Use generic-lens or generic-optics with 'configFile' instead." #-}

-- | The certificate file containing the server.pem files of the HSMs.
--
-- /Note:/ Consider using 'configCred' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrsConfigCred :: Lens.Lens' GetConfigResponse (Lude.Maybe Lude.Text)
gcrsConfigCred = Lens.lens (configCred :: GetConfigResponse -> Lude.Maybe Lude.Text) (\s a -> s {configCred = a} :: GetConfigResponse)
{-# DEPRECATED gcrsConfigCred "Use generic-lens or generic-optics with 'configCred' instead." #-}

-- | The type of credentials.
--
-- /Note:/ Consider using 'configType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrsConfigType :: Lens.Lens' GetConfigResponse (Lude.Maybe Lude.Text)
gcrsConfigType = Lens.lens (configType :: GetConfigResponse -> Lude.Maybe Lude.Text) (\s a -> s {configType = a} :: GetConfigResponse)
{-# DEPRECATED gcrsConfigType "Use generic-lens or generic-optics with 'configType' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrsResponseStatus :: Lens.Lens' GetConfigResponse Lude.Int
gcrsResponseStatus = Lens.lens (responseStatus :: GetConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetConfigResponse)
{-# DEPRECATED gcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
