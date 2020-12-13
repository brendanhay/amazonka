{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.StartConfigurationRecorder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts recording configurations of the AWS resources you have selected to record in your AWS account.
--
-- You must have created at least one delivery channel to successfully start the configuration recorder.
module Network.AWS.Config.StartConfigurationRecorder
  ( -- * Creating a request
    StartConfigurationRecorder (..),
    mkStartConfigurationRecorder,

    -- ** Request lenses
    scrConfigurationRecorderName,

    -- * Destructuring the response
    StartConfigurationRecorderResponse (..),
    mkStartConfigurationRecorderResponse,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the 'StartConfigurationRecorder' action.
--
-- /See:/ 'mkStartConfigurationRecorder' smart constructor.
newtype StartConfigurationRecorder = StartConfigurationRecorder'
  { -- | The name of the recorder object that records each configuration change made to the resources.
    configurationRecorderName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartConfigurationRecorder' with the minimum fields required to make a request.
--
-- * 'configurationRecorderName' - The name of the recorder object that records each configuration change made to the resources.
mkStartConfigurationRecorder ::
  -- | 'configurationRecorderName'
  Lude.Text ->
  StartConfigurationRecorder
mkStartConfigurationRecorder pConfigurationRecorderName_ =
  StartConfigurationRecorder'
    { configurationRecorderName =
        pConfigurationRecorderName_
    }

-- | The name of the recorder object that records each configuration change made to the resources.
--
-- /Note:/ Consider using 'configurationRecorderName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrConfigurationRecorderName :: Lens.Lens' StartConfigurationRecorder Lude.Text
scrConfigurationRecorderName = Lens.lens (configurationRecorderName :: StartConfigurationRecorder -> Lude.Text) (\s a -> s {configurationRecorderName = a} :: StartConfigurationRecorder)
{-# DEPRECATED scrConfigurationRecorderName "Use generic-lens or generic-optics with 'configurationRecorderName' instead." #-}

instance Lude.AWSRequest StartConfigurationRecorder where
  type
    Rs StartConfigurationRecorder =
      StartConfigurationRecorderResponse
  request = Req.postJSON configService
  response = Res.receiveNull StartConfigurationRecorderResponse'

instance Lude.ToHeaders StartConfigurationRecorder where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.StartConfigurationRecorder" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartConfigurationRecorder where
  toJSON StartConfigurationRecorder' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("ConfigurationRecorderName" Lude..= configurationRecorderName)
          ]
      )

instance Lude.ToPath StartConfigurationRecorder where
  toPath = Lude.const "/"

instance Lude.ToQuery StartConfigurationRecorder where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartConfigurationRecorderResponse' smart constructor.
data StartConfigurationRecorderResponse = StartConfigurationRecorderResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartConfigurationRecorderResponse' with the minimum fields required to make a request.
mkStartConfigurationRecorderResponse ::
  StartConfigurationRecorderResponse
mkStartConfigurationRecorderResponse =
  StartConfigurationRecorderResponse'
