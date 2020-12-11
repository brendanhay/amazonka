{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.StopConfigurationRecorder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops recording configurations of the AWS resources you have selected to record in your AWS account.
module Network.AWS.Config.StopConfigurationRecorder
  ( -- * Creating a request
    StopConfigurationRecorder (..),
    mkStopConfigurationRecorder,

    -- ** Request lenses
    scrConfigurationRecorderName,

    -- * Destructuring the response
    StopConfigurationRecorderResponse (..),
    mkStopConfigurationRecorderResponse,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the 'StopConfigurationRecorder' action.
--
-- /See:/ 'mkStopConfigurationRecorder' smart constructor.
newtype StopConfigurationRecorder = StopConfigurationRecorder'
  { configurationRecorderName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopConfigurationRecorder' with the minimum fields required to make a request.
--
-- * 'configurationRecorderName' - The name of the recorder object that records each configuration change made to the resources.
mkStopConfigurationRecorder ::
  -- | 'configurationRecorderName'
  Lude.Text ->
  StopConfigurationRecorder
mkStopConfigurationRecorder pConfigurationRecorderName_ =
  StopConfigurationRecorder'
    { configurationRecorderName =
        pConfigurationRecorderName_
    }

-- | The name of the recorder object that records each configuration change made to the resources.
--
-- /Note:/ Consider using 'configurationRecorderName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrConfigurationRecorderName :: Lens.Lens' StopConfigurationRecorder Lude.Text
scrConfigurationRecorderName = Lens.lens (configurationRecorderName :: StopConfigurationRecorder -> Lude.Text) (\s a -> s {configurationRecorderName = a} :: StopConfigurationRecorder)
{-# DEPRECATED scrConfigurationRecorderName "Use generic-lens or generic-optics with 'configurationRecorderName' instead." #-}

instance Lude.AWSRequest StopConfigurationRecorder where
  type
    Rs StopConfigurationRecorder =
      StopConfigurationRecorderResponse
  request = Req.postJSON configService
  response = Res.receiveNull StopConfigurationRecorderResponse'

instance Lude.ToHeaders StopConfigurationRecorder where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.StopConfigurationRecorder" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopConfigurationRecorder where
  toJSON StopConfigurationRecorder' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("ConfigurationRecorderName" Lude..= configurationRecorderName)
          ]
      )

instance Lude.ToPath StopConfigurationRecorder where
  toPath = Lude.const "/"

instance Lude.ToQuery StopConfigurationRecorder where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopConfigurationRecorderResponse' smart constructor.
data StopConfigurationRecorderResponse = StopConfigurationRecorderResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopConfigurationRecorderResponse' with the minimum fields required to make a request.
mkStopConfigurationRecorderResponse ::
  StopConfigurationRecorderResponse
mkStopConfigurationRecorderResponse =
  StopConfigurationRecorderResponse'
