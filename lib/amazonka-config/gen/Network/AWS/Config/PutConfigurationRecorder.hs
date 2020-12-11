{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.PutConfigurationRecorder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new configuration recorder to record the selected resource configurations.
--
-- You can use this action to change the role @roleARN@ or the @recordingGroup@ of an existing recorder. To change the role, call the action on the existing configuration recorder and specify a role.
module Network.AWS.Config.PutConfigurationRecorder
  ( -- * Creating a request
    PutConfigurationRecorder (..),
    mkPutConfigurationRecorder,

    -- ** Request lenses
    pcrConfigurationRecorder,

    -- * Destructuring the response
    PutConfigurationRecorderResponse (..),
    mkPutConfigurationRecorderResponse,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the 'PutConfigurationRecorder' action.
--
-- /See:/ 'mkPutConfigurationRecorder' smart constructor.
newtype PutConfigurationRecorder = PutConfigurationRecorder'
  { configurationRecorder ::
      ConfigurationRecorder
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutConfigurationRecorder' with the minimum fields required to make a request.
--
-- * 'configurationRecorder' - The configuration recorder object that records each configuration change made to the resources.
mkPutConfigurationRecorder ::
  -- | 'configurationRecorder'
  ConfigurationRecorder ->
  PutConfigurationRecorder
mkPutConfigurationRecorder pConfigurationRecorder_ =
  PutConfigurationRecorder'
    { configurationRecorder =
        pConfigurationRecorder_
    }

-- | The configuration recorder object that records each configuration change made to the resources.
--
-- /Note:/ Consider using 'configurationRecorder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrConfigurationRecorder :: Lens.Lens' PutConfigurationRecorder ConfigurationRecorder
pcrConfigurationRecorder = Lens.lens (configurationRecorder :: PutConfigurationRecorder -> ConfigurationRecorder) (\s a -> s {configurationRecorder = a} :: PutConfigurationRecorder)
{-# DEPRECATED pcrConfigurationRecorder "Use generic-lens or generic-optics with 'configurationRecorder' instead." #-}

instance Lude.AWSRequest PutConfigurationRecorder where
  type Rs PutConfigurationRecorder = PutConfigurationRecorderResponse
  request = Req.postJSON configService
  response = Res.receiveNull PutConfigurationRecorderResponse'

instance Lude.ToHeaders PutConfigurationRecorder where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.PutConfigurationRecorder" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutConfigurationRecorder where
  toJSON PutConfigurationRecorder' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("ConfigurationRecorder" Lude..= configurationRecorder)
          ]
      )

instance Lude.ToPath PutConfigurationRecorder where
  toPath = Lude.const "/"

instance Lude.ToQuery PutConfigurationRecorder where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutConfigurationRecorderResponse' smart constructor.
data PutConfigurationRecorderResponse = PutConfigurationRecorderResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutConfigurationRecorderResponse' with the minimum fields required to make a request.
mkPutConfigurationRecorderResponse ::
  PutConfigurationRecorderResponse
mkPutConfigurationRecorderResponse =
  PutConfigurationRecorderResponse'
