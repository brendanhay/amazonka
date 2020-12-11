{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DeleteConfigurationRecorder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the configuration recorder.
--
-- After the configuration recorder is deleted, AWS Config will not record resource configuration changes until you create a new configuration recorder.
-- This action does not delete the configuration information that was previously recorded. You will be able to access the previously recorded information by using the @GetResourceConfigHistory@ action, but you will not be able to access this information in the AWS Config console until you create a new configuration recorder.
module Network.AWS.Config.DeleteConfigurationRecorder
  ( -- * Creating a request
    DeleteConfigurationRecorder (..),
    mkDeleteConfigurationRecorder,

    -- ** Request lenses
    dcrConfigurationRecorderName,

    -- * Destructuring the response
    DeleteConfigurationRecorderResponse (..),
    mkDeleteConfigurationRecorderResponse,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The request object for the @DeleteConfigurationRecorder@ action.
--
-- /See:/ 'mkDeleteConfigurationRecorder' smart constructor.
newtype DeleteConfigurationRecorder = DeleteConfigurationRecorder'
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

-- | Creates a value of 'DeleteConfigurationRecorder' with the minimum fields required to make a request.
--
-- * 'configurationRecorderName' - The name of the configuration recorder to be deleted. You can retrieve the name of your configuration recorder by using the @DescribeConfigurationRecorders@ action.
mkDeleteConfigurationRecorder ::
  -- | 'configurationRecorderName'
  Lude.Text ->
  DeleteConfigurationRecorder
mkDeleteConfigurationRecorder pConfigurationRecorderName_ =
  DeleteConfigurationRecorder'
    { configurationRecorderName =
        pConfigurationRecorderName_
    }

-- | The name of the configuration recorder to be deleted. You can retrieve the name of your configuration recorder by using the @DescribeConfigurationRecorders@ action.
--
-- /Note:/ Consider using 'configurationRecorderName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrConfigurationRecorderName :: Lens.Lens' DeleteConfigurationRecorder Lude.Text
dcrConfigurationRecorderName = Lens.lens (configurationRecorderName :: DeleteConfigurationRecorder -> Lude.Text) (\s a -> s {configurationRecorderName = a} :: DeleteConfigurationRecorder)
{-# DEPRECATED dcrConfigurationRecorderName "Use generic-lens or generic-optics with 'configurationRecorderName' instead." #-}

instance Lude.AWSRequest DeleteConfigurationRecorder where
  type
    Rs DeleteConfigurationRecorder =
      DeleteConfigurationRecorderResponse
  request = Req.postJSON configService
  response = Res.receiveNull DeleteConfigurationRecorderResponse'

instance Lude.ToHeaders DeleteConfigurationRecorder where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.DeleteConfigurationRecorder" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteConfigurationRecorder where
  toJSON DeleteConfigurationRecorder' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("ConfigurationRecorderName" Lude..= configurationRecorderName)
          ]
      )

instance Lude.ToPath DeleteConfigurationRecorder where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteConfigurationRecorder where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteConfigurationRecorderResponse' smart constructor.
data DeleteConfigurationRecorderResponse = DeleteConfigurationRecorderResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteConfigurationRecorderResponse' with the minimum fields required to make a request.
mkDeleteConfigurationRecorderResponse ::
  DeleteConfigurationRecorderResponse
mkDeleteConfigurationRecorderResponse =
  DeleteConfigurationRecorderResponse'
