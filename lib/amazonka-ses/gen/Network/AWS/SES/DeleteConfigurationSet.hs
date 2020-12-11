{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.DeleteConfigurationSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a configuration set. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.DeleteConfigurationSet
  ( -- * Creating a request
    DeleteConfigurationSet (..),
    mkDeleteConfigurationSet,

    -- ** Request lenses
    dConfigurationSetName,

    -- * Destructuring the response
    DeleteConfigurationSetResponse (..),
    mkDeleteConfigurationSetResponse,

    -- ** Response lenses
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to delete a configuration set. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkDeleteConfigurationSet' smart constructor.
newtype DeleteConfigurationSet = DeleteConfigurationSet'
  { configurationSetName ::
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

-- | Creates a value of 'DeleteConfigurationSet' with the minimum fields required to make a request.
--
-- * 'configurationSetName' - The name of the configuration set to delete.
mkDeleteConfigurationSet ::
  -- | 'configurationSetName'
  Lude.Text ->
  DeleteConfigurationSet
mkDeleteConfigurationSet pConfigurationSetName_ =
  DeleteConfigurationSet'
    { configurationSetName =
        pConfigurationSetName_
    }

-- | The name of the configuration set to delete.
--
-- /Note:/ Consider using 'configurationSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dConfigurationSetName :: Lens.Lens' DeleteConfigurationSet Lude.Text
dConfigurationSetName = Lens.lens (configurationSetName :: DeleteConfigurationSet -> Lude.Text) (\s a -> s {configurationSetName = a} :: DeleteConfigurationSet)
{-# DEPRECATED dConfigurationSetName "Use generic-lens or generic-optics with 'configurationSetName' instead." #-}

instance Lude.AWSRequest DeleteConfigurationSet where
  type Rs DeleteConfigurationSet = DeleteConfigurationSetResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "DeleteConfigurationSetResult"
      ( \s h x ->
          DeleteConfigurationSetResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteConfigurationSet where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteConfigurationSet where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteConfigurationSet where
  toQuery DeleteConfigurationSet' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteConfigurationSet" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "ConfigurationSetName" Lude.=: configurationSetName
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkDeleteConfigurationSetResponse' smart constructor.
newtype DeleteConfigurationSetResponse = DeleteConfigurationSetResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteConfigurationSetResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteConfigurationSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteConfigurationSetResponse
mkDeleteConfigurationSetResponse pResponseStatus_ =
  DeleteConfigurationSetResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteConfigurationSetResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DeleteConfigurationSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteConfigurationSetResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
