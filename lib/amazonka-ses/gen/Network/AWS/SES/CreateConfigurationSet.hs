{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.CreateConfigurationSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a configuration set.
--
-- Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.CreateConfigurationSet
  ( -- * Creating a request
    CreateConfigurationSet (..),
    mkCreateConfigurationSet,

    -- ** Request lenses
    ccsConfigurationSet,

    -- * Destructuring the response
    CreateConfigurationSetResponse (..),
    mkCreateConfigurationSetResponse,

    -- ** Response lenses
    ccsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to create a configuration set. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkCreateConfigurationSet' smart constructor.
newtype CreateConfigurationSet = CreateConfigurationSet'
  { configurationSet ::
      ConfigurationSet
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateConfigurationSet' with the minimum fields required to make a request.
--
-- * 'configurationSet' - A data structure that contains the name of the configuration set.
mkCreateConfigurationSet ::
  -- | 'configurationSet'
  ConfigurationSet ->
  CreateConfigurationSet
mkCreateConfigurationSet pConfigurationSet_ =
  CreateConfigurationSet' {configurationSet = pConfigurationSet_}

-- | A data structure that contains the name of the configuration set.
--
-- /Note:/ Consider using 'configurationSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsConfigurationSet :: Lens.Lens' CreateConfigurationSet ConfigurationSet
ccsConfigurationSet = Lens.lens (configurationSet :: CreateConfigurationSet -> ConfigurationSet) (\s a -> s {configurationSet = a} :: CreateConfigurationSet)
{-# DEPRECATED ccsConfigurationSet "Use generic-lens or generic-optics with 'configurationSet' instead." #-}

instance Lude.AWSRequest CreateConfigurationSet where
  type Rs CreateConfigurationSet = CreateConfigurationSetResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "CreateConfigurationSetResult"
      ( \s h x ->
          CreateConfigurationSetResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateConfigurationSet where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateConfigurationSet where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateConfigurationSet where
  toQuery CreateConfigurationSet' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateConfigurationSet" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "ConfigurationSet" Lude.=: configurationSet
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkCreateConfigurationSetResponse' smart constructor.
newtype CreateConfigurationSetResponse = CreateConfigurationSetResponse'
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

-- | Creates a value of 'CreateConfigurationSetResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCreateConfigurationSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateConfigurationSetResponse
mkCreateConfigurationSetResponse pResponseStatus_ =
  CreateConfigurationSetResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsrsResponseStatus :: Lens.Lens' CreateConfigurationSetResponse Lude.Int
ccsrsResponseStatus = Lens.lens (responseStatus :: CreateConfigurationSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateConfigurationSetResponse)
{-# DEPRECATED ccsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
