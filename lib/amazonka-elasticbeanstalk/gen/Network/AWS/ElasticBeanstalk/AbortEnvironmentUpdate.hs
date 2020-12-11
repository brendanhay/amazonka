{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.AbortEnvironmentUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels in-progress environment configuration update or application version deployment.
module Network.AWS.ElasticBeanstalk.AbortEnvironmentUpdate
  ( -- * Creating a request
    AbortEnvironmentUpdate (..),
    mkAbortEnvironmentUpdate,

    -- ** Request lenses
    aeuEnvironmentName,
    aeuEnvironmentId,

    -- * Destructuring the response
    AbortEnvironmentUpdateResponse (..),
    mkAbortEnvironmentUpdateResponse,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkAbortEnvironmentUpdate' smart constructor.
data AbortEnvironmentUpdate = AbortEnvironmentUpdate'
  { environmentName ::
      Lude.Maybe Lude.Text,
    environmentId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AbortEnvironmentUpdate' with the minimum fields required to make a request.
--
-- * 'environmentId' - This specifies the ID of the environment with the in-progress update that you want to cancel.
-- * 'environmentName' - This specifies the name of the environment with the in-progress update that you want to cancel.
mkAbortEnvironmentUpdate ::
  AbortEnvironmentUpdate
mkAbortEnvironmentUpdate =
  AbortEnvironmentUpdate'
    { environmentName = Lude.Nothing,
      environmentId = Lude.Nothing
    }

-- | This specifies the name of the environment with the in-progress update that you want to cancel.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeuEnvironmentName :: Lens.Lens' AbortEnvironmentUpdate (Lude.Maybe Lude.Text)
aeuEnvironmentName = Lens.lens (environmentName :: AbortEnvironmentUpdate -> Lude.Maybe Lude.Text) (\s a -> s {environmentName = a} :: AbortEnvironmentUpdate)
{-# DEPRECATED aeuEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

-- | This specifies the ID of the environment with the in-progress update that you want to cancel.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeuEnvironmentId :: Lens.Lens' AbortEnvironmentUpdate (Lude.Maybe Lude.Text)
aeuEnvironmentId = Lens.lens (environmentId :: AbortEnvironmentUpdate -> Lude.Maybe Lude.Text) (\s a -> s {environmentId = a} :: AbortEnvironmentUpdate)
{-# DEPRECATED aeuEnvironmentId "Use generic-lens or generic-optics with 'environmentId' instead." #-}

instance Lude.AWSRequest AbortEnvironmentUpdate where
  type Rs AbortEnvironmentUpdate = AbortEnvironmentUpdateResponse
  request = Req.postQuery elasticBeanstalkService
  response = Res.receiveNull AbortEnvironmentUpdateResponse'

instance Lude.ToHeaders AbortEnvironmentUpdate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AbortEnvironmentUpdate where
  toPath = Lude.const "/"

instance Lude.ToQuery AbortEnvironmentUpdate where
  toQuery AbortEnvironmentUpdate' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("AbortEnvironmentUpdate" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "EnvironmentName" Lude.=: environmentName,
        "EnvironmentId" Lude.=: environmentId
      ]

-- | /See:/ 'mkAbortEnvironmentUpdateResponse' smart constructor.
data AbortEnvironmentUpdateResponse = AbortEnvironmentUpdateResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AbortEnvironmentUpdateResponse' with the minimum fields required to make a request.
mkAbortEnvironmentUpdateResponse ::
  AbortEnvironmentUpdateResponse
mkAbortEnvironmentUpdateResponse = AbortEnvironmentUpdateResponse'
