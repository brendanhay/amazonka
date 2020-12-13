{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.DeregisterOnPremisesInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters an on-premises instance.
module Network.AWS.CodeDeploy.DeregisterOnPremisesInstance
  ( -- * Creating a request
    DeregisterOnPremisesInstance (..),
    mkDeregisterOnPremisesInstance,

    -- ** Request lenses
    dopiInstanceName,

    -- * Destructuring the response
    DeregisterOnPremisesInstanceResponse (..),
    mkDeregisterOnPremisesInstanceResponse,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @DeregisterOnPremisesInstance@ operation.
--
-- /See:/ 'mkDeregisterOnPremisesInstance' smart constructor.
newtype DeregisterOnPremisesInstance = DeregisterOnPremisesInstance'
  { -- | The name of the on-premises instance to deregister.
    instanceName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterOnPremisesInstance' with the minimum fields required to make a request.
--
-- * 'instanceName' - The name of the on-premises instance to deregister.
mkDeregisterOnPremisesInstance ::
  -- | 'instanceName'
  Lude.Text ->
  DeregisterOnPremisesInstance
mkDeregisterOnPremisesInstance pInstanceName_ =
  DeregisterOnPremisesInstance' {instanceName = pInstanceName_}

-- | The name of the on-premises instance to deregister.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dopiInstanceName :: Lens.Lens' DeregisterOnPremisesInstance Lude.Text
dopiInstanceName = Lens.lens (instanceName :: DeregisterOnPremisesInstance -> Lude.Text) (\s a -> s {instanceName = a} :: DeregisterOnPremisesInstance)
{-# DEPRECATED dopiInstanceName "Use generic-lens or generic-optics with 'instanceName' instead." #-}

instance Lude.AWSRequest DeregisterOnPremisesInstance where
  type
    Rs DeregisterOnPremisesInstance =
      DeregisterOnPremisesInstanceResponse
  request = Req.postJSON codeDeployService
  response = Res.receiveNull DeregisterOnPremisesInstanceResponse'

instance Lude.ToHeaders DeregisterOnPremisesInstance where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeDeploy_20141006.DeregisterOnPremisesInstance" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeregisterOnPremisesInstance where
  toJSON DeregisterOnPremisesInstance' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("instanceName" Lude..= instanceName)])

instance Lude.ToPath DeregisterOnPremisesInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery DeregisterOnPremisesInstance where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeregisterOnPremisesInstanceResponse' smart constructor.
data DeregisterOnPremisesInstanceResponse = DeregisterOnPremisesInstanceResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterOnPremisesInstanceResponse' with the minimum fields required to make a request.
mkDeregisterOnPremisesInstanceResponse ::
  DeregisterOnPremisesInstanceResponse
mkDeregisterOnPremisesInstanceResponse =
  DeregisterOnPremisesInstanceResponse'
