{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.RegisterOnPremisesInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers an on-premises instance.
module Network.AWS.CodeDeploy.RegisterOnPremisesInstance
  ( -- * Creating a request
    RegisterOnPremisesInstance (..),
    mkRegisterOnPremisesInstance,

    -- ** Request lenses
    ropiIamUserARN,
    ropiInstanceName,
    ropiIamSessionARN,

    -- * Destructuring the response
    RegisterOnPremisesInstanceResponse (..),
    mkRegisterOnPremisesInstanceResponse,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of the register on-premises instance operation.
--
-- /See:/ 'mkRegisterOnPremisesInstance' smart constructor.
data RegisterOnPremisesInstance = RegisterOnPremisesInstance'
  { -- | The ARN of the IAM user to associate with the on-premises instance.
    iamUserARN :: Lude.Maybe Lude.Text,
    -- | The name of the on-premises instance to register.
    instanceName :: Lude.Text,
    -- | The ARN of the IAM session to associate with the on-premises instance.
    iamSessionARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterOnPremisesInstance' with the minimum fields required to make a request.
--
-- * 'iamUserARN' - The ARN of the IAM user to associate with the on-premises instance.
-- * 'instanceName' - The name of the on-premises instance to register.
-- * 'iamSessionARN' - The ARN of the IAM session to associate with the on-premises instance.
mkRegisterOnPremisesInstance ::
  -- | 'instanceName'
  Lude.Text ->
  RegisterOnPremisesInstance
mkRegisterOnPremisesInstance pInstanceName_ =
  RegisterOnPremisesInstance'
    { iamUserARN = Lude.Nothing,
      instanceName = pInstanceName_,
      iamSessionARN = Lude.Nothing
    }

-- | The ARN of the IAM user to associate with the on-premises instance.
--
-- /Note:/ Consider using 'iamUserARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ropiIamUserARN :: Lens.Lens' RegisterOnPremisesInstance (Lude.Maybe Lude.Text)
ropiIamUserARN = Lens.lens (iamUserARN :: RegisterOnPremisesInstance -> Lude.Maybe Lude.Text) (\s a -> s {iamUserARN = a} :: RegisterOnPremisesInstance)
{-# DEPRECATED ropiIamUserARN "Use generic-lens or generic-optics with 'iamUserARN' instead." #-}

-- | The name of the on-premises instance to register.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ropiInstanceName :: Lens.Lens' RegisterOnPremisesInstance Lude.Text
ropiInstanceName = Lens.lens (instanceName :: RegisterOnPremisesInstance -> Lude.Text) (\s a -> s {instanceName = a} :: RegisterOnPremisesInstance)
{-# DEPRECATED ropiInstanceName "Use generic-lens or generic-optics with 'instanceName' instead." #-}

-- | The ARN of the IAM session to associate with the on-premises instance.
--
-- /Note:/ Consider using 'iamSessionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ropiIamSessionARN :: Lens.Lens' RegisterOnPremisesInstance (Lude.Maybe Lude.Text)
ropiIamSessionARN = Lens.lens (iamSessionARN :: RegisterOnPremisesInstance -> Lude.Maybe Lude.Text) (\s a -> s {iamSessionARN = a} :: RegisterOnPremisesInstance)
{-# DEPRECATED ropiIamSessionARN "Use generic-lens or generic-optics with 'iamSessionARN' instead." #-}

instance Lude.AWSRequest RegisterOnPremisesInstance where
  type
    Rs RegisterOnPremisesInstance =
      RegisterOnPremisesInstanceResponse
  request = Req.postJSON codeDeployService
  response = Res.receiveNull RegisterOnPremisesInstanceResponse'

instance Lude.ToHeaders RegisterOnPremisesInstance where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeDeploy_20141006.RegisterOnPremisesInstance" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RegisterOnPremisesInstance where
  toJSON RegisterOnPremisesInstance' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("iamUserArn" Lude..=) Lude.<$> iamUserARN,
            Lude.Just ("instanceName" Lude..= instanceName),
            ("iamSessionArn" Lude..=) Lude.<$> iamSessionARN
          ]
      )

instance Lude.ToPath RegisterOnPremisesInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery RegisterOnPremisesInstance where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRegisterOnPremisesInstanceResponse' smart constructor.
data RegisterOnPremisesInstanceResponse = RegisterOnPremisesInstanceResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterOnPremisesInstanceResponse' with the minimum fields required to make a request.
mkRegisterOnPremisesInstanceResponse ::
  RegisterOnPremisesInstanceResponse
mkRegisterOnPremisesInstanceResponse =
  RegisterOnPremisesInstanceResponse'
