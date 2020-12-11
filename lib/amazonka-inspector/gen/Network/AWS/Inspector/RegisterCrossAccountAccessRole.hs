{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.RegisterCrossAccountAccessRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers the IAM role that grants Amazon Inspector access to AWS Services needed to perform security assessments.
module Network.AWS.Inspector.RegisterCrossAccountAccessRole
  ( -- * Creating a request
    RegisterCrossAccountAccessRole (..),
    mkRegisterCrossAccountAccessRole,

    -- ** Request lenses
    rcaarRoleARN,

    -- * Destructuring the response
    RegisterCrossAccountAccessRoleResponse (..),
    mkRegisterCrossAccountAccessRoleResponse,
  )
where

import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRegisterCrossAccountAccessRole' smart constructor.
newtype RegisterCrossAccountAccessRole = RegisterCrossAccountAccessRole'
  { roleARN ::
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

-- | Creates a value of 'RegisterCrossAccountAccessRole' with the minimum fields required to make a request.
--
-- * 'roleARN' - The ARN of the IAM role that grants Amazon Inspector access to AWS Services needed to perform security assessments.
mkRegisterCrossAccountAccessRole ::
  -- | 'roleARN'
  Lude.Text ->
  RegisterCrossAccountAccessRole
mkRegisterCrossAccountAccessRole pRoleARN_ =
  RegisterCrossAccountAccessRole' {roleARN = pRoleARN_}

-- | The ARN of the IAM role that grants Amazon Inspector access to AWS Services needed to perform security assessments.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcaarRoleARN :: Lens.Lens' RegisterCrossAccountAccessRole Lude.Text
rcaarRoleARN = Lens.lens (roleARN :: RegisterCrossAccountAccessRole -> Lude.Text) (\s a -> s {roleARN = a} :: RegisterCrossAccountAccessRole)
{-# DEPRECATED rcaarRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest RegisterCrossAccountAccessRole where
  type
    Rs RegisterCrossAccountAccessRole =
      RegisterCrossAccountAccessRoleResponse
  request = Req.postJSON inspectorService
  response = Res.receiveNull RegisterCrossAccountAccessRoleResponse'

instance Lude.ToHeaders RegisterCrossAccountAccessRole where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "InspectorService.RegisterCrossAccountAccessRole" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RegisterCrossAccountAccessRole where
  toJSON RegisterCrossAccountAccessRole' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("roleArn" Lude..= roleARN)])

instance Lude.ToPath RegisterCrossAccountAccessRole where
  toPath = Lude.const "/"

instance Lude.ToQuery RegisterCrossAccountAccessRole where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRegisterCrossAccountAccessRoleResponse' smart constructor.
data RegisterCrossAccountAccessRoleResponse = RegisterCrossAccountAccessRoleResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterCrossAccountAccessRoleResponse' with the minimum fields required to make a request.
mkRegisterCrossAccountAccessRoleResponse ::
  RegisterCrossAccountAccessRoleResponse
mkRegisterCrossAccountAccessRoleResponse =
  RegisterCrossAccountAccessRoleResponse'
