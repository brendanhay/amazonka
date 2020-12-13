{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AssociateIAMInstanceProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates an IAM instance profile with a running or stopped instance. You cannot associate more than one IAM instance profile with an instance.
module Network.AWS.EC2.AssociateIAMInstanceProfile
  ( -- * Creating a request
    AssociateIAMInstanceProfile (..),
    mkAssociateIAMInstanceProfile,

    -- ** Request lenses
    aiapInstanceId,
    aiapIAMInstanceProfile,

    -- * Destructuring the response
    AssociateIAMInstanceProfileResponse (..),
    mkAssociateIAMInstanceProfileResponse,

    -- ** Response lenses
    aiaprsIAMInstanceProfileAssociation,
    aiaprsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAssociateIAMInstanceProfile' smart constructor.
data AssociateIAMInstanceProfile = AssociateIAMInstanceProfile'
  { -- | The ID of the instance.
    instanceId :: Lude.Text,
    -- | The IAM instance profile.
    iamInstanceProfile :: IAMInstanceProfileSpecification
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateIAMInstanceProfile' with the minimum fields required to make a request.
--
-- * 'instanceId' - The ID of the instance.
-- * 'iamInstanceProfile' - The IAM instance profile.
mkAssociateIAMInstanceProfile ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'iamInstanceProfile'
  IAMInstanceProfileSpecification ->
  AssociateIAMInstanceProfile
mkAssociateIAMInstanceProfile pInstanceId_ pIAMInstanceProfile_ =
  AssociateIAMInstanceProfile'
    { instanceId = pInstanceId_,
      iamInstanceProfile = pIAMInstanceProfile_
    }

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiapInstanceId :: Lens.Lens' AssociateIAMInstanceProfile Lude.Text
aiapInstanceId = Lens.lens (instanceId :: AssociateIAMInstanceProfile -> Lude.Text) (\s a -> s {instanceId = a} :: AssociateIAMInstanceProfile)
{-# DEPRECATED aiapInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The IAM instance profile.
--
-- /Note:/ Consider using 'iamInstanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiapIAMInstanceProfile :: Lens.Lens' AssociateIAMInstanceProfile IAMInstanceProfileSpecification
aiapIAMInstanceProfile = Lens.lens (iamInstanceProfile :: AssociateIAMInstanceProfile -> IAMInstanceProfileSpecification) (\s a -> s {iamInstanceProfile = a} :: AssociateIAMInstanceProfile)
{-# DEPRECATED aiapIAMInstanceProfile "Use generic-lens or generic-optics with 'iamInstanceProfile' instead." #-}

instance Lude.AWSRequest AssociateIAMInstanceProfile where
  type
    Rs AssociateIAMInstanceProfile =
      AssociateIAMInstanceProfileResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          AssociateIAMInstanceProfileResponse'
            Lude.<$> (x Lude..@? "iamInstanceProfileAssociation")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssociateIAMInstanceProfile where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AssociateIAMInstanceProfile where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociateIAMInstanceProfile where
  toQuery AssociateIAMInstanceProfile' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("AssociateIamInstanceProfile" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "InstanceId" Lude.=: instanceId,
        "IamInstanceProfile" Lude.=: iamInstanceProfile
      ]

-- | /See:/ 'mkAssociateIAMInstanceProfileResponse' smart constructor.
data AssociateIAMInstanceProfileResponse = AssociateIAMInstanceProfileResponse'
  { -- | Information about the IAM instance profile association.
    iamInstanceProfileAssociation :: Lude.Maybe IAMInstanceProfileAssociation,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateIAMInstanceProfileResponse' with the minimum fields required to make a request.
--
-- * 'iamInstanceProfileAssociation' - Information about the IAM instance profile association.
-- * 'responseStatus' - The response status code.
mkAssociateIAMInstanceProfileResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssociateIAMInstanceProfileResponse
mkAssociateIAMInstanceProfileResponse pResponseStatus_ =
  AssociateIAMInstanceProfileResponse'
    { iamInstanceProfileAssociation =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the IAM instance profile association.
--
-- /Note:/ Consider using 'iamInstanceProfileAssociation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiaprsIAMInstanceProfileAssociation :: Lens.Lens' AssociateIAMInstanceProfileResponse (Lude.Maybe IAMInstanceProfileAssociation)
aiaprsIAMInstanceProfileAssociation = Lens.lens (iamInstanceProfileAssociation :: AssociateIAMInstanceProfileResponse -> Lude.Maybe IAMInstanceProfileAssociation) (\s a -> s {iamInstanceProfileAssociation = a} :: AssociateIAMInstanceProfileResponse)
{-# DEPRECATED aiaprsIAMInstanceProfileAssociation "Use generic-lens or generic-optics with 'iamInstanceProfileAssociation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiaprsResponseStatus :: Lens.Lens' AssociateIAMInstanceProfileResponse Lude.Int
aiaprsResponseStatus = Lens.lens (responseStatus :: AssociateIAMInstanceProfileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociateIAMInstanceProfileResponse)
{-# DEPRECATED aiaprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
