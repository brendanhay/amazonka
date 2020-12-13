{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DisassociateIAMInstanceProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates an IAM instance profile from a running or stopped instance.
--
-- Use 'DescribeIamInstanceProfileAssociations' to get the association ID.
module Network.AWS.EC2.DisassociateIAMInstanceProfile
  ( -- * Creating a request
    DisassociateIAMInstanceProfile (..),
    mkDisassociateIAMInstanceProfile,

    -- ** Request lenses
    diapAssociationId,

    -- * Destructuring the response
    DisassociateIAMInstanceProfileResponse (..),
    mkDisassociateIAMInstanceProfileResponse,

    -- ** Response lenses
    diaprsIAMInstanceProfileAssociation,
    diaprsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisassociateIAMInstanceProfile' smart constructor.
newtype DisassociateIAMInstanceProfile = DisassociateIAMInstanceProfile'
  { -- | The ID of the IAM instance profile association.
    associationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateIAMInstanceProfile' with the minimum fields required to make a request.
--
-- * 'associationId' - The ID of the IAM instance profile association.
mkDisassociateIAMInstanceProfile ::
  -- | 'associationId'
  Lude.Text ->
  DisassociateIAMInstanceProfile
mkDisassociateIAMInstanceProfile pAssociationId_ =
  DisassociateIAMInstanceProfile' {associationId = pAssociationId_}

-- | The ID of the IAM instance profile association.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diapAssociationId :: Lens.Lens' DisassociateIAMInstanceProfile Lude.Text
diapAssociationId = Lens.lens (associationId :: DisassociateIAMInstanceProfile -> Lude.Text) (\s a -> s {associationId = a} :: DisassociateIAMInstanceProfile)
{-# DEPRECATED diapAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

instance Lude.AWSRequest DisassociateIAMInstanceProfile where
  type
    Rs DisassociateIAMInstanceProfile =
      DisassociateIAMInstanceProfileResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DisassociateIAMInstanceProfileResponse'
            Lude.<$> (x Lude..@? "iamInstanceProfileAssociation")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisassociateIAMInstanceProfile where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DisassociateIAMInstanceProfile where
  toPath = Lude.const "/"

instance Lude.ToQuery DisassociateIAMInstanceProfile where
  toQuery DisassociateIAMInstanceProfile' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DisassociateIamInstanceProfile" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "AssociationId" Lude.=: associationId
      ]

-- | /See:/ 'mkDisassociateIAMInstanceProfileResponse' smart constructor.
data DisassociateIAMInstanceProfileResponse = DisassociateIAMInstanceProfileResponse'
  { -- | Information about the IAM instance profile association.
    iamInstanceProfileAssociation :: Lude.Maybe IAMInstanceProfileAssociation,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateIAMInstanceProfileResponse' with the minimum fields required to make a request.
--
-- * 'iamInstanceProfileAssociation' - Information about the IAM instance profile association.
-- * 'responseStatus' - The response status code.
mkDisassociateIAMInstanceProfileResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisassociateIAMInstanceProfileResponse
mkDisassociateIAMInstanceProfileResponse pResponseStatus_ =
  DisassociateIAMInstanceProfileResponse'
    { iamInstanceProfileAssociation =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the IAM instance profile association.
--
-- /Note:/ Consider using 'iamInstanceProfileAssociation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diaprsIAMInstanceProfileAssociation :: Lens.Lens' DisassociateIAMInstanceProfileResponse (Lude.Maybe IAMInstanceProfileAssociation)
diaprsIAMInstanceProfileAssociation = Lens.lens (iamInstanceProfileAssociation :: DisassociateIAMInstanceProfileResponse -> Lude.Maybe IAMInstanceProfileAssociation) (\s a -> s {iamInstanceProfileAssociation = a} :: DisassociateIAMInstanceProfileResponse)
{-# DEPRECATED diaprsIAMInstanceProfileAssociation "Use generic-lens or generic-optics with 'iamInstanceProfileAssociation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diaprsResponseStatus :: Lens.Lens' DisassociateIAMInstanceProfileResponse Lude.Int
diaprsResponseStatus = Lens.lens (responseStatus :: DisassociateIAMInstanceProfileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisassociateIAMInstanceProfileResponse)
{-# DEPRECATED diaprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
