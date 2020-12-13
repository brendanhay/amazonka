{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ReplaceIAMInstanceProfileAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces an IAM instance profile for the specified running instance. You can use this action to change the IAM instance profile that's associated with an instance without having to disassociate the existing IAM instance profile first.
--
-- Use 'DescribeIamInstanceProfileAssociations' to get the association ID.
module Network.AWS.EC2.ReplaceIAMInstanceProfileAssociation
  ( -- * Creating a request
    ReplaceIAMInstanceProfileAssociation (..),
    mkReplaceIAMInstanceProfileAssociation,

    -- ** Request lenses
    riapaAssociationId,
    riapaIAMInstanceProfile,

    -- * Destructuring the response
    ReplaceIAMInstanceProfileAssociationResponse (..),
    mkReplaceIAMInstanceProfileAssociationResponse,

    -- ** Response lenses
    riaparsIAMInstanceProfileAssociation,
    riaparsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkReplaceIAMInstanceProfileAssociation' smart constructor.
data ReplaceIAMInstanceProfileAssociation = ReplaceIAMInstanceProfileAssociation'
  { -- | The ID of the existing IAM instance profile association.
    associationId :: Lude.Text,
    -- | The IAM instance profile.
    iamInstanceProfile :: IAMInstanceProfileSpecification
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplaceIAMInstanceProfileAssociation' with the minimum fields required to make a request.
--
-- * 'associationId' - The ID of the existing IAM instance profile association.
-- * 'iamInstanceProfile' - The IAM instance profile.
mkReplaceIAMInstanceProfileAssociation ::
  -- | 'associationId'
  Lude.Text ->
  -- | 'iamInstanceProfile'
  IAMInstanceProfileSpecification ->
  ReplaceIAMInstanceProfileAssociation
mkReplaceIAMInstanceProfileAssociation
  pAssociationId_
  pIAMInstanceProfile_ =
    ReplaceIAMInstanceProfileAssociation'
      { associationId =
          pAssociationId_,
        iamInstanceProfile = pIAMInstanceProfile_
      }

-- | The ID of the existing IAM instance profile association.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riapaAssociationId :: Lens.Lens' ReplaceIAMInstanceProfileAssociation Lude.Text
riapaAssociationId = Lens.lens (associationId :: ReplaceIAMInstanceProfileAssociation -> Lude.Text) (\s a -> s {associationId = a} :: ReplaceIAMInstanceProfileAssociation)
{-# DEPRECATED riapaAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | The IAM instance profile.
--
-- /Note:/ Consider using 'iamInstanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riapaIAMInstanceProfile :: Lens.Lens' ReplaceIAMInstanceProfileAssociation IAMInstanceProfileSpecification
riapaIAMInstanceProfile = Lens.lens (iamInstanceProfile :: ReplaceIAMInstanceProfileAssociation -> IAMInstanceProfileSpecification) (\s a -> s {iamInstanceProfile = a} :: ReplaceIAMInstanceProfileAssociation)
{-# DEPRECATED riapaIAMInstanceProfile "Use generic-lens or generic-optics with 'iamInstanceProfile' instead." #-}

instance Lude.AWSRequest ReplaceIAMInstanceProfileAssociation where
  type
    Rs ReplaceIAMInstanceProfileAssociation =
      ReplaceIAMInstanceProfileAssociationResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ReplaceIAMInstanceProfileAssociationResponse'
            Lude.<$> (x Lude..@? "iamInstanceProfileAssociation")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ReplaceIAMInstanceProfileAssociation where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ReplaceIAMInstanceProfileAssociation where
  toPath = Lude.const "/"

instance Lude.ToQuery ReplaceIAMInstanceProfileAssociation where
  toQuery ReplaceIAMInstanceProfileAssociation' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ReplaceIamInstanceProfileAssociation" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "AssociationId" Lude.=: associationId,
        "IamInstanceProfile" Lude.=: iamInstanceProfile
      ]

-- | /See:/ 'mkReplaceIAMInstanceProfileAssociationResponse' smart constructor.
data ReplaceIAMInstanceProfileAssociationResponse = ReplaceIAMInstanceProfileAssociationResponse'
  { -- | Information about the IAM instance profile association.
    iamInstanceProfileAssociation :: Lude.Maybe IAMInstanceProfileAssociation,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplaceIAMInstanceProfileAssociationResponse' with the minimum fields required to make a request.
--
-- * 'iamInstanceProfileAssociation' - Information about the IAM instance profile association.
-- * 'responseStatus' - The response status code.
mkReplaceIAMInstanceProfileAssociationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ReplaceIAMInstanceProfileAssociationResponse
mkReplaceIAMInstanceProfileAssociationResponse pResponseStatus_ =
  ReplaceIAMInstanceProfileAssociationResponse'
    { iamInstanceProfileAssociation =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the IAM instance profile association.
--
-- /Note:/ Consider using 'iamInstanceProfileAssociation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riaparsIAMInstanceProfileAssociation :: Lens.Lens' ReplaceIAMInstanceProfileAssociationResponse (Lude.Maybe IAMInstanceProfileAssociation)
riaparsIAMInstanceProfileAssociation = Lens.lens (iamInstanceProfileAssociation :: ReplaceIAMInstanceProfileAssociationResponse -> Lude.Maybe IAMInstanceProfileAssociation) (\s a -> s {iamInstanceProfileAssociation = a} :: ReplaceIAMInstanceProfileAssociationResponse)
{-# DEPRECATED riaparsIAMInstanceProfileAssociation "Use generic-lens or generic-optics with 'iamInstanceProfileAssociation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riaparsResponseStatus :: Lens.Lens' ReplaceIAMInstanceProfileAssociationResponse Lude.Int
riaparsResponseStatus = Lens.lens (responseStatus :: ReplaceIAMInstanceProfileAssociationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ReplaceIAMInstanceProfileAssociationResponse)
{-# DEPRECATED riaparsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
