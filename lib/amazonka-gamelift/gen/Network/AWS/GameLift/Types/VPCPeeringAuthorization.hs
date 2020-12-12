{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.VPCPeeringAuthorization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.VPCPeeringAuthorization
  ( VPCPeeringAuthorization (..),

    -- * Smart constructor
    mkVPCPeeringAuthorization,

    -- * Lenses
    vpaCreationTime,
    vpaPeerVPCId,
    vpaPeerVPCAWSAccountId,
    vpaGameLiftAWSAccountId,
    vpaExpirationTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents an authorization for a VPC peering connection between the VPC for an Amazon GameLift fleet and another VPC on an account you have access to. This authorization must exist and be valid for the peering connection to be established. Authorizations are valid for 24 hours after they are issued.
--
--
--     * 'CreateVpcPeeringAuthorization'
--
--
--     * 'DescribeVpcPeeringAuthorizations'
--
--
--     * 'DeleteVpcPeeringAuthorization'
--
--
--     * 'CreateVpcPeeringConnection'
--
--
--     * 'DescribeVpcPeeringConnections'
--
--
--     * 'DeleteVpcPeeringConnection'
--
--
--
-- /See:/ 'mkVPCPeeringAuthorization' smart constructor.
data VPCPeeringAuthorization = VPCPeeringAuthorization'
  { creationTime ::
      Lude.Maybe Lude.Timestamp,
    peerVPCId :: Lude.Maybe Lude.Text,
    peerVPCAWSAccountId :: Lude.Maybe Lude.Text,
    gameLiftAWSAccountId ::
      Lude.Maybe Lude.Text,
    expirationTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VPCPeeringAuthorization' with the minimum fields required to make a request.
--
-- * 'creationTime' - Time stamp indicating when this authorization was issued. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
-- * 'expirationTime' - Time stamp indicating when this authorization expires (24 hours after issuance). Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
-- * 'gameLiftAWSAccountId' - A unique identifier for the AWS account that you use to manage your Amazon GameLift fleet. You can find your Account ID in the AWS Management Console under account settings.
-- * 'peerVPCAWSAccountId' -
-- * 'peerVPCId' - A unique identifier for a VPC with resources to be accessed by your Amazon GameLift fleet. The VPC must be in the same Region where your fleet is deployed. Look up a VPC ID using the <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS Management Console. Learn more about VPC peering in <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets> .
mkVPCPeeringAuthorization ::
  VPCPeeringAuthorization
mkVPCPeeringAuthorization =
  VPCPeeringAuthorization'
    { creationTime = Lude.Nothing,
      peerVPCId = Lude.Nothing,
      peerVPCAWSAccountId = Lude.Nothing,
      gameLiftAWSAccountId = Lude.Nothing,
      expirationTime = Lude.Nothing
    }

-- | Time stamp indicating when this authorization was issued. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpaCreationTime :: Lens.Lens' VPCPeeringAuthorization (Lude.Maybe Lude.Timestamp)
vpaCreationTime = Lens.lens (creationTime :: VPCPeeringAuthorization -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: VPCPeeringAuthorization)
{-# DEPRECATED vpaCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | A unique identifier for a VPC with resources to be accessed by your Amazon GameLift fleet. The VPC must be in the same Region where your fleet is deployed. Look up a VPC ID using the <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS Management Console. Learn more about VPC peering in <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets> .
--
-- /Note:/ Consider using 'peerVPCId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpaPeerVPCId :: Lens.Lens' VPCPeeringAuthorization (Lude.Maybe Lude.Text)
vpaPeerVPCId = Lens.lens (peerVPCId :: VPCPeeringAuthorization -> Lude.Maybe Lude.Text) (\s a -> s {peerVPCId = a} :: VPCPeeringAuthorization)
{-# DEPRECATED vpaPeerVPCId "Use generic-lens or generic-optics with 'peerVPCId' instead." #-}

-- |
--
-- /Note:/ Consider using 'peerVPCAWSAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpaPeerVPCAWSAccountId :: Lens.Lens' VPCPeeringAuthorization (Lude.Maybe Lude.Text)
vpaPeerVPCAWSAccountId = Lens.lens (peerVPCAWSAccountId :: VPCPeeringAuthorization -> Lude.Maybe Lude.Text) (\s a -> s {peerVPCAWSAccountId = a} :: VPCPeeringAuthorization)
{-# DEPRECATED vpaPeerVPCAWSAccountId "Use generic-lens or generic-optics with 'peerVPCAWSAccountId' instead." #-}

-- | A unique identifier for the AWS account that you use to manage your Amazon GameLift fleet. You can find your Account ID in the AWS Management Console under account settings.
--
-- /Note:/ Consider using 'gameLiftAWSAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpaGameLiftAWSAccountId :: Lens.Lens' VPCPeeringAuthorization (Lude.Maybe Lude.Text)
vpaGameLiftAWSAccountId = Lens.lens (gameLiftAWSAccountId :: VPCPeeringAuthorization -> Lude.Maybe Lude.Text) (\s a -> s {gameLiftAWSAccountId = a} :: VPCPeeringAuthorization)
{-# DEPRECATED vpaGameLiftAWSAccountId "Use generic-lens or generic-optics with 'gameLiftAWSAccountId' instead." #-}

-- | Time stamp indicating when this authorization expires (24 hours after issuance). Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- /Note:/ Consider using 'expirationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpaExpirationTime :: Lens.Lens' VPCPeeringAuthorization (Lude.Maybe Lude.Timestamp)
vpaExpirationTime = Lens.lens (expirationTime :: VPCPeeringAuthorization -> Lude.Maybe Lude.Timestamp) (\s a -> s {expirationTime = a} :: VPCPeeringAuthorization)
{-# DEPRECATED vpaExpirationTime "Use generic-lens or generic-optics with 'expirationTime' instead." #-}

instance Lude.FromJSON VPCPeeringAuthorization where
  parseJSON =
    Lude.withObject
      "VPCPeeringAuthorization"
      ( \x ->
          VPCPeeringAuthorization'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "PeerVpcId")
            Lude.<*> (x Lude..:? "PeerVpcAwsAccountId")
            Lude.<*> (x Lude..:? "GameLiftAwsAccountId")
            Lude.<*> (x Lude..:? "ExpirationTime")
      )
