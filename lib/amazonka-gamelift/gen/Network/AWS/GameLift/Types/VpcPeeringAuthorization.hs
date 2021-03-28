{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.VpcPeeringAuthorization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.VpcPeeringAuthorization
  ( VpcPeeringAuthorization (..)
  -- * Smart constructor
  , mkVpcPeeringAuthorization
  -- * Lenses
  , vpaCreationTime
  , vpaExpirationTime
  , vpaGameLiftAwsAccountId
  , vpaPeerVpcAwsAccountId
  , vpaPeerVpcId
  ) where

import qualified Network.AWS.GameLift.Types.NonZeroAndMaxString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

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
-- /See:/ 'mkVpcPeeringAuthorization' smart constructor.
data VpcPeeringAuthorization = VpcPeeringAuthorization'
  { creationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ Time stamp indicating when this authorization was issued. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
  , expirationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ Time stamp indicating when this authorization expires (24 hours after issuance). Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
  , gameLiftAwsAccountId :: Core.Maybe Types.NonZeroAndMaxString
    -- ^ A unique identifier for the AWS account that you use to manage your Amazon GameLift fleet. You can find your Account ID in the AWS Management Console under account settings.
  , peerVpcAwsAccountId :: Core.Maybe Types.NonZeroAndMaxString
    -- ^ 
  , peerVpcId :: Core.Maybe Types.NonZeroAndMaxString
    -- ^ A unique identifier for a VPC with resources to be accessed by your Amazon GameLift fleet. The VPC must be in the same Region where your fleet is deployed. Look up a VPC ID using the <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS Management Console. Learn more about VPC peering in <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'VpcPeeringAuthorization' value with any optional fields omitted.
mkVpcPeeringAuthorization
    :: VpcPeeringAuthorization
mkVpcPeeringAuthorization
  = VpcPeeringAuthorization'{creationTime = Core.Nothing,
                             expirationTime = Core.Nothing, gameLiftAwsAccountId = Core.Nothing,
                             peerVpcAwsAccountId = Core.Nothing, peerVpcId = Core.Nothing}

-- | Time stamp indicating when this authorization was issued. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpaCreationTime :: Lens.Lens' VpcPeeringAuthorization (Core.Maybe Core.NominalDiffTime)
vpaCreationTime = Lens.field @"creationTime"
{-# INLINEABLE vpaCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | Time stamp indicating when this authorization expires (24 hours after issuance). Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- /Note:/ Consider using 'expirationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpaExpirationTime :: Lens.Lens' VpcPeeringAuthorization (Core.Maybe Core.NominalDiffTime)
vpaExpirationTime = Lens.field @"expirationTime"
{-# INLINEABLE vpaExpirationTime #-}
{-# DEPRECATED expirationTime "Use generic-lens or generic-optics with 'expirationTime' instead"  #-}

-- | A unique identifier for the AWS account that you use to manage your Amazon GameLift fleet. You can find your Account ID in the AWS Management Console under account settings.
--
-- /Note:/ Consider using 'gameLiftAwsAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpaGameLiftAwsAccountId :: Lens.Lens' VpcPeeringAuthorization (Core.Maybe Types.NonZeroAndMaxString)
vpaGameLiftAwsAccountId = Lens.field @"gameLiftAwsAccountId"
{-# INLINEABLE vpaGameLiftAwsAccountId #-}
{-# DEPRECATED gameLiftAwsAccountId "Use generic-lens or generic-optics with 'gameLiftAwsAccountId' instead"  #-}

-- | 
--
-- /Note:/ Consider using 'peerVpcAwsAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpaPeerVpcAwsAccountId :: Lens.Lens' VpcPeeringAuthorization (Core.Maybe Types.NonZeroAndMaxString)
vpaPeerVpcAwsAccountId = Lens.field @"peerVpcAwsAccountId"
{-# INLINEABLE vpaPeerVpcAwsAccountId #-}
{-# DEPRECATED peerVpcAwsAccountId "Use generic-lens or generic-optics with 'peerVpcAwsAccountId' instead"  #-}

-- | A unique identifier for a VPC with resources to be accessed by your Amazon GameLift fleet. The VPC must be in the same Region where your fleet is deployed. Look up a VPC ID using the <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS Management Console. Learn more about VPC peering in <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets> .
--
-- /Note:/ Consider using 'peerVpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpaPeerVpcId :: Lens.Lens' VpcPeeringAuthorization (Core.Maybe Types.NonZeroAndMaxString)
vpaPeerVpcId = Lens.field @"peerVpcId"
{-# INLINEABLE vpaPeerVpcId #-}
{-# DEPRECATED peerVpcId "Use generic-lens or generic-optics with 'peerVpcId' instead"  #-}

instance Core.FromJSON VpcPeeringAuthorization where
        parseJSON
          = Core.withObject "VpcPeeringAuthorization" Core.$
              \ x ->
                VpcPeeringAuthorization' Core.<$>
                  (x Core..:? "CreationTime") Core.<*> x Core..:? "ExpirationTime"
                    Core.<*> x Core..:? "GameLiftAwsAccountId"
                    Core.<*> x Core..:? "PeerVpcAwsAccountId"
                    Core.<*> x Core..:? "PeerVpcId"
