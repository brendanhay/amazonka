{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameSessionConnectionInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.GameSessionConnectionInfo
  ( GameSessionConnectionInfo (..)
  -- * Smart constructor
  , mkGameSessionConnectionInfo
  -- * Lenses
  , gsciDnsName
  , gsciGameSessionArn
  , gsciIpAddress
  , gsciMatchedPlayerSessions
  , gsciPort
  ) where

import qualified Network.AWS.GameLift.Types.DnsName as Types
import qualified Network.AWS.GameLift.Types.GameSessionArn as Types
import qualified Network.AWS.GameLift.Types.MatchedPlayerSession as Types
import qualified Network.AWS.GameLift.Types.StringModel as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Connection information for a new game session that is created in response to a 'StartMatchmaking' request. Once a match is made, the FlexMatch engine creates a new game session for it. This information, including the game session endpoint and player sessions for each player in the original matchmaking request, is added to the 'MatchmakingTicket' , which can be retrieved by calling 'DescribeMatchmaking' .
--
-- /See:/ 'mkGameSessionConnectionInfo' smart constructor.
data GameSessionConnectionInfo = GameSessionConnectionInfo'
  { dnsName :: Core.Maybe Types.DnsName
    -- ^ DNS identifier assigned to the instance that is running the game session. Values have the following format:
--
--
--     * TLS-enabled fleets: @<unique identifier>.<region identifier>.amazongamelift.com@ .
--
--
--     * Non-TLS-enabled fleets: @ec2-<unique identifier>.compute.amazonaws.com@ . (See <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-instance-addressing.html#concepts-public-addresses Amazon EC2 Instance IP Addressing> .)
--
--
-- When connecting to a game session that is running on a TLS-enabled fleet, you must use the DNS name, not the IP address.
  , gameSessionArn :: Core.Maybe Types.GameSessionArn
    -- ^ Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a game session and uniquely identifies it.
  , ipAddress :: Core.Maybe Types.StringModel
    -- ^ IP address of the instance that is running the game session. When connecting to a Amazon GameLift game server, a client needs to reference an IP address (or DNS name) and port number.
  , matchedPlayerSessions :: Core.Maybe [Types.MatchedPlayerSession]
    -- ^ A collection of player session IDs, one for each player ID that was included in the original matchmaking request. 
  , port :: Core.Maybe Core.Natural
    -- ^ Port number for the game session. To connect to a Amazon GameLift game server, an app needs both the IP address and port number.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GameSessionConnectionInfo' value with any optional fields omitted.
mkGameSessionConnectionInfo
    :: GameSessionConnectionInfo
mkGameSessionConnectionInfo
  = GameSessionConnectionInfo'{dnsName = Core.Nothing,
                               gameSessionArn = Core.Nothing, ipAddress = Core.Nothing,
                               matchedPlayerSessions = Core.Nothing, port = Core.Nothing}

-- | DNS identifier assigned to the instance that is running the game session. Values have the following format:
--
--
--     * TLS-enabled fleets: @<unique identifier>.<region identifier>.amazongamelift.com@ .
--
--
--     * Non-TLS-enabled fleets: @ec2-<unique identifier>.compute.amazonaws.com@ . (See <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-instance-addressing.html#concepts-public-addresses Amazon EC2 Instance IP Addressing> .)
--
--
-- When connecting to a game session that is running on a TLS-enabled fleet, you must use the DNS name, not the IP address.
--
-- /Note:/ Consider using 'dnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsciDnsName :: Lens.Lens' GameSessionConnectionInfo (Core.Maybe Types.DnsName)
gsciDnsName = Lens.field @"dnsName"
{-# INLINEABLE gsciDnsName #-}
{-# DEPRECATED dnsName "Use generic-lens or generic-optics with 'dnsName' instead"  #-}

-- | Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a game session and uniquely identifies it.
--
-- /Note:/ Consider using 'gameSessionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsciGameSessionArn :: Lens.Lens' GameSessionConnectionInfo (Core.Maybe Types.GameSessionArn)
gsciGameSessionArn = Lens.field @"gameSessionArn"
{-# INLINEABLE gsciGameSessionArn #-}
{-# DEPRECATED gameSessionArn "Use generic-lens or generic-optics with 'gameSessionArn' instead"  #-}

-- | IP address of the instance that is running the game session. When connecting to a Amazon GameLift game server, a client needs to reference an IP address (or DNS name) and port number.
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsciIpAddress :: Lens.Lens' GameSessionConnectionInfo (Core.Maybe Types.StringModel)
gsciIpAddress = Lens.field @"ipAddress"
{-# INLINEABLE gsciIpAddress #-}
{-# DEPRECATED ipAddress "Use generic-lens or generic-optics with 'ipAddress' instead"  #-}

-- | A collection of player session IDs, one for each player ID that was included in the original matchmaking request. 
--
-- /Note:/ Consider using 'matchedPlayerSessions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsciMatchedPlayerSessions :: Lens.Lens' GameSessionConnectionInfo (Core.Maybe [Types.MatchedPlayerSession])
gsciMatchedPlayerSessions = Lens.field @"matchedPlayerSessions"
{-# INLINEABLE gsciMatchedPlayerSessions #-}
{-# DEPRECATED matchedPlayerSessions "Use generic-lens or generic-optics with 'matchedPlayerSessions' instead"  #-}

-- | Port number for the game session. To connect to a Amazon GameLift game server, an app needs both the IP address and port number.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsciPort :: Lens.Lens' GameSessionConnectionInfo (Core.Maybe Core.Natural)
gsciPort = Lens.field @"port"
{-# INLINEABLE gsciPort #-}
{-# DEPRECATED port "Use generic-lens or generic-optics with 'port' instead"  #-}

instance Core.FromJSON GameSessionConnectionInfo where
        parseJSON
          = Core.withObject "GameSessionConnectionInfo" Core.$
              \ x ->
                GameSessionConnectionInfo' Core.<$>
                  (x Core..:? "DnsName") Core.<*> x Core..:? "GameSessionArn"
                    Core.<*> x Core..:? "IpAddress"
                    Core.<*> x Core..:? "MatchedPlayerSessions"
                    Core.<*> x Core..:? "Port"
