-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameSessionConnectionInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameSessionConnectionInfo
  ( GameSessionConnectionInfo (..),

    -- * Smart constructor
    mkGameSessionConnectionInfo,

    -- * Lenses
    gsciMatchedPlayerSessions,
    gsciIPAddress,
    gsciGameSessionARN,
    gsciDNSName,
    gsciPort,
  )
where

import Network.AWS.GameLift.Types.MatchedPlayerSession
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Connection information for a new game session that is created in response to a 'StartMatchmaking' request. Once a match is made, the FlexMatch engine creates a new game session for it. This information, including the game session endpoint and player sessions for each player in the original matchmaking request, is added to the 'MatchmakingTicket' , which can be retrieved by calling 'DescribeMatchmaking' .
--
-- /See:/ 'mkGameSessionConnectionInfo' smart constructor.
data GameSessionConnectionInfo = GameSessionConnectionInfo'
  { matchedPlayerSessions ::
      Lude.Maybe [MatchedPlayerSession],
    ipAddress :: Lude.Maybe Lude.Text,
    gameSessionARN :: Lude.Maybe Lude.Text,
    dnsName :: Lude.Maybe Lude.Text,
    port :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GameSessionConnectionInfo' with the minimum fields required to make a request.
--
-- * 'dnsName' - DNS identifier assigned to the instance that is running the game session. Values have the following format:
--
--
--     * TLS-enabled fleets: @<unique identifier>.<region identifier>.amazongamelift.com@ .
--
--
--     * Non-TLS-enabled fleets: @ec2-<unique identifier>.compute.amazonaws.com@ . (See <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-instance-addressing.html#concepts-public-addresses Amazon EC2 Instance IP Addressing> .)
--
--
-- When connecting to a game session that is running on a TLS-enabled fleet, you must use the DNS name, not the IP address.
-- * 'gameSessionARN' - Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a game session and uniquely identifies it.
-- * 'ipAddress' - IP address of the instance that is running the game session. When connecting to a Amazon GameLift game server, a client needs to reference an IP address (or DNS name) and port number.
-- * 'matchedPlayerSessions' - A collection of player session IDs, one for each player ID that was included in the original matchmaking request.
-- * 'port' - Port number for the game session. To connect to a Amazon GameLift game server, an app needs both the IP address and port number.
mkGameSessionConnectionInfo ::
  GameSessionConnectionInfo
mkGameSessionConnectionInfo =
  GameSessionConnectionInfo'
    { matchedPlayerSessions = Lude.Nothing,
      ipAddress = Lude.Nothing,
      gameSessionARN = Lude.Nothing,
      dnsName = Lude.Nothing,
      port = Lude.Nothing
    }

-- | A collection of player session IDs, one for each player ID that was included in the original matchmaking request.
--
-- /Note:/ Consider using 'matchedPlayerSessions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsciMatchedPlayerSessions :: Lens.Lens' GameSessionConnectionInfo (Lude.Maybe [MatchedPlayerSession])
gsciMatchedPlayerSessions = Lens.lens (matchedPlayerSessions :: GameSessionConnectionInfo -> Lude.Maybe [MatchedPlayerSession]) (\s a -> s {matchedPlayerSessions = a} :: GameSessionConnectionInfo)
{-# DEPRECATED gsciMatchedPlayerSessions "Use generic-lens or generic-optics with 'matchedPlayerSessions' instead." #-}

-- | IP address of the instance that is running the game session. When connecting to a Amazon GameLift game server, a client needs to reference an IP address (or DNS name) and port number.
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsciIPAddress :: Lens.Lens' GameSessionConnectionInfo (Lude.Maybe Lude.Text)
gsciIPAddress = Lens.lens (ipAddress :: GameSessionConnectionInfo -> Lude.Maybe Lude.Text) (\s a -> s {ipAddress = a} :: GameSessionConnectionInfo)
{-# DEPRECATED gsciIPAddress "Use generic-lens or generic-optics with 'ipAddress' instead." #-}

-- | Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a game session and uniquely identifies it.
--
-- /Note:/ Consider using 'gameSessionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsciGameSessionARN :: Lens.Lens' GameSessionConnectionInfo (Lude.Maybe Lude.Text)
gsciGameSessionARN = Lens.lens (gameSessionARN :: GameSessionConnectionInfo -> Lude.Maybe Lude.Text) (\s a -> s {gameSessionARN = a} :: GameSessionConnectionInfo)
{-# DEPRECATED gsciGameSessionARN "Use generic-lens or generic-optics with 'gameSessionARN' instead." #-}

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
gsciDNSName :: Lens.Lens' GameSessionConnectionInfo (Lude.Maybe Lude.Text)
gsciDNSName = Lens.lens (dnsName :: GameSessionConnectionInfo -> Lude.Maybe Lude.Text) (\s a -> s {dnsName = a} :: GameSessionConnectionInfo)
{-# DEPRECATED gsciDNSName "Use generic-lens or generic-optics with 'dnsName' instead." #-}

-- | Port number for the game session. To connect to a Amazon GameLift game server, an app needs both the IP address and port number.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsciPort :: Lens.Lens' GameSessionConnectionInfo (Lude.Maybe Lude.Natural)
gsciPort = Lens.lens (port :: GameSessionConnectionInfo -> Lude.Maybe Lude.Natural) (\s a -> s {port = a} :: GameSessionConnectionInfo)
{-# DEPRECATED gsciPort "Use generic-lens or generic-optics with 'port' instead." #-}

instance Lude.FromJSON GameSessionConnectionInfo where
  parseJSON =
    Lude.withObject
      "GameSessionConnectionInfo"
      ( \x ->
          GameSessionConnectionInfo'
            Lude.<$> (x Lude..:? "MatchedPlayerSessions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "IpAddress")
            Lude.<*> (x Lude..:? "GameSessionArn")
            Lude.<*> (x Lude..:? "DnsName")
            Lude.<*> (x Lude..:? "Port")
      )
