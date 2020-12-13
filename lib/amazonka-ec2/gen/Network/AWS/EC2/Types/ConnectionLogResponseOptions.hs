{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ConnectionLogResponseOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ConnectionLogResponseOptions
  ( ConnectionLogResponseOptions (..),

    -- * Smart constructor
    mkConnectionLogResponseOptions,

    -- * Lenses
    clroEnabled,
    clroCloudwatchLogStream,
    clroCloudwatchLogGroup,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the client connection logging options for a Client VPN endpoint.
--
-- /See:/ 'mkConnectionLogResponseOptions' smart constructor.
data ConnectionLogResponseOptions = ConnectionLogResponseOptions'
  { -- | Indicates whether client connection logging is enabled for the Client VPN endpoint.
    enabled :: Lude.Maybe Lude.Bool,
    -- | The name of the Amazon CloudWatch Logs log stream to which connection logging data is published.
    cloudwatchLogStream :: Lude.Maybe Lude.Text,
    -- | The name of the Amazon CloudWatch Logs log group to which connection logging data is published.
    cloudwatchLogGroup :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConnectionLogResponseOptions' with the minimum fields required to make a request.
--
-- * 'enabled' - Indicates whether client connection logging is enabled for the Client VPN endpoint.
-- * 'cloudwatchLogStream' - The name of the Amazon CloudWatch Logs log stream to which connection logging data is published.
-- * 'cloudwatchLogGroup' - The name of the Amazon CloudWatch Logs log group to which connection logging data is published.
mkConnectionLogResponseOptions ::
  ConnectionLogResponseOptions
mkConnectionLogResponseOptions =
  ConnectionLogResponseOptions'
    { enabled = Lude.Nothing,
      cloudwatchLogStream = Lude.Nothing,
      cloudwatchLogGroup = Lude.Nothing
    }

-- | Indicates whether client connection logging is enabled for the Client VPN endpoint.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clroEnabled :: Lens.Lens' ConnectionLogResponseOptions (Lude.Maybe Lude.Bool)
clroEnabled = Lens.lens (enabled :: ConnectionLogResponseOptions -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: ConnectionLogResponseOptions)
{-# DEPRECATED clroEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The name of the Amazon CloudWatch Logs log stream to which connection logging data is published.
--
-- /Note:/ Consider using 'cloudwatchLogStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clroCloudwatchLogStream :: Lens.Lens' ConnectionLogResponseOptions (Lude.Maybe Lude.Text)
clroCloudwatchLogStream = Lens.lens (cloudwatchLogStream :: ConnectionLogResponseOptions -> Lude.Maybe Lude.Text) (\s a -> s {cloudwatchLogStream = a} :: ConnectionLogResponseOptions)
{-# DEPRECATED clroCloudwatchLogStream "Use generic-lens or generic-optics with 'cloudwatchLogStream' instead." #-}

-- | The name of the Amazon CloudWatch Logs log group to which connection logging data is published.
--
-- /Note:/ Consider using 'cloudwatchLogGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clroCloudwatchLogGroup :: Lens.Lens' ConnectionLogResponseOptions (Lude.Maybe Lude.Text)
clroCloudwatchLogGroup = Lens.lens (cloudwatchLogGroup :: ConnectionLogResponseOptions -> Lude.Maybe Lude.Text) (\s a -> s {cloudwatchLogGroup = a} :: ConnectionLogResponseOptions)
{-# DEPRECATED clroCloudwatchLogGroup "Use generic-lens or generic-optics with 'cloudwatchLogGroup' instead." #-}

instance Lude.FromXML ConnectionLogResponseOptions where
  parseXML x =
    ConnectionLogResponseOptions'
      Lude.<$> (x Lude..@? "Enabled")
      Lude.<*> (x Lude..@? "CloudwatchLogStream")
      Lude.<*> (x Lude..@? "CloudwatchLogGroup")
