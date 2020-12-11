-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ConnectionLogOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ConnectionLogOptions
  ( ConnectionLogOptions (..),

    -- * Smart constructor
    mkConnectionLogOptions,

    -- * Lenses
    cloEnabled,
    cloCloudwatchLogStream,
    cloCloudwatchLogGroup,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the client connection logging options for the Client VPN endpoint.
--
-- /See:/ 'mkConnectionLogOptions' smart constructor.
data ConnectionLogOptions = ConnectionLogOptions'
  { enabled ::
      Lude.Maybe Lude.Bool,
    cloudwatchLogStream :: Lude.Maybe Lude.Text,
    cloudwatchLogGroup :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConnectionLogOptions' with the minimum fields required to make a request.
--
-- * 'cloudwatchLogGroup' - The name of the CloudWatch Logs log group. Required if connection logging is enabled.
-- * 'cloudwatchLogStream' - The name of the CloudWatch Logs log stream to which the connection data is published.
-- * 'enabled' - Indicates whether connection logging is enabled.
mkConnectionLogOptions ::
  ConnectionLogOptions
mkConnectionLogOptions =
  ConnectionLogOptions'
    { enabled = Lude.Nothing,
      cloudwatchLogStream = Lude.Nothing,
      cloudwatchLogGroup = Lude.Nothing
    }

-- | Indicates whether connection logging is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cloEnabled :: Lens.Lens' ConnectionLogOptions (Lude.Maybe Lude.Bool)
cloEnabled = Lens.lens (enabled :: ConnectionLogOptions -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: ConnectionLogOptions)
{-# DEPRECATED cloEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The name of the CloudWatch Logs log stream to which the connection data is published.
--
-- /Note:/ Consider using 'cloudwatchLogStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cloCloudwatchLogStream :: Lens.Lens' ConnectionLogOptions (Lude.Maybe Lude.Text)
cloCloudwatchLogStream = Lens.lens (cloudwatchLogStream :: ConnectionLogOptions -> Lude.Maybe Lude.Text) (\s a -> s {cloudwatchLogStream = a} :: ConnectionLogOptions)
{-# DEPRECATED cloCloudwatchLogStream "Use generic-lens or generic-optics with 'cloudwatchLogStream' instead." #-}

-- | The name of the CloudWatch Logs log group. Required if connection logging is enabled.
--
-- /Note:/ Consider using 'cloudwatchLogGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cloCloudwatchLogGroup :: Lens.Lens' ConnectionLogOptions (Lude.Maybe Lude.Text)
cloCloudwatchLogGroup = Lens.lens (cloudwatchLogGroup :: ConnectionLogOptions -> Lude.Maybe Lude.Text) (\s a -> s {cloudwatchLogGroup = a} :: ConnectionLogOptions)
{-# DEPRECATED cloCloudwatchLogGroup "Use generic-lens or generic-optics with 'cloudwatchLogGroup' instead." #-}

instance Lude.ToQuery ConnectionLogOptions where
  toQuery ConnectionLogOptions' {..} =
    Lude.mconcat
      [ "Enabled" Lude.=: enabled,
        "CloudwatchLogStream" Lude.=: cloudwatchLogStream,
        "CloudwatchLogGroup" Lude.=: cloudwatchLogGroup
      ]
