-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotInstanceStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotInstanceStatus
  ( SpotInstanceStatus (..),

    -- * Smart constructor
    mkSpotInstanceStatus,

    -- * Lenses
    sisUpdateTime,
    sisCode,
    sisMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the status of a Spot Instance request.
--
-- /See:/ 'mkSpotInstanceStatus' smart constructor.
data SpotInstanceStatus = SpotInstanceStatus'
  { updateTime ::
      Lude.Maybe Lude.ISO8601,
    code :: Lude.Maybe Lude.Text,
    message :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SpotInstanceStatus' with the minimum fields required to make a request.
--
-- * 'code' - The status code. For a list of status codes, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-bid-status.html#spot-instance-bid-status-understand Spot status codes> in the /Amazon EC2 User Guide for Linux Instances/ .
-- * 'message' - The description for the status code.
-- * 'updateTime' - The date and time of the most recent status update, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
mkSpotInstanceStatus ::
  SpotInstanceStatus
mkSpotInstanceStatus =
  SpotInstanceStatus'
    { updateTime = Lude.Nothing,
      code = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The date and time of the most recent status update, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
--
-- /Note:/ Consider using 'updateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sisUpdateTime :: Lens.Lens' SpotInstanceStatus (Lude.Maybe Lude.ISO8601)
sisUpdateTime = Lens.lens (updateTime :: SpotInstanceStatus -> Lude.Maybe Lude.ISO8601) (\s a -> s {updateTime = a} :: SpotInstanceStatus)
{-# DEPRECATED sisUpdateTime "Use generic-lens or generic-optics with 'updateTime' instead." #-}

-- | The status code. For a list of status codes, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-bid-status.html#spot-instance-bid-status-understand Spot status codes> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sisCode :: Lens.Lens' SpotInstanceStatus (Lude.Maybe Lude.Text)
sisCode = Lens.lens (code :: SpotInstanceStatus -> Lude.Maybe Lude.Text) (\s a -> s {code = a} :: SpotInstanceStatus)
{-# DEPRECATED sisCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | The description for the status code.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sisMessage :: Lens.Lens' SpotInstanceStatus (Lude.Maybe Lude.Text)
sisMessage = Lens.lens (message :: SpotInstanceStatus -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: SpotInstanceStatus)
{-# DEPRECATED sisMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromXML SpotInstanceStatus where
  parseXML x =
    SpotInstanceStatus'
      Lude.<$> (x Lude..@? "updateTime")
      Lude.<*> (x Lude..@? "code")
      Lude.<*> (x Lude..@? "message")
