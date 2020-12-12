{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.SecurityProfileTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.SecurityProfileTarget
  ( SecurityProfileTarget (..),

    -- * Smart constructor
    mkSecurityProfileTarget,

    -- * Lenses
    sptArn,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A target to which an alert is sent when a security profile behavior is violated.
--
-- /See:/ 'mkSecurityProfileTarget' smart constructor.
newtype SecurityProfileTarget = SecurityProfileTarget'
  { arn ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SecurityProfileTarget' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the security profile.
mkSecurityProfileTarget ::
  -- | 'arn'
  Lude.Text ->
  SecurityProfileTarget
mkSecurityProfileTarget pArn_ = SecurityProfileTarget' {arn = pArn_}

-- | The ARN of the security profile.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sptArn :: Lens.Lens' SecurityProfileTarget Lude.Text
sptArn = Lens.lens (arn :: SecurityProfileTarget -> Lude.Text) (\s a -> s {arn = a} :: SecurityProfileTarget)
{-# DEPRECATED sptArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.FromJSON SecurityProfileTarget where
  parseJSON =
    Lude.withObject
      "SecurityProfileTarget"
      (\x -> SecurityProfileTarget' Lude.<$> (x Lude..: "arn"))
