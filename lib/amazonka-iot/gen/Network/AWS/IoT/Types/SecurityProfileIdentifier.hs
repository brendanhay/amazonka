{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.SecurityProfileIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.SecurityProfileIdentifier
  ( SecurityProfileIdentifier (..),

    -- * Smart constructor
    mkSecurityProfileIdentifier,

    -- * Lenses
    spiName,
    spiArn,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Identifying information for a Device Defender security profile.
--
-- /See:/ 'mkSecurityProfileIdentifier' smart constructor.
data SecurityProfileIdentifier = SecurityProfileIdentifier'
  { name ::
      Lude.Text,
    arn :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SecurityProfileIdentifier' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the security profile.
-- * 'name' - The name you have given to the security profile.
mkSecurityProfileIdentifier ::
  -- | 'name'
  Lude.Text ->
  -- | 'arn'
  Lude.Text ->
  SecurityProfileIdentifier
mkSecurityProfileIdentifier pName_ pArn_ =
  SecurityProfileIdentifier' {name = pName_, arn = pArn_}

-- | The name you have given to the security profile.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spiName :: Lens.Lens' SecurityProfileIdentifier Lude.Text
spiName = Lens.lens (name :: SecurityProfileIdentifier -> Lude.Text) (\s a -> s {name = a} :: SecurityProfileIdentifier)
{-# DEPRECATED spiName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ARN of the security profile.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spiArn :: Lens.Lens' SecurityProfileIdentifier Lude.Text
spiArn = Lens.lens (arn :: SecurityProfileIdentifier -> Lude.Text) (\s a -> s {arn = a} :: SecurityProfileIdentifier)
{-# DEPRECATED spiArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.FromJSON SecurityProfileIdentifier where
  parseJSON =
    Lude.withObject
      "SecurityProfileIdentifier"
      ( \x ->
          SecurityProfileIdentifier'
            Lude.<$> (x Lude..: "name") Lude.<*> (x Lude..: "arn")
      )
