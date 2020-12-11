-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AWSJobPresignedURLConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AWSJobPresignedURLConfig
  ( AWSJobPresignedURLConfig (..),

    -- * Smart constructor
    mkAWSJobPresignedURLConfig,

    -- * Lenses
    ajpucExpiresInSec,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configuration information for pre-signed URLs. Valid when @protocols@ contains HTTP.
--
-- /See:/ 'mkAWSJobPresignedURLConfig' smart constructor.
newtype AWSJobPresignedURLConfig = AWSJobPresignedURLConfig'
  { expiresInSec ::
      Lude.Maybe Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AWSJobPresignedURLConfig' with the minimum fields required to make a request.
--
-- * 'expiresInSec' - How long (in seconds) pre-signed URLs are valid. Valid values are 60 - 3600, the default value is 1800 seconds. Pre-signed URLs are generated when a request for the job document is received.
mkAWSJobPresignedURLConfig ::
  AWSJobPresignedURLConfig
mkAWSJobPresignedURLConfig =
  AWSJobPresignedURLConfig' {expiresInSec = Lude.Nothing}

-- | How long (in seconds) pre-signed URLs are valid. Valid values are 60 - 3600, the default value is 1800 seconds. Pre-signed URLs are generated when a request for the job document is received.
--
-- /Note:/ Consider using 'expiresInSec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ajpucExpiresInSec :: Lens.Lens' AWSJobPresignedURLConfig (Lude.Maybe Lude.Integer)
ajpucExpiresInSec = Lens.lens (expiresInSec :: AWSJobPresignedURLConfig -> Lude.Maybe Lude.Integer) (\s a -> s {expiresInSec = a} :: AWSJobPresignedURLConfig)
{-# DEPRECATED ajpucExpiresInSec "Use generic-lens or generic-optics with 'expiresInSec' instead." #-}

instance Lude.FromJSON AWSJobPresignedURLConfig where
  parseJSON =
    Lude.withObject
      "AWSJobPresignedURLConfig"
      ( \x ->
          AWSJobPresignedURLConfig' Lude.<$> (x Lude..:? "expiresInSec")
      )

instance Lude.ToJSON AWSJobPresignedURLConfig where
  toJSON AWSJobPresignedURLConfig' {..} =
    Lude.object
      (Lude.catMaybes [("expiresInSec" Lude..=) Lude.<$> expiresInSec])
