{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.AWSIAMConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.AWSIAMConfig
  ( AWSIAMConfig (..),

    -- * Smart constructor
    mkAWSIAMConfig,

    -- * Lenses
    aicSigningServiceName,
    aicSigningRegion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The AWS IAM configuration.
--
-- /See:/ 'mkAWSIAMConfig' smart constructor.
data AWSIAMConfig = AWSIAMConfig'
  { signingServiceName ::
      Lude.Maybe Lude.Text,
    signingRegion :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AWSIAMConfig' with the minimum fields required to make a request.
--
-- * 'signingRegion' - The signing region for AWS IAM authorization.
-- * 'signingServiceName' - The signing service name for AWS IAM authorization.
mkAWSIAMConfig ::
  AWSIAMConfig
mkAWSIAMConfig =
  AWSIAMConfig'
    { signingServiceName = Lude.Nothing,
      signingRegion = Lude.Nothing
    }

-- | The signing service name for AWS IAM authorization.
--
-- /Note:/ Consider using 'signingServiceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aicSigningServiceName :: Lens.Lens' AWSIAMConfig (Lude.Maybe Lude.Text)
aicSigningServiceName = Lens.lens (signingServiceName :: AWSIAMConfig -> Lude.Maybe Lude.Text) (\s a -> s {signingServiceName = a} :: AWSIAMConfig)
{-# DEPRECATED aicSigningServiceName "Use generic-lens or generic-optics with 'signingServiceName' instead." #-}

-- | The signing region for AWS IAM authorization.
--
-- /Note:/ Consider using 'signingRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aicSigningRegion :: Lens.Lens' AWSIAMConfig (Lude.Maybe Lude.Text)
aicSigningRegion = Lens.lens (signingRegion :: AWSIAMConfig -> Lude.Maybe Lude.Text) (\s a -> s {signingRegion = a} :: AWSIAMConfig)
{-# DEPRECATED aicSigningRegion "Use generic-lens or generic-optics with 'signingRegion' instead." #-}

instance Lude.FromJSON AWSIAMConfig where
  parseJSON =
    Lude.withObject
      "AWSIAMConfig"
      ( \x ->
          AWSIAMConfig'
            Lude.<$> (x Lude..:? "signingServiceName")
            Lude.<*> (x Lude..:? "signingRegion")
      )

instance Lude.ToJSON AWSIAMConfig where
  toJSON AWSIAMConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("signingServiceName" Lude..=) Lude.<$> signingServiceName,
            ("signingRegion" Lude..=) Lude.<$> signingRegion
          ]
      )
