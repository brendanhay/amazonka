{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.CodeSigningConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.CodeSigningConfig
  ( CodeSigningConfig (..),

    -- * Smart constructor
    mkCodeSigningConfig,

    -- * Lenses
    cscAllowedPublishers,
    cscCodeSigningPolicies,
    cscCodeSigningConfigARN,
    cscLastModified,
    cscDescription,
    cscCodeSigningConfigId,
  )
where

import Network.AWS.Lambda.Types.AllowedPublishers
import Network.AWS.Lambda.Types.CodeSigningPolicies
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details about a Code signing configuration.
--
-- /See:/ 'mkCodeSigningConfig' smart constructor.
data CodeSigningConfig = CodeSigningConfig'
  { -- | List of allowed publishers.
    allowedPublishers :: AllowedPublishers,
    -- | The code signing policy controls the validation failure action for signature mismatch or expiry.
    codeSigningPolicies :: CodeSigningPolicies,
    -- | The Amazon Resource Name (ARN) of the Code signing configuration.
    codeSigningConfigARN :: Lude.Text,
    -- | The date and time that the Code signing configuration was last modified, in ISO-8601 format (YYYY-MM-DDThh:mm:ss.sTZD).
    lastModified :: Lude.Text,
    -- | Code signing configuration description.
    description :: Lude.Maybe Lude.Text,
    -- | Unique identifer for the Code signing configuration.
    codeSigningConfigId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CodeSigningConfig' with the minimum fields required to make a request.
--
-- * 'allowedPublishers' - List of allowed publishers.
-- * 'codeSigningPolicies' - The code signing policy controls the validation failure action for signature mismatch or expiry.
-- * 'codeSigningConfigARN' - The Amazon Resource Name (ARN) of the Code signing configuration.
-- * 'lastModified' - The date and time that the Code signing configuration was last modified, in ISO-8601 format (YYYY-MM-DDThh:mm:ss.sTZD).
-- * 'description' - Code signing configuration description.
-- * 'codeSigningConfigId' - Unique identifer for the Code signing configuration.
mkCodeSigningConfig ::
  -- | 'allowedPublishers'
  AllowedPublishers ->
  -- | 'codeSigningPolicies'
  CodeSigningPolicies ->
  -- | 'codeSigningConfigARN'
  Lude.Text ->
  -- | 'lastModified'
  Lude.Text ->
  -- | 'codeSigningConfigId'
  Lude.Text ->
  CodeSigningConfig
mkCodeSigningConfig
  pAllowedPublishers_
  pCodeSigningPolicies_
  pCodeSigningConfigARN_
  pLastModified_
  pCodeSigningConfigId_ =
    CodeSigningConfig'
      { allowedPublishers = pAllowedPublishers_,
        codeSigningPolicies = pCodeSigningPolicies_,
        codeSigningConfigARN = pCodeSigningConfigARN_,
        lastModified = pLastModified_,
        description = Lude.Nothing,
        codeSigningConfigId = pCodeSigningConfigId_
      }

-- | List of allowed publishers.
--
-- /Note:/ Consider using 'allowedPublishers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscAllowedPublishers :: Lens.Lens' CodeSigningConfig AllowedPublishers
cscAllowedPublishers = Lens.lens (allowedPublishers :: CodeSigningConfig -> AllowedPublishers) (\s a -> s {allowedPublishers = a} :: CodeSigningConfig)
{-# DEPRECATED cscAllowedPublishers "Use generic-lens or generic-optics with 'allowedPublishers' instead." #-}

-- | The code signing policy controls the validation failure action for signature mismatch or expiry.
--
-- /Note:/ Consider using 'codeSigningPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscCodeSigningPolicies :: Lens.Lens' CodeSigningConfig CodeSigningPolicies
cscCodeSigningPolicies = Lens.lens (codeSigningPolicies :: CodeSigningConfig -> CodeSigningPolicies) (\s a -> s {codeSigningPolicies = a} :: CodeSigningConfig)
{-# DEPRECATED cscCodeSigningPolicies "Use generic-lens or generic-optics with 'codeSigningPolicies' instead." #-}

-- | The Amazon Resource Name (ARN) of the Code signing configuration.
--
-- /Note:/ Consider using 'codeSigningConfigARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscCodeSigningConfigARN :: Lens.Lens' CodeSigningConfig Lude.Text
cscCodeSigningConfigARN = Lens.lens (codeSigningConfigARN :: CodeSigningConfig -> Lude.Text) (\s a -> s {codeSigningConfigARN = a} :: CodeSigningConfig)
{-# DEPRECATED cscCodeSigningConfigARN "Use generic-lens or generic-optics with 'codeSigningConfigARN' instead." #-}

-- | The date and time that the Code signing configuration was last modified, in ISO-8601 format (YYYY-MM-DDThh:mm:ss.sTZD).
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscLastModified :: Lens.Lens' CodeSigningConfig Lude.Text
cscLastModified = Lens.lens (lastModified :: CodeSigningConfig -> Lude.Text) (\s a -> s {lastModified = a} :: CodeSigningConfig)
{-# DEPRECATED cscLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}

-- | Code signing configuration description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscDescription :: Lens.Lens' CodeSigningConfig (Lude.Maybe Lude.Text)
cscDescription = Lens.lens (description :: CodeSigningConfig -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CodeSigningConfig)
{-# DEPRECATED cscDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Unique identifer for the Code signing configuration.
--
-- /Note:/ Consider using 'codeSigningConfigId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscCodeSigningConfigId :: Lens.Lens' CodeSigningConfig Lude.Text
cscCodeSigningConfigId = Lens.lens (codeSigningConfigId :: CodeSigningConfig -> Lude.Text) (\s a -> s {codeSigningConfigId = a} :: CodeSigningConfig)
{-# DEPRECATED cscCodeSigningConfigId "Use generic-lens or generic-optics with 'codeSigningConfigId' instead." #-}

instance Lude.FromJSON CodeSigningConfig where
  parseJSON =
    Lude.withObject
      "CodeSigningConfig"
      ( \x ->
          CodeSigningConfig'
            Lude.<$> (x Lude..: "AllowedPublishers")
            Lude.<*> (x Lude..: "CodeSigningPolicies")
            Lude.<*> (x Lude..: "CodeSigningConfigArn")
            Lude.<*> (x Lude..: "LastModified")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..: "CodeSigningConfigId")
      )
