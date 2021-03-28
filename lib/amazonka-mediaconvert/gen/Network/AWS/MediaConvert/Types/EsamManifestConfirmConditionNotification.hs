{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.EsamManifestConfirmConditionNotification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.EsamManifestConfirmConditionNotification
  ( EsamManifestConfirmConditionNotification (..)
  -- * Smart constructor
  , mkEsamManifestConfirmConditionNotification
  -- * Lenses
  , emccnMccXml
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | ESAM ManifestConfirmConditionNotification defined by OC-SP-ESAM-API-I03-131025.
--
-- /See:/ 'mkEsamManifestConfirmConditionNotification' smart constructor.
newtype EsamManifestConfirmConditionNotification = EsamManifestConfirmConditionNotification'
  { mccXml :: Core.Maybe Core.Text
    -- ^ Provide your ESAM ManifestConfirmConditionNotification XML document inside your JSON job settings. Form the XML document as per OC-SP-ESAM-API-I03-131025. The transcoder will use the Manifest Conditioning instructions in the message that you supply.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'EsamManifestConfirmConditionNotification' value with any optional fields omitted.
mkEsamManifestConfirmConditionNotification
    :: EsamManifestConfirmConditionNotification
mkEsamManifestConfirmConditionNotification
  = EsamManifestConfirmConditionNotification'{mccXml = Core.Nothing}

-- | Provide your ESAM ManifestConfirmConditionNotification XML document inside your JSON job settings. Form the XML document as per OC-SP-ESAM-API-I03-131025. The transcoder will use the Manifest Conditioning instructions in the message that you supply.
--
-- /Note:/ Consider using 'mccXml' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emccnMccXml :: Lens.Lens' EsamManifestConfirmConditionNotification (Core.Maybe Core.Text)
emccnMccXml = Lens.field @"mccXml"
{-# INLINEABLE emccnMccXml #-}
{-# DEPRECATED mccXml "Use generic-lens or generic-optics with 'mccXml' instead"  #-}

instance Core.FromJSON EsamManifestConfirmConditionNotification
         where
        toJSON EsamManifestConfirmConditionNotification{..}
          = Core.object (Core.catMaybes [("mccXml" Core..=) Core.<$> mccXml])

instance Core.FromJSON EsamManifestConfirmConditionNotification
         where
        parseJSON
          = Core.withObject "EsamManifestConfirmConditionNotification" Core.$
              \ x ->
                EsamManifestConfirmConditionNotification' Core.<$>
                  (x Core..:? "mccXml")
