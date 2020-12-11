-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.EsamManifestConfirmConditionNotification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.EsamManifestConfirmConditionNotification
  ( EsamManifestConfirmConditionNotification (..),

    -- * Smart constructor
    mkEsamManifestConfirmConditionNotification,

    -- * Lenses
    emccnMccXML,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | ESAM ManifestConfirmConditionNotification defined by OC-SP-ESAM-API-I03-131025.
--
-- /See:/ 'mkEsamManifestConfirmConditionNotification' smart constructor.
newtype EsamManifestConfirmConditionNotification = EsamManifestConfirmConditionNotification'
  { mccXML ::
      Lude.Maybe
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

-- | Creates a value of 'EsamManifestConfirmConditionNotification' with the minimum fields required to make a request.
--
-- * 'mccXML' - Provide your ESAM ManifestConfirmConditionNotification XML document inside your JSON job settings. Form the XML document as per OC-SP-ESAM-API-I03-131025. The transcoder will use the Manifest Conditioning instructions in the message that you supply.
mkEsamManifestConfirmConditionNotification ::
  EsamManifestConfirmConditionNotification
mkEsamManifestConfirmConditionNotification =
  EsamManifestConfirmConditionNotification' {mccXML = Lude.Nothing}

-- | Provide your ESAM ManifestConfirmConditionNotification XML document inside your JSON job settings. Form the XML document as per OC-SP-ESAM-API-I03-131025. The transcoder will use the Manifest Conditioning instructions in the message that you supply.
--
-- /Note:/ Consider using 'mccXML' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emccnMccXML :: Lens.Lens' EsamManifestConfirmConditionNotification (Lude.Maybe Lude.Text)
emccnMccXML = Lens.lens (mccXML :: EsamManifestConfirmConditionNotification -> Lude.Maybe Lude.Text) (\s a -> s {mccXML = a} :: EsamManifestConfirmConditionNotification)
{-# DEPRECATED emccnMccXML "Use generic-lens or generic-optics with 'mccXML' instead." #-}

instance Lude.FromJSON EsamManifestConfirmConditionNotification where
  parseJSON =
    Lude.withObject
      "EsamManifestConfirmConditionNotification"
      ( \x ->
          EsamManifestConfirmConditionNotification'
            Lude.<$> (x Lude..:? "mccXml")
      )

instance Lude.ToJSON EsamManifestConfirmConditionNotification where
  toJSON EsamManifestConfirmConditionNotification' {..} =
    Lude.object (Lude.catMaybes [("mccXml" Lude..=) Lude.<$> mccXML])
