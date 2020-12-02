{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.EsamManifestConfirmConditionNotification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.EsamManifestConfirmConditionNotification where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | ESAM ManifestConfirmConditionNotification defined by OC-SP-ESAM-API-I03-131025.
--
-- /See:/ 'esamManifestConfirmConditionNotification' smart constructor.
newtype EsamManifestConfirmConditionNotification = EsamManifestConfirmConditionNotification'
  { _emccnMccXML ::
      Maybe
        Text
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'EsamManifestConfirmConditionNotification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'emccnMccXML' - Provide your ESAM ManifestConfirmConditionNotification XML document inside your JSON job settings. Form the XML document as per OC-SP-ESAM-API-I03-131025. The transcoder will use the Manifest Conditioning instructions in the message that you supply.
esamManifestConfirmConditionNotification ::
  EsamManifestConfirmConditionNotification
esamManifestConfirmConditionNotification =
  EsamManifestConfirmConditionNotification' {_emccnMccXML = Nothing}

-- | Provide your ESAM ManifestConfirmConditionNotification XML document inside your JSON job settings. Form the XML document as per OC-SP-ESAM-API-I03-131025. The transcoder will use the Manifest Conditioning instructions in the message that you supply.
emccnMccXML :: Lens' EsamManifestConfirmConditionNotification (Maybe Text)
emccnMccXML = lens _emccnMccXML (\s a -> s {_emccnMccXML = a})

instance FromJSON EsamManifestConfirmConditionNotification where
  parseJSON =
    withObject
      "EsamManifestConfirmConditionNotification"
      ( \x ->
          EsamManifestConfirmConditionNotification' <$> (x .:? "mccXml")
      )

instance Hashable EsamManifestConfirmConditionNotification

instance NFData EsamManifestConfirmConditionNotification

instance ToJSON EsamManifestConfirmConditionNotification where
  toJSON EsamManifestConfirmConditionNotification' {..} =
    object (catMaybes [("mccXml" .=) <$> _emccnMccXML])
