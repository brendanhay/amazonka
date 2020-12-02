{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.EsamSignalProcessingNotification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.EsamSignalProcessingNotification where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | ESAM SignalProcessingNotification data defined by OC-SP-ESAM-API-I03-131025.
--
-- /See:/ 'esamSignalProcessingNotification' smart constructor.
newtype EsamSignalProcessingNotification = EsamSignalProcessingNotification'
  { _espnSccXML ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EsamSignalProcessingNotification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'espnSccXML' - Provide your ESAM SignalProcessingNotification XML document inside your JSON job settings. Form the XML document as per OC-SP-ESAM-API-I03-131025. The transcoder will use the signal processing instructions in the message that you supply. Provide your ESAM SignalProcessingNotification XML document inside your JSON job settings. For your MPEG2-TS file outputs, if you want the service to place SCTE-35 markers at the insertion points you specify in the XML document, you must also enable SCTE-35 ESAM (scte35Esam). Note that you can either specify an ESAM XML document or enable SCTE-35 passthrough. You can't do both.
esamSignalProcessingNotification ::
  EsamSignalProcessingNotification
esamSignalProcessingNotification =
  EsamSignalProcessingNotification' {_espnSccXML = Nothing}

-- | Provide your ESAM SignalProcessingNotification XML document inside your JSON job settings. Form the XML document as per OC-SP-ESAM-API-I03-131025. The transcoder will use the signal processing instructions in the message that you supply. Provide your ESAM SignalProcessingNotification XML document inside your JSON job settings. For your MPEG2-TS file outputs, if you want the service to place SCTE-35 markers at the insertion points you specify in the XML document, you must also enable SCTE-35 ESAM (scte35Esam). Note that you can either specify an ESAM XML document or enable SCTE-35 passthrough. You can't do both.
espnSccXML :: Lens' EsamSignalProcessingNotification (Maybe Text)
espnSccXML = lens _espnSccXML (\s a -> s {_espnSccXML = a})

instance FromJSON EsamSignalProcessingNotification where
  parseJSON =
    withObject
      "EsamSignalProcessingNotification"
      (\x -> EsamSignalProcessingNotification' <$> (x .:? "sccXml"))

instance Hashable EsamSignalProcessingNotification

instance NFData EsamSignalProcessingNotification

instance ToJSON EsamSignalProcessingNotification where
  toJSON EsamSignalProcessingNotification' {..} =
    object (catMaybes [("sccXml" .=) <$> _espnSccXML])
