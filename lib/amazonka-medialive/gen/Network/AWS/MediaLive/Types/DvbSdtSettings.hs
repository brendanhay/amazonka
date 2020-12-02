{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.DvbSdtSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.DvbSdtSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.DvbSdtOutputSdt
import Network.AWS.Prelude

-- | DVB Service Description Table (SDT)
--
-- /See:/ 'dvbSdtSettings' smart constructor.
data DvbSdtSettings = DvbSdtSettings'
  { _dssRepInterval ::
      !(Maybe Nat),
    _dssServiceProviderName :: !(Maybe Text),
    _dssOutputSdt :: !(Maybe DvbSdtOutputSdt),
    _dssServiceName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DvbSdtSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dssRepInterval' - The number of milliseconds between instances of this table in the output transport stream.
--
-- * 'dssServiceProviderName' - The service provider name placed in the serviceDescriptor in the Service Description Table. Maximum length is 256 characters.
--
-- * 'dssOutputSdt' - Selects method of inserting SDT information into output stream. The sdtFollow setting copies SDT information from input stream to output stream. The sdtFollowIfPresent setting copies SDT information from input stream to output stream if SDT information is present in the input, otherwise it will fall back on the user-defined values. The sdtManual setting means user will enter the SDT information. The sdtNone setting means output stream will not contain SDT information.
--
-- * 'dssServiceName' - The service name placed in the serviceDescriptor in the Service Description Table. Maximum length is 256 characters.
dvbSdtSettings ::
  DvbSdtSettings
dvbSdtSettings =
  DvbSdtSettings'
    { _dssRepInterval = Nothing,
      _dssServiceProviderName = Nothing,
      _dssOutputSdt = Nothing,
      _dssServiceName = Nothing
    }

-- | The number of milliseconds between instances of this table in the output transport stream.
dssRepInterval :: Lens' DvbSdtSettings (Maybe Natural)
dssRepInterval = lens _dssRepInterval (\s a -> s {_dssRepInterval = a}) . mapping _Nat

-- | The service provider name placed in the serviceDescriptor in the Service Description Table. Maximum length is 256 characters.
dssServiceProviderName :: Lens' DvbSdtSettings (Maybe Text)
dssServiceProviderName = lens _dssServiceProviderName (\s a -> s {_dssServiceProviderName = a})

-- | Selects method of inserting SDT information into output stream. The sdtFollow setting copies SDT information from input stream to output stream. The sdtFollowIfPresent setting copies SDT information from input stream to output stream if SDT information is present in the input, otherwise it will fall back on the user-defined values. The sdtManual setting means user will enter the SDT information. The sdtNone setting means output stream will not contain SDT information.
dssOutputSdt :: Lens' DvbSdtSettings (Maybe DvbSdtOutputSdt)
dssOutputSdt = lens _dssOutputSdt (\s a -> s {_dssOutputSdt = a})

-- | The service name placed in the serviceDescriptor in the Service Description Table. Maximum length is 256 characters.
dssServiceName :: Lens' DvbSdtSettings (Maybe Text)
dssServiceName = lens _dssServiceName (\s a -> s {_dssServiceName = a})

instance FromJSON DvbSdtSettings where
  parseJSON =
    withObject
      "DvbSdtSettings"
      ( \x ->
          DvbSdtSettings'
            <$> (x .:? "repInterval")
            <*> (x .:? "serviceProviderName")
            <*> (x .:? "outputSdt")
            <*> (x .:? "serviceName")
      )

instance Hashable DvbSdtSettings

instance NFData DvbSdtSettings

instance ToJSON DvbSdtSettings where
  toJSON DvbSdtSettings' {..} =
    object
      ( catMaybes
          [ ("repInterval" .=) <$> _dssRepInterval,
            ("serviceProviderName" .=) <$> _dssServiceProviderName,
            ("outputSdt" .=) <$> _dssOutputSdt,
            ("serviceName" .=) <$> _dssServiceName
          ]
      )
