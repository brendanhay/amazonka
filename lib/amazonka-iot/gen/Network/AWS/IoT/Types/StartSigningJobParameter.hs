{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.StartSigningJobParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.StartSigningJobParameter where

import Network.AWS.IoT.Types.Destination
import Network.AWS.IoT.Types.SigningProfileParameter
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information required to start a signing job.
--
--
--
-- /See:/ 'startSigningJobParameter' smart constructor.
data StartSigningJobParameter = StartSigningJobParameter'
  { _ssjpDestination ::
      !(Maybe Destination),
    _ssjpSigningProfileName :: !(Maybe Text),
    _ssjpSigningProfileParameter ::
      !(Maybe SigningProfileParameter)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartSigningJobParameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssjpDestination' - The location to write the code-signed file.
--
-- * 'ssjpSigningProfileName' - The code-signing profile name.
--
-- * 'ssjpSigningProfileParameter' - Describes the code-signing profile.
startSigningJobParameter ::
  StartSigningJobParameter
startSigningJobParameter =
  StartSigningJobParameter'
    { _ssjpDestination = Nothing,
      _ssjpSigningProfileName = Nothing,
      _ssjpSigningProfileParameter = Nothing
    }

-- | The location to write the code-signed file.
ssjpDestination :: Lens' StartSigningJobParameter (Maybe Destination)
ssjpDestination = lens _ssjpDestination (\s a -> s {_ssjpDestination = a})

-- | The code-signing profile name.
ssjpSigningProfileName :: Lens' StartSigningJobParameter (Maybe Text)
ssjpSigningProfileName = lens _ssjpSigningProfileName (\s a -> s {_ssjpSigningProfileName = a})

-- | Describes the code-signing profile.
ssjpSigningProfileParameter :: Lens' StartSigningJobParameter (Maybe SigningProfileParameter)
ssjpSigningProfileParameter = lens _ssjpSigningProfileParameter (\s a -> s {_ssjpSigningProfileParameter = a})

instance FromJSON StartSigningJobParameter where
  parseJSON =
    withObject
      "StartSigningJobParameter"
      ( \x ->
          StartSigningJobParameter'
            <$> (x .:? "destination")
            <*> (x .:? "signingProfileName")
            <*> (x .:? "signingProfileParameter")
      )

instance Hashable StartSigningJobParameter

instance NFData StartSigningJobParameter

instance ToJSON StartSigningJobParameter where
  toJSON StartSigningJobParameter' {..} =
    object
      ( catMaybes
          [ ("destination" .=) <$> _ssjpDestination,
            ("signingProfileName" .=) <$> _ssjpSigningProfileName,
            ("signingProfileParameter" .=) <$> _ssjpSigningProfileParameter
          ]
      )
