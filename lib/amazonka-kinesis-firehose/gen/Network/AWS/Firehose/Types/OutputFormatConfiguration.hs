{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.OutputFormatConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.OutputFormatConfiguration where

import Network.AWS.Firehose.Types.Serializer
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the serializer that you want Kinesis Data Firehose to use to convert the format of your data before it writes it to Amazon S3. This parameter is required if @Enabled@ is set to true.
--
--
--
-- /See:/ 'outputFormatConfiguration' smart constructor.
newtype OutputFormatConfiguration = OutputFormatConfiguration'
  { _ofcSerializer ::
      Maybe Serializer
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OutputFormatConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ofcSerializer' - Specifies which serializer to use. You can choose either the ORC SerDe or the Parquet SerDe. If both are non-null, the server rejects the request.
outputFormatConfiguration ::
  OutputFormatConfiguration
outputFormatConfiguration =
  OutputFormatConfiguration' {_ofcSerializer = Nothing}

-- | Specifies which serializer to use. You can choose either the ORC SerDe or the Parquet SerDe. If both are non-null, the server rejects the request.
ofcSerializer :: Lens' OutputFormatConfiguration (Maybe Serializer)
ofcSerializer = lens _ofcSerializer (\s a -> s {_ofcSerializer = a})

instance FromJSON OutputFormatConfiguration where
  parseJSON =
    withObject
      "OutputFormatConfiguration"
      (\x -> OutputFormatConfiguration' <$> (x .:? "Serializer"))

instance Hashable OutputFormatConfiguration

instance NFData OutputFormatConfiguration

instance ToJSON OutputFormatConfiguration where
  toJSON OutputFormatConfiguration' {..} =
    object (catMaybes [("Serializer" .=) <$> _ofcSerializer])
