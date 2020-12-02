{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.InputFormatConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.InputFormatConfiguration where

import Network.AWS.Firehose.Types.Deserializer
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the deserializer you want to use to convert the format of the input data. This parameter is required if @Enabled@ is set to true.
--
--
--
-- /See:/ 'inputFormatConfiguration' smart constructor.
newtype InputFormatConfiguration = InputFormatConfiguration'
  { _ifcDeserializer ::
      Maybe Deserializer
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputFormatConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ifcDeserializer' - Specifies which deserializer to use. You can choose either the Apache Hive JSON SerDe or the OpenX JSON SerDe. If both are non-null, the server rejects the request.
inputFormatConfiguration ::
  InputFormatConfiguration
inputFormatConfiguration =
  InputFormatConfiguration' {_ifcDeserializer = Nothing}

-- | Specifies which deserializer to use. You can choose either the Apache Hive JSON SerDe or the OpenX JSON SerDe. If both are non-null, the server rejects the request.
ifcDeserializer :: Lens' InputFormatConfiguration (Maybe Deserializer)
ifcDeserializer = lens _ifcDeserializer (\s a -> s {_ifcDeserializer = a})

instance FromJSON InputFormatConfiguration where
  parseJSON =
    withObject
      "InputFormatConfiguration"
      (\x -> InputFormatConfiguration' <$> (x .:? "Deserializer"))

instance Hashable InputFormatConfiguration

instance NFData InputFormatConfiguration

instance ToJSON InputFormatConfiguration where
  toJSON InputFormatConfiguration' {..} =
    object (catMaybes [("Deserializer" .=) <$> _ifcDeserializer])
