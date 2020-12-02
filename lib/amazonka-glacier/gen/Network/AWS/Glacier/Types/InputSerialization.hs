{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.InputSerialization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.InputSerialization where

import Network.AWS.Glacier.Types.CSVInput
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes how the archive is serialized.
--
--
--
-- /See:/ 'inputSerialization' smart constructor.
newtype InputSerialization = InputSerialization'
  { _isCsv ::
      Maybe CSVInput
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputSerialization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isCsv' - Describes the serialization of a CSV-encoded object.
inputSerialization ::
  InputSerialization
inputSerialization = InputSerialization' {_isCsv = Nothing}

-- | Describes the serialization of a CSV-encoded object.
isCsv :: Lens' InputSerialization (Maybe CSVInput)
isCsv = lens _isCsv (\s a -> s {_isCsv = a})

instance FromJSON InputSerialization where
  parseJSON =
    withObject
      "InputSerialization"
      (\x -> InputSerialization' <$> (x .:? "csv"))

instance Hashable InputSerialization

instance NFData InputSerialization

instance ToJSON InputSerialization where
  toJSON InputSerialization' {..} =
    object (catMaybes [("csv" .=) <$> _isCsv])
