{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.OutputSerialization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.OutputSerialization where

import Network.AWS.Glacier.Types.CSVOutput
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes how the select output is serialized.
--
--
--
-- /See:/ 'outputSerialization' smart constructor.
newtype OutputSerialization = OutputSerialization'
  { _osCsv ::
      Maybe CSVOutput
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OutputSerialization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'osCsv' - Describes the serialization of CSV-encoded query results.
outputSerialization ::
  OutputSerialization
outputSerialization = OutputSerialization' {_osCsv = Nothing}

-- | Describes the serialization of CSV-encoded query results.
osCsv :: Lens' OutputSerialization (Maybe CSVOutput)
osCsv = lens _osCsv (\s a -> s {_osCsv = a})

instance FromJSON OutputSerialization where
  parseJSON =
    withObject
      "OutputSerialization"
      (\x -> OutputSerialization' <$> (x .:? "csv"))

instance Hashable OutputSerialization

instance NFData OutputSerialization

instance ToJSON OutputSerialization where
  toJSON OutputSerialization' {..} =
    object (catMaybes [("csv" .=) <$> _osCsv])
