{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.ProcessingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.ProcessingConfiguration where

import Network.AWS.Firehose.Types.Processor
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a data processing configuration.
--
--
--
-- /See:/ 'processingConfiguration' smart constructor.
data ProcessingConfiguration = ProcessingConfiguration'
  { _pcEnabled ::
      !(Maybe Bool),
    _pcProcessors :: !(Maybe [Processor])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProcessingConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcEnabled' - Enables or disables data processing.
--
-- * 'pcProcessors' - The data processors.
processingConfiguration ::
  ProcessingConfiguration
processingConfiguration =
  ProcessingConfiguration'
    { _pcEnabled = Nothing,
      _pcProcessors = Nothing
    }

-- | Enables or disables data processing.
pcEnabled :: Lens' ProcessingConfiguration (Maybe Bool)
pcEnabled = lens _pcEnabled (\s a -> s {_pcEnabled = a})

-- | The data processors.
pcProcessors :: Lens' ProcessingConfiguration [Processor]
pcProcessors = lens _pcProcessors (\s a -> s {_pcProcessors = a}) . _Default . _Coerce

instance FromJSON ProcessingConfiguration where
  parseJSON =
    withObject
      "ProcessingConfiguration"
      ( \x ->
          ProcessingConfiguration'
            <$> (x .:? "Enabled") <*> (x .:? "Processors" .!= mempty)
      )

instance Hashable ProcessingConfiguration

instance NFData ProcessingConfiguration

instance ToJSON ProcessingConfiguration where
  toJSON ProcessingConfiguration' {..} =
    object
      ( catMaybes
          [ ("Enabled" .=) <$> _pcEnabled,
            ("Processors" .=) <$> _pcProcessors
          ]
      )
