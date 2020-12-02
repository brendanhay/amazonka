{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.Processor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.Processor where

import Network.AWS.Firehose.Types.ProcessorParameter
import Network.AWS.Firehose.Types.ProcessorType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a data processor.
--
--
--
-- /See:/ 'processor' smart constructor.
data Processor = Processor'
  { _pParameters ::
      !(Maybe [ProcessorParameter]),
    _pType :: !ProcessorType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Processor' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pParameters' - The processor parameters.
--
-- * 'pType' - The type of processor.
processor ::
  -- | 'pType'
  ProcessorType ->
  Processor
processor pType_ =
  Processor' {_pParameters = Nothing, _pType = pType_}

-- | The processor parameters.
pParameters :: Lens' Processor [ProcessorParameter]
pParameters = lens _pParameters (\s a -> s {_pParameters = a}) . _Default . _Coerce

-- | The type of processor.
pType :: Lens' Processor ProcessorType
pType = lens _pType (\s a -> s {_pType = a})

instance FromJSON Processor where
  parseJSON =
    withObject
      "Processor"
      ( \x ->
          Processor' <$> (x .:? "Parameters" .!= mempty) <*> (x .: "Type")
      )

instance Hashable Processor

instance NFData Processor

instance ToJSON Processor where
  toJSON Processor' {..} =
    object
      ( catMaybes
          [("Parameters" .=) <$> _pParameters, Just ("Type" .= _pType)]
      )
