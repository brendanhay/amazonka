{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.ProcessorParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.ProcessorParameter where

import Network.AWS.Firehose.Types.ProcessorParameterName
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the processor parameter.
--
--
--
-- /See:/ 'processorParameter' smart constructor.
data ProcessorParameter = ProcessorParameter'
  { _ppParameterName ::
      !ProcessorParameterName,
    _ppParameterValue :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProcessorParameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppParameterName' - The name of the parameter.
--
-- * 'ppParameterValue' - The parameter value.
processorParameter ::
  -- | 'ppParameterName'
  ProcessorParameterName ->
  -- | 'ppParameterValue'
  Text ->
  ProcessorParameter
processorParameter pParameterName_ pParameterValue_ =
  ProcessorParameter'
    { _ppParameterName = pParameterName_,
      _ppParameterValue = pParameterValue_
    }

-- | The name of the parameter.
ppParameterName :: Lens' ProcessorParameter ProcessorParameterName
ppParameterName = lens _ppParameterName (\s a -> s {_ppParameterName = a})

-- | The parameter value.
ppParameterValue :: Lens' ProcessorParameter Text
ppParameterValue = lens _ppParameterValue (\s a -> s {_ppParameterValue = a})

instance FromJSON ProcessorParameter where
  parseJSON =
    withObject
      "ProcessorParameter"
      ( \x ->
          ProcessorParameter'
            <$> (x .: "ParameterName") <*> (x .: "ParameterValue")
      )

instance Hashable ProcessorParameter

instance NFData ProcessorParameter

instance ToJSON ProcessorParameter where
  toJSON ProcessorParameter' {..} =
    object
      ( catMaybes
          [ Just ("ParameterName" .= _ppParameterName),
            Just ("ParameterValue" .= _ppParameterValue)
          ]
      )
