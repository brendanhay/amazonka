{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.Variable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.Variable where

import Network.AWS.IoTAnalytics.Types.DatasetContentVersionValue
import Network.AWS.IoTAnalytics.Types.OutputFileURIValue
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An instance of a variable to be passed to the @containerAction@ execution. Each variable must have a name and a value given by one of @stringValue@ , @datasetContentVersionValue@ , or @outputFileUriValue@ .
--
--
--
-- /See:/ 'variable' smart constructor.
data Variable = Variable'
  { _vOutputFileURIValue ::
      !(Maybe OutputFileURIValue),
    _vDoubleValue :: !(Maybe Double),
    _vStringValue :: !(Maybe Text),
    _vDatasetContentVersionValue ::
      !(Maybe DatasetContentVersionValue),
    _vName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Variable' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vOutputFileURIValue' - The value of the variable as a structure that specifies an output file URI.
--
-- * 'vDoubleValue' - The value of the variable as a double (numeric).
--
-- * 'vStringValue' - The value of the variable as a string.
--
-- * 'vDatasetContentVersionValue' - The value of the variable as a structure that specifies a dataset content version.
--
-- * 'vName' - The name of the variable.
variable ::
  -- | 'vName'
  Text ->
  Variable
variable pName_ =
  Variable'
    { _vOutputFileURIValue = Nothing,
      _vDoubleValue = Nothing,
      _vStringValue = Nothing,
      _vDatasetContentVersionValue = Nothing,
      _vName = pName_
    }

-- | The value of the variable as a structure that specifies an output file URI.
vOutputFileURIValue :: Lens' Variable (Maybe OutputFileURIValue)
vOutputFileURIValue = lens _vOutputFileURIValue (\s a -> s {_vOutputFileURIValue = a})

-- | The value of the variable as a double (numeric).
vDoubleValue :: Lens' Variable (Maybe Double)
vDoubleValue = lens _vDoubleValue (\s a -> s {_vDoubleValue = a})

-- | The value of the variable as a string.
vStringValue :: Lens' Variable (Maybe Text)
vStringValue = lens _vStringValue (\s a -> s {_vStringValue = a})

-- | The value of the variable as a structure that specifies a dataset content version.
vDatasetContentVersionValue :: Lens' Variable (Maybe DatasetContentVersionValue)
vDatasetContentVersionValue = lens _vDatasetContentVersionValue (\s a -> s {_vDatasetContentVersionValue = a})

-- | The name of the variable.
vName :: Lens' Variable Text
vName = lens _vName (\s a -> s {_vName = a})

instance FromJSON Variable where
  parseJSON =
    withObject
      "Variable"
      ( \x ->
          Variable'
            <$> (x .:? "outputFileUriValue")
            <*> (x .:? "doubleValue")
            <*> (x .:? "stringValue")
            <*> (x .:? "datasetContentVersionValue")
            <*> (x .: "name")
      )

instance Hashable Variable

instance NFData Variable

instance ToJSON Variable where
  toJSON Variable' {..} =
    object
      ( catMaybes
          [ ("outputFileUriValue" .=) <$> _vOutputFileURIValue,
            ("doubleValue" .=) <$> _vDoubleValue,
            ("stringValue" .=) <$> _vStringValue,
            ("datasetContentVersionValue" .=) <$> _vDatasetContentVersionValue,
            Just ("name" .= _vName)
          ]
      )
