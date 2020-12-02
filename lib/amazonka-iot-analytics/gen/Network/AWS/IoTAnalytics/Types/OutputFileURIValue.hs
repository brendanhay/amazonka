{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.OutputFileURIValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.OutputFileURIValue where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The value of the variable as a structure that specifies an output file URI.
--
--
--
-- /See:/ 'outputFileURIValue' smart constructor.
newtype OutputFileURIValue = OutputFileURIValue'
  { _ofuvFileName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OutputFileURIValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ofuvFileName' - The URI of the location where dataset contents are stored, usually the URI of a file in an S3 bucket.
outputFileURIValue ::
  -- | 'ofuvFileName'
  Text ->
  OutputFileURIValue
outputFileURIValue pFileName_ =
  OutputFileURIValue' {_ofuvFileName = pFileName_}

-- | The URI of the location where dataset contents are stored, usually the URI of a file in an S3 bucket.
ofuvFileName :: Lens' OutputFileURIValue Text
ofuvFileName = lens _ofuvFileName (\s a -> s {_ofuvFileName = a})

instance FromJSON OutputFileURIValue where
  parseJSON =
    withObject
      "OutputFileURIValue"
      (\x -> OutputFileURIValue' <$> (x .: "fileName"))

instance Hashable OutputFileURIValue

instance NFData OutputFileURIValue

instance ToJSON OutputFileURIValue where
  toJSON OutputFileURIValue' {..} =
    object (catMaybes [Just ("fileName" .= _ofuvFileName)])
