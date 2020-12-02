{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.DataRetrievalRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.DataRetrievalRule where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Data retrieval policy rule.
--
--
--
-- /See:/ 'dataRetrievalRule' smart constructor.
data DataRetrievalRule = DataRetrievalRule'
  { _drrStrategy ::
      !(Maybe Text),
    _drrBytesPerHour :: !(Maybe Integer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DataRetrievalRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drrStrategy' - The type of data retrieval policy to set. Valid values: BytesPerHour|FreeTier|None
--
-- * 'drrBytesPerHour' - The maximum number of bytes that can be retrieved in an hour. This field is required only if the value of the Strategy field is @BytesPerHour@ . Your PUT operation will be rejected if the Strategy field is not set to @BytesPerHour@ and you set this field.
dataRetrievalRule ::
  DataRetrievalRule
dataRetrievalRule =
  DataRetrievalRule'
    { _drrStrategy = Nothing,
      _drrBytesPerHour = Nothing
    }

-- | The type of data retrieval policy to set. Valid values: BytesPerHour|FreeTier|None
drrStrategy :: Lens' DataRetrievalRule (Maybe Text)
drrStrategy = lens _drrStrategy (\s a -> s {_drrStrategy = a})

-- | The maximum number of bytes that can be retrieved in an hour. This field is required only if the value of the Strategy field is @BytesPerHour@ . Your PUT operation will be rejected if the Strategy field is not set to @BytesPerHour@ and you set this field.
drrBytesPerHour :: Lens' DataRetrievalRule (Maybe Integer)
drrBytesPerHour = lens _drrBytesPerHour (\s a -> s {_drrBytesPerHour = a})

instance FromJSON DataRetrievalRule where
  parseJSON =
    withObject
      "DataRetrievalRule"
      ( \x ->
          DataRetrievalRule'
            <$> (x .:? "Strategy") <*> (x .:? "BytesPerHour")
      )

instance Hashable DataRetrievalRule

instance NFData DataRetrievalRule

instance ToJSON DataRetrievalRule where
  toJSON DataRetrievalRule' {..} =
    object
      ( catMaybes
          [ ("Strategy" .=) <$> _drrStrategy,
            ("BytesPerHour" .=) <$> _drrBytesPerHour
          ]
      )
