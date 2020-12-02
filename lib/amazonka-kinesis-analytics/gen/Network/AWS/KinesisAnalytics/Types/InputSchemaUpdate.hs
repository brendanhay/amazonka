{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.InputSchemaUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.InputSchemaUpdate where

import Network.AWS.KinesisAnalytics.Types.RecordColumn
import Network.AWS.KinesisAnalytics.Types.RecordFormat
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes updates for the application's input schema.
--
--
--
-- /See:/ 'inputSchemaUpdate' smart constructor.
data InputSchemaUpdate = InputSchemaUpdate'
  { _isuRecordFormatUpdate ::
      !(Maybe RecordFormat),
    _isuRecordEncodingUpdate :: !(Maybe Text),
    _isuRecordColumnUpdates ::
      !(Maybe (List1 RecordColumn))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputSchemaUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isuRecordFormatUpdate' - Specifies the format of the records on the streaming source.
--
-- * 'isuRecordEncodingUpdate' - Specifies the encoding of the records in the streaming source. For example, UTF-8.
--
-- * 'isuRecordColumnUpdates' - A list of @RecordColumn@ objects. Each object describes the mapping of the streaming source element to the corresponding column in the in-application stream.
inputSchemaUpdate ::
  InputSchemaUpdate
inputSchemaUpdate =
  InputSchemaUpdate'
    { _isuRecordFormatUpdate = Nothing,
      _isuRecordEncodingUpdate = Nothing,
      _isuRecordColumnUpdates = Nothing
    }

-- | Specifies the format of the records on the streaming source.
isuRecordFormatUpdate :: Lens' InputSchemaUpdate (Maybe RecordFormat)
isuRecordFormatUpdate = lens _isuRecordFormatUpdate (\s a -> s {_isuRecordFormatUpdate = a})

-- | Specifies the encoding of the records in the streaming source. For example, UTF-8.
isuRecordEncodingUpdate :: Lens' InputSchemaUpdate (Maybe Text)
isuRecordEncodingUpdate = lens _isuRecordEncodingUpdate (\s a -> s {_isuRecordEncodingUpdate = a})

-- | A list of @RecordColumn@ objects. Each object describes the mapping of the streaming source element to the corresponding column in the in-application stream.
isuRecordColumnUpdates :: Lens' InputSchemaUpdate (Maybe (NonEmpty RecordColumn))
isuRecordColumnUpdates = lens _isuRecordColumnUpdates (\s a -> s {_isuRecordColumnUpdates = a}) . mapping _List1

instance Hashable InputSchemaUpdate

instance NFData InputSchemaUpdate

instance ToJSON InputSchemaUpdate where
  toJSON InputSchemaUpdate' {..} =
    object
      ( catMaybes
          [ ("RecordFormatUpdate" .=) <$> _isuRecordFormatUpdate,
            ("RecordEncodingUpdate" .=) <$> _isuRecordEncodingUpdate,
            ("RecordColumnUpdates" .=) <$> _isuRecordColumnUpdates
          ]
      )
