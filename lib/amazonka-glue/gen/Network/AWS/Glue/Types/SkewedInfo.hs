{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.SkewedInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.SkewedInfo where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies skewed values in a table. Skewed values are those that occur with very high frequency.
--
--
--
-- /See:/ 'skewedInfo' smart constructor.
data SkewedInfo = SkewedInfo'
  { _siSkewedColumnValueLocationMaps ::
      !(Maybe (Map Text (Text))),
    _siSkewedColumnValues :: !(Maybe [Text]),
    _siSkewedColumnNames :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SkewedInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siSkewedColumnValueLocationMaps' - A mapping of skewed values to the columns that contain them.
--
-- * 'siSkewedColumnValues' - A list of values that appear so frequently as to be considered skewed.
--
-- * 'siSkewedColumnNames' - A list of names of columns that contain skewed values.
skewedInfo ::
  SkewedInfo
skewedInfo =
  SkewedInfo'
    { _siSkewedColumnValueLocationMaps = Nothing,
      _siSkewedColumnValues = Nothing,
      _siSkewedColumnNames = Nothing
    }

-- | A mapping of skewed values to the columns that contain them.
siSkewedColumnValueLocationMaps :: Lens' SkewedInfo (HashMap Text (Text))
siSkewedColumnValueLocationMaps = lens _siSkewedColumnValueLocationMaps (\s a -> s {_siSkewedColumnValueLocationMaps = a}) . _Default . _Map

-- | A list of values that appear so frequently as to be considered skewed.
siSkewedColumnValues :: Lens' SkewedInfo [Text]
siSkewedColumnValues = lens _siSkewedColumnValues (\s a -> s {_siSkewedColumnValues = a}) . _Default . _Coerce

-- | A list of names of columns that contain skewed values.
siSkewedColumnNames :: Lens' SkewedInfo [Text]
siSkewedColumnNames = lens _siSkewedColumnNames (\s a -> s {_siSkewedColumnNames = a}) . _Default . _Coerce

instance FromJSON SkewedInfo where
  parseJSON =
    withObject
      "SkewedInfo"
      ( \x ->
          SkewedInfo'
            <$> (x .:? "SkewedColumnValueLocationMaps" .!= mempty)
            <*> (x .:? "SkewedColumnValues" .!= mempty)
            <*> (x .:? "SkewedColumnNames" .!= mempty)
      )

instance Hashable SkewedInfo

instance NFData SkewedInfo

instance ToJSON SkewedInfo where
  toJSON SkewedInfo' {..} =
    object
      ( catMaybes
          [ ("SkewedColumnValueLocationMaps" .=)
              <$> _siSkewedColumnValueLocationMaps,
            ("SkewedColumnValues" .=) <$> _siSkewedColumnValues,
            ("SkewedColumnNames" .=) <$> _siSkewedColumnNames
          ]
      )
