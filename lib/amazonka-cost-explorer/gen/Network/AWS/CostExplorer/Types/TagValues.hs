{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.TagValues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.TagValues where

import Network.AWS.CostExplorer.Types.MatchOption
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The values that are available for a tag.
--
--
--
-- /See:/ 'tagValues' smart constructor.
data TagValues = TagValues'
  { _tvValues :: !(Maybe [Text]),
    _tvKey :: !(Maybe Text),
    _tvMatchOptions :: !(Maybe [MatchOption])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TagValues' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tvValues' - The specific value of the tag.
--
-- * 'tvKey' - The key for the tag.
--
-- * 'tvMatchOptions' - The match options that you can use to filter your results. @MatchOptions@ is only applicable for actions related to Cost Category. The default values for @MatchOptions@ are @EQUALS@ and @CASE_SENSITIVE@ .
tagValues ::
  TagValues
tagValues =
  TagValues'
    { _tvValues = Nothing,
      _tvKey = Nothing,
      _tvMatchOptions = Nothing
    }

-- | The specific value of the tag.
tvValues :: Lens' TagValues [Text]
tvValues = lens _tvValues (\s a -> s {_tvValues = a}) . _Default . _Coerce

-- | The key for the tag.
tvKey :: Lens' TagValues (Maybe Text)
tvKey = lens _tvKey (\s a -> s {_tvKey = a})

-- | The match options that you can use to filter your results. @MatchOptions@ is only applicable for actions related to Cost Category. The default values for @MatchOptions@ are @EQUALS@ and @CASE_SENSITIVE@ .
tvMatchOptions :: Lens' TagValues [MatchOption]
tvMatchOptions = lens _tvMatchOptions (\s a -> s {_tvMatchOptions = a}) . _Default . _Coerce

instance FromJSON TagValues where
  parseJSON =
    withObject
      "TagValues"
      ( \x ->
          TagValues'
            <$> (x .:? "Values" .!= mempty)
            <*> (x .:? "Key")
            <*> (x .:? "MatchOptions" .!= mempty)
      )

instance Hashable TagValues

instance NFData TagValues

instance ToJSON TagValues where
  toJSON TagValues' {..} =
    object
      ( catMaybes
          [ ("Values" .=) <$> _tvValues,
            ("Key" .=) <$> _tvKey,
            ("MatchOptions" .=) <$> _tvMatchOptions
          ]
      )
