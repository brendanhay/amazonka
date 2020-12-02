{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.TagFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.TagFilter where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a tag.
--
--
--
-- /See:/ 'tagFilter' smart constructor.
data TagFilter = TagFilter'
  { _tfValues :: !(Maybe [Text]),
    _tfKey :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TagFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tfValues' - The tag values (0 to 20).
--
-- * 'tfKey' - The tag key.
tagFilter ::
  TagFilter
tagFilter = TagFilter' {_tfValues = Nothing, _tfKey = Nothing}

-- | The tag values (0 to 20).
tfValues :: Lens' TagFilter [Text]
tfValues = lens _tfValues (\s a -> s {_tfValues = a}) . _Default . _Coerce

-- | The tag key.
tfKey :: Lens' TagFilter (Maybe Text)
tfKey = lens _tfKey (\s a -> s {_tfKey = a})

instance FromJSON TagFilter where
  parseJSON =
    withObject
      "TagFilter"
      ( \x ->
          TagFilter' <$> (x .:? "Values" .!= mempty) <*> (x .:? "Key")
      )

instance Hashable TagFilter

instance NFData TagFilter

instance ToJSON TagFilter where
  toJSON TagFilter' {..} =
    object
      (catMaybes [("Values" .=) <$> _tfValues, ("Key" .=) <$> _tfKey])
