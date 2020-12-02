{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.TagFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.TagFilter where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The tag filter. Valid names are: @tagKey@ , @tagValue@ , @configurationId@ .
--
--
--
-- /See:/ 'tagFilter' smart constructor.
data TagFilter = TagFilter' {_tfName :: !Text, _tfValues :: ![Text]}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TagFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tfName' - A name of the tag filter.
--
-- * 'tfValues' - Values for the tag filter.
tagFilter ::
  -- | 'tfName'
  Text ->
  TagFilter
tagFilter pName_ = TagFilter' {_tfName = pName_, _tfValues = mempty}

-- | A name of the tag filter.
tfName :: Lens' TagFilter Text
tfName = lens _tfName (\s a -> s {_tfName = a})

-- | Values for the tag filter.
tfValues :: Lens' TagFilter [Text]
tfValues = lens _tfValues (\s a -> s {_tfValues = a}) . _Coerce

instance Hashable TagFilter

instance NFData TagFilter

instance ToJSON TagFilter where
  toJSON TagFilter' {..} =
    object
      ( catMaybes
          [Just ("name" .= _tfName), Just ("values" .= _tfValues)]
      )
