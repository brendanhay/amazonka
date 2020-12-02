{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.TagFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.TagFilter where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Used to filter the workflow executions in visibility APIs based on a tag.
--
--
--
-- /See:/ 'tagFilter' smart constructor.
newtype TagFilter = TagFilter' {_tfTag :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TagFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tfTag' - Specifies the tag that must be associated with the execution for it to meet the filter criteria. Tags may only contain unicode letters, digits, whitespace, or these symbols: @_ . : / = + - @@ .
tagFilter ::
  -- | 'tfTag'
  Text ->
  TagFilter
tagFilter pTag_ = TagFilter' {_tfTag = pTag_}

-- | Specifies the tag that must be associated with the execution for it to meet the filter criteria. Tags may only contain unicode letters, digits, whitespace, or these symbols: @_ . : / = + - @@ .
tfTag :: Lens' TagFilter Text
tfTag = lens _tfTag (\s a -> s {_tfTag = a})

instance Hashable TagFilter

instance NFData TagFilter

instance ToJSON TagFilter where
  toJSON TagFilter' {..} = object (catMaybes [Just ("tag" .= _tfTag)])
