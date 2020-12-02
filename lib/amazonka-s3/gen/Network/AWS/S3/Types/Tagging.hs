{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Tagging
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Tagging where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Tag

-- | Container for @TagSet@ elements.
--
--
--
-- /See:/ 'tagging' smart constructor.
newtype Tagging = Tagging' {_tTagSet :: [Tag]}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Tagging' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tTagSet' - A collection for a set of tags
tagging ::
  Tagging
tagging = Tagging' {_tTagSet = mempty}

-- | A collection for a set of tags
tTagSet :: Lens' Tagging [Tag]
tTagSet = lens _tTagSet (\s a -> s {_tTagSet = a}) . _Coerce

instance Hashable Tagging

instance NFData Tagging

instance ToXML Tagging where
  toXML Tagging' {..} = mconcat ["TagSet" @= toXMLList "Tag" _tTagSet]
