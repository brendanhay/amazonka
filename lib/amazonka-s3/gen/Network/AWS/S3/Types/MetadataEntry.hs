{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.MetadataEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.MetadataEntry where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal

-- | A metadata key-value pair to store with an object.
--
--
--
-- /See:/ 'metadataEntry' smart constructor.
data MetadataEntry = MetadataEntry'
  { _meValue :: !(Maybe Text),
    _meName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MetadataEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'meValue' - Value of the Object.
--
-- * 'meName' - Name of the Object.
metadataEntry ::
  MetadataEntry
metadataEntry =
  MetadataEntry' {_meValue = Nothing, _meName = Nothing}

-- | Value of the Object.
meValue :: Lens' MetadataEntry (Maybe Text)
meValue = lens _meValue (\s a -> s {_meValue = a})

-- | Name of the Object.
meName :: Lens' MetadataEntry (Maybe Text)
meName = lens _meName (\s a -> s {_meName = a})

instance Hashable MetadataEntry

instance NFData MetadataEntry

instance ToXML MetadataEntry where
  toXML MetadataEntry' {..} =
    mconcat ["Value" @= _meValue, "Name" @= _meName]
