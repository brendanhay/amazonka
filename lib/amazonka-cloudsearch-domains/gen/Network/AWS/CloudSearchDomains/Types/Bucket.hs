{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Types.Bucket
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearchDomains.Types.Bucket where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A container for facet information.
--
--
--
-- /See:/ 'bucket' smart constructor.
data Bucket = Bucket'
  { _bValue :: !(Maybe Text),
    _bCount :: !(Maybe Integer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Bucket' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bValue' - The facet value being counted.
--
-- * 'bCount' - The number of hits that contain the facet value in the specified facet field.
bucket ::
  Bucket
bucket = Bucket' {_bValue = Nothing, _bCount = Nothing}

-- | The facet value being counted.
bValue :: Lens' Bucket (Maybe Text)
bValue = lens _bValue (\s a -> s {_bValue = a})

-- | The number of hits that contain the facet value in the specified facet field.
bCount :: Lens' Bucket (Maybe Integer)
bCount = lens _bCount (\s a -> s {_bCount = a})

instance FromJSON Bucket where
  parseJSON =
    withObject
      "Bucket"
      (\x -> Bucket' <$> (x .:? "value") <*> (x .:? "count"))

instance Hashable Bucket

instance NFData Bucket
