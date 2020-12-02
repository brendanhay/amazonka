{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.Replica
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.Replica where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the properties of a replica.
--
--
--
-- /See:/ 'replica' smart constructor.
newtype Replica = Replica' {_rRegionName :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Replica' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rRegionName' - The Region where the replica needs to be created.
replica ::
  Replica
replica = Replica' {_rRegionName = Nothing}

-- | The Region where the replica needs to be created.
rRegionName :: Lens' Replica (Maybe Text)
rRegionName = lens _rRegionName (\s a -> s {_rRegionName = a})

instance FromJSON Replica where
  parseJSON =
    withObject "Replica" (\x -> Replica' <$> (x .:? "RegionName"))

instance Hashable Replica

instance NFData Replica

instance ToJSON Replica where
  toJSON Replica' {..} =
    object (catMaybes [("RegionName" .=) <$> _rRegionName])
