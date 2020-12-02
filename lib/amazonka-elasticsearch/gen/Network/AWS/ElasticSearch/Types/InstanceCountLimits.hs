{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.InstanceCountLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.InstanceCountLimits where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | InstanceCountLimits represents the limits on number of instances that be created in Amazon Elasticsearch for given InstanceType.
--
--
--
-- /See:/ 'instanceCountLimits' smart constructor.
data InstanceCountLimits = InstanceCountLimits'
  { _iclMaximumInstanceCount ::
      !(Maybe Int),
    _iclMinimumInstanceCount :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceCountLimits' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iclMaximumInstanceCount' - Undocumented member.
--
-- * 'iclMinimumInstanceCount' - Undocumented member.
instanceCountLimits ::
  InstanceCountLimits
instanceCountLimits =
  InstanceCountLimits'
    { _iclMaximumInstanceCount = Nothing,
      _iclMinimumInstanceCount = Nothing
    }

-- | Undocumented member.
iclMaximumInstanceCount :: Lens' InstanceCountLimits (Maybe Int)
iclMaximumInstanceCount = lens _iclMaximumInstanceCount (\s a -> s {_iclMaximumInstanceCount = a})

-- | Undocumented member.
iclMinimumInstanceCount :: Lens' InstanceCountLimits (Maybe Int)
iclMinimumInstanceCount = lens _iclMinimumInstanceCount (\s a -> s {_iclMinimumInstanceCount = a})

instance FromJSON InstanceCountLimits where
  parseJSON =
    withObject
      "InstanceCountLimits"
      ( \x ->
          InstanceCountLimits'
            <$> (x .:? "MaximumInstanceCount") <*> (x .:? "MinimumInstanceCount")
      )

instance Hashable InstanceCountLimits

instance NFData InstanceCountLimits
