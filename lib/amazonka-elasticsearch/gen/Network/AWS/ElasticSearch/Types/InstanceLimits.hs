{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.InstanceLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.InstanceLimits where

import Network.AWS.ElasticSearch.Types.InstanceCountLimits
import Network.AWS.Lens
import Network.AWS.Prelude

-- | InstanceLimits represents the list of instance related attributes that are available for given InstanceType.
--
--
--
-- /See:/ 'instanceLimits' smart constructor.
newtype InstanceLimits = InstanceLimits'
  { _ilInstanceCountLimits ::
      Maybe InstanceCountLimits
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceLimits' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ilInstanceCountLimits' - Undocumented member.
instanceLimits ::
  InstanceLimits
instanceLimits = InstanceLimits' {_ilInstanceCountLimits = Nothing}

-- | Undocumented member.
ilInstanceCountLimits :: Lens' InstanceLimits (Maybe InstanceCountLimits)
ilInstanceCountLimits = lens _ilInstanceCountLimits (\s a -> s {_ilInstanceCountLimits = a})

instance FromJSON InstanceLimits where
  parseJSON =
    withObject
      "InstanceLimits"
      (\x -> InstanceLimits' <$> (x .:? "InstanceCountLimits"))

instance Hashable InstanceLimits

instance NFData InstanceLimits
