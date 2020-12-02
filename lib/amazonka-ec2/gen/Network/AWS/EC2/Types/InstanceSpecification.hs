{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceSpecification where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The instance details to specify which volumes should be snapshotted.
--
--
--
-- /See:/ 'instanceSpecification' smart constructor.
data InstanceSpecification = InstanceSpecification'
  { _isInstanceId ::
      !(Maybe Text),
    _isExcludeBootVolume :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isInstanceId' - The instance to specify which volumes should be snapshotted.
--
-- * 'isExcludeBootVolume' - Excludes the root volume from being snapshotted.
instanceSpecification ::
  InstanceSpecification
instanceSpecification =
  InstanceSpecification'
    { _isInstanceId = Nothing,
      _isExcludeBootVolume = Nothing
    }

-- | The instance to specify which volumes should be snapshotted.
isInstanceId :: Lens' InstanceSpecification (Maybe Text)
isInstanceId = lens _isInstanceId (\s a -> s {_isInstanceId = a})

-- | Excludes the root volume from being snapshotted.
isExcludeBootVolume :: Lens' InstanceSpecification (Maybe Bool)
isExcludeBootVolume = lens _isExcludeBootVolume (\s a -> s {_isExcludeBootVolume = a})

instance Hashable InstanceSpecification

instance NFData InstanceSpecification

instance ToQuery InstanceSpecification where
  toQuery InstanceSpecification' {..} =
    mconcat
      [ "InstanceId" =: _isInstanceId,
        "ExcludeBootVolume" =: _isExcludeBootVolume
      ]
