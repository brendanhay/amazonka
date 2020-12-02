{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceCount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceCount where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ListingState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a Reserved Instance listing state.
--
--
--
-- /See:/ 'instanceCount' smart constructor.
data InstanceCount = InstanceCount'
  { _icState ::
      !(Maybe ListingState),
    _icInstanceCount :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceCount' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icState' - The states of the listed Reserved Instances.
--
-- * 'icInstanceCount' - The number of listed Reserved Instances in the state specified by the @state@ .
instanceCount ::
  InstanceCount
instanceCount =
  InstanceCount' {_icState = Nothing, _icInstanceCount = Nothing}

-- | The states of the listed Reserved Instances.
icState :: Lens' InstanceCount (Maybe ListingState)
icState = lens _icState (\s a -> s {_icState = a})

-- | The number of listed Reserved Instances in the state specified by the @state@ .
icInstanceCount :: Lens' InstanceCount (Maybe Int)
icInstanceCount = lens _icInstanceCount (\s a -> s {_icInstanceCount = a})

instance FromXML InstanceCount where
  parseXML x =
    InstanceCount' <$> (x .@? "state") <*> (x .@? "instanceCount")

instance Hashable InstanceCount

instance NFData InstanceCount
