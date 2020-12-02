{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InstanceInformationStringFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InstanceInformationStringFilter where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The filters to describe or get information about your managed instances.
--
--
--
-- /See:/ 'instanceInformationStringFilter' smart constructor.
data InstanceInformationStringFilter = InstanceInformationStringFilter'
  { _iisfKey ::
      !Text,
    _iisfValues ::
      !(List1 Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceInformationStringFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iisfKey' - The filter key name to describe your instances. For example: "InstanceIds"|"AgentVersion"|"PingStatus"|"PlatformTypes"|"ActivationIds"|"IamRole"|"ResourceType"|"AssociationStatus"|"Tag Key"
--
-- * 'iisfValues' - The filter values.
instanceInformationStringFilter ::
  -- | 'iisfKey'
  Text ->
  -- | 'iisfValues'
  NonEmpty Text ->
  InstanceInformationStringFilter
instanceInformationStringFilter pKey_ pValues_ =
  InstanceInformationStringFilter'
    { _iisfKey = pKey_,
      _iisfValues = _List1 # pValues_
    }

-- | The filter key name to describe your instances. For example: "InstanceIds"|"AgentVersion"|"PingStatus"|"PlatformTypes"|"ActivationIds"|"IamRole"|"ResourceType"|"AssociationStatus"|"Tag Key"
iisfKey :: Lens' InstanceInformationStringFilter Text
iisfKey = lens _iisfKey (\s a -> s {_iisfKey = a})

-- | The filter values.
iisfValues :: Lens' InstanceInformationStringFilter (NonEmpty Text)
iisfValues = lens _iisfValues (\s a -> s {_iisfValues = a}) . _List1

instance Hashable InstanceInformationStringFilter

instance NFData InstanceInformationStringFilter

instance ToJSON InstanceInformationStringFilter where
  toJSON InstanceInformationStringFilter' {..} =
    object
      ( catMaybes
          [Just ("Key" .= _iisfKey), Just ("Values" .= _iisfValues)]
      )
