{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceResizePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceResizePolicy where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Custom policy for requesting termination protection or termination of specific instances when shrinking an instance group.
--
--
--
-- /See:/ 'instanceResizePolicy' smart constructor.
data InstanceResizePolicy = InstanceResizePolicy'
  { _irpInstancesToProtect ::
      !(Maybe [Text]),
    _irpInstancesToTerminate :: !(Maybe [Text]),
    _irpInstanceTerminationTimeout :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceResizePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'irpInstancesToProtect' - Specific list of instances to be protected when shrinking an instance group.
--
-- * 'irpInstancesToTerminate' - Specific list of instances to be terminated when shrinking an instance group.
--
-- * 'irpInstanceTerminationTimeout' - Decommissioning timeout override for the specific list of instances to be terminated.
instanceResizePolicy ::
  InstanceResizePolicy
instanceResizePolicy =
  InstanceResizePolicy'
    { _irpInstancesToProtect = Nothing,
      _irpInstancesToTerminate = Nothing,
      _irpInstanceTerminationTimeout = Nothing
    }

-- | Specific list of instances to be protected when shrinking an instance group.
irpInstancesToProtect :: Lens' InstanceResizePolicy [Text]
irpInstancesToProtect = lens _irpInstancesToProtect (\s a -> s {_irpInstancesToProtect = a}) . _Default . _Coerce

-- | Specific list of instances to be terminated when shrinking an instance group.
irpInstancesToTerminate :: Lens' InstanceResizePolicy [Text]
irpInstancesToTerminate = lens _irpInstancesToTerminate (\s a -> s {_irpInstancesToTerminate = a}) . _Default . _Coerce

-- | Decommissioning timeout override for the specific list of instances to be terminated.
irpInstanceTerminationTimeout :: Lens' InstanceResizePolicy (Maybe Int)
irpInstanceTerminationTimeout = lens _irpInstanceTerminationTimeout (\s a -> s {_irpInstanceTerminationTimeout = a})

instance FromJSON InstanceResizePolicy where
  parseJSON =
    withObject
      "InstanceResizePolicy"
      ( \x ->
          InstanceResizePolicy'
            <$> (x .:? "InstancesToProtect" .!= mempty)
            <*> (x .:? "InstancesToTerminate" .!= mempty)
            <*> (x .:? "InstanceTerminationTimeout")
      )

instance Hashable InstanceResizePolicy

instance NFData InstanceResizePolicy

instance ToJSON InstanceResizePolicy where
  toJSON InstanceResizePolicy' {..} =
    object
      ( catMaybes
          [ ("InstancesToProtect" .=) <$> _irpInstancesToProtect,
            ("InstancesToTerminate" .=) <$> _irpInstancesToTerminate,
            ("InstanceTerminationTimeout" .=)
              <$> _irpInstanceTerminationTimeout
          ]
      )
