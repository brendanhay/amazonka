{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ShrinkPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ShrinkPolicy where

import Network.AWS.EMR.Types.InstanceResizePolicy
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Policy for customizing shrink operations. Allows configuration of decommissioning timeout and targeted instance shrinking.
--
--
--
-- /See:/ 'shrinkPolicy' smart constructor.
data ShrinkPolicy = ShrinkPolicy'
  { _spDecommissionTimeout ::
      !(Maybe Int),
    _spInstanceResizePolicy :: !(Maybe InstanceResizePolicy)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ShrinkPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spDecommissionTimeout' - The desired timeout for decommissioning an instance. Overrides the default YARN decommissioning timeout.
--
-- * 'spInstanceResizePolicy' - Custom policy for requesting termination protection or termination of specific instances when shrinking an instance group.
shrinkPolicy ::
  ShrinkPolicy
shrinkPolicy =
  ShrinkPolicy'
    { _spDecommissionTimeout = Nothing,
      _spInstanceResizePolicy = Nothing
    }

-- | The desired timeout for decommissioning an instance. Overrides the default YARN decommissioning timeout.
spDecommissionTimeout :: Lens' ShrinkPolicy (Maybe Int)
spDecommissionTimeout = lens _spDecommissionTimeout (\s a -> s {_spDecommissionTimeout = a})

-- | Custom policy for requesting termination protection or termination of specific instances when shrinking an instance group.
spInstanceResizePolicy :: Lens' ShrinkPolicy (Maybe InstanceResizePolicy)
spInstanceResizePolicy = lens _spInstanceResizePolicy (\s a -> s {_spInstanceResizePolicy = a})

instance FromJSON ShrinkPolicy where
  parseJSON =
    withObject
      "ShrinkPolicy"
      ( \x ->
          ShrinkPolicy'
            <$> (x .:? "DecommissionTimeout") <*> (x .:? "InstanceResizePolicy")
      )

instance Hashable ShrinkPolicy

instance NFData ShrinkPolicy

instance ToJSON ShrinkPolicy where
  toJSON ShrinkPolicy' {..} =
    object
      ( catMaybes
          [ ("DecommissionTimeout" .=) <$> _spDecommissionTimeout,
            ("InstanceResizePolicy" .=) <$> _spInstanceResizePolicy
          ]
      )
