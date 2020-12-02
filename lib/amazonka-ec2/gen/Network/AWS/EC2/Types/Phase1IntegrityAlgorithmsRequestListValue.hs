{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Phase1IntegrityAlgorithmsRequestListValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Phase1IntegrityAlgorithmsRequestListValue where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the integrity algorithm for the VPN tunnel for phase 1 IKE negotiations.
--
--
--
-- /See:/ 'phase1IntegrityAlgorithmsRequestListValue' smart constructor.
newtype Phase1IntegrityAlgorithmsRequestListValue = Phase1IntegrityAlgorithmsRequestListValue'
  { _piarlviValue ::
      Maybe
        Text
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'Phase1IntegrityAlgorithmsRequestListValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'piarlviValue' - The value for the integrity algorithm.
phase1IntegrityAlgorithmsRequestListValue ::
  Phase1IntegrityAlgorithmsRequestListValue
phase1IntegrityAlgorithmsRequestListValue =
  Phase1IntegrityAlgorithmsRequestListValue'
    { _piarlviValue =
        Nothing
    }

-- | The value for the integrity algorithm.
piarlviValue :: Lens' Phase1IntegrityAlgorithmsRequestListValue (Maybe Text)
piarlviValue = lens _piarlviValue (\s a -> s {_piarlviValue = a})

instance Hashable Phase1IntegrityAlgorithmsRequestListValue

instance NFData Phase1IntegrityAlgorithmsRequestListValue

instance ToQuery Phase1IntegrityAlgorithmsRequestListValue where
  toQuery Phase1IntegrityAlgorithmsRequestListValue' {..} =
    mconcat ["Value" =: _piarlviValue]
