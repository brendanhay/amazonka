{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Phase2IntegrityAlgorithmsRequestListValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Phase2IntegrityAlgorithmsRequestListValue where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the integrity algorithm for the VPN tunnel for phase 2 IKE negotiations.
--
--
--
-- /See:/ 'phase2IntegrityAlgorithmsRequestListValue' smart constructor.
newtype Phase2IntegrityAlgorithmsRequestListValue = Phase2IntegrityAlgorithmsRequestListValue'
  { _piarlvValue ::
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

-- | Creates a value of 'Phase2IntegrityAlgorithmsRequestListValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'piarlvValue' - The integrity algorithm.
phase2IntegrityAlgorithmsRequestListValue ::
  Phase2IntegrityAlgorithmsRequestListValue
phase2IntegrityAlgorithmsRequestListValue =
  Phase2IntegrityAlgorithmsRequestListValue'
    { _piarlvValue =
        Nothing
    }

-- | The integrity algorithm.
piarlvValue :: Lens' Phase2IntegrityAlgorithmsRequestListValue (Maybe Text)
piarlvValue = lens _piarlvValue (\s a -> s {_piarlvValue = a})

instance Hashable Phase2IntegrityAlgorithmsRequestListValue

instance NFData Phase2IntegrityAlgorithmsRequestListValue

instance ToQuery Phase2IntegrityAlgorithmsRequestListValue where
  toQuery Phase2IntegrityAlgorithmsRequestListValue' {..} =
    mconcat ["Value" =: _piarlvValue]
