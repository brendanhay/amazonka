{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Phase2IntegrityAlgorithmsListValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Phase2IntegrityAlgorithmsListValue where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The integrity algorithm for phase 2 IKE negotiations.
--
--
--
-- /See:/ 'phase2IntegrityAlgorithmsListValue' smart constructor.
newtype Phase2IntegrityAlgorithmsListValue = Phase2IntegrityAlgorithmsListValue'
  { _phaValue ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Phase2IntegrityAlgorithmsListValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'phaValue' - The integrity algorithm.
phase2IntegrityAlgorithmsListValue ::
  Phase2IntegrityAlgorithmsListValue
phase2IntegrityAlgorithmsListValue =
  Phase2IntegrityAlgorithmsListValue' {_phaValue = Nothing}

-- | The integrity algorithm.
phaValue :: Lens' Phase2IntegrityAlgorithmsListValue (Maybe Text)
phaValue = lens _phaValue (\s a -> s {_phaValue = a})

instance FromXML Phase2IntegrityAlgorithmsListValue where
  parseXML x =
    Phase2IntegrityAlgorithmsListValue' <$> (x .@? "value")

instance Hashable Phase2IntegrityAlgorithmsListValue

instance NFData Phase2IntegrityAlgorithmsListValue
