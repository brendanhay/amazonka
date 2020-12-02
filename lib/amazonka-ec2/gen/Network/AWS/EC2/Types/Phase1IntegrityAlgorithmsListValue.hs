{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Phase1IntegrityAlgorithmsListValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Phase1IntegrityAlgorithmsListValue where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The integrity algorithm for phase 1 IKE negotiations.
--
--
--
-- /See:/ 'phase1IntegrityAlgorithmsListValue' smart constructor.
newtype Phase1IntegrityAlgorithmsListValue = Phase1IntegrityAlgorithmsListValue'
  { _pialvValue ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Phase1IntegrityAlgorithmsListValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pialvValue' - The value for the integrity algorithm.
phase1IntegrityAlgorithmsListValue ::
  Phase1IntegrityAlgorithmsListValue
phase1IntegrityAlgorithmsListValue =
  Phase1IntegrityAlgorithmsListValue' {_pialvValue = Nothing}

-- | The value for the integrity algorithm.
pialvValue :: Lens' Phase1IntegrityAlgorithmsListValue (Maybe Text)
pialvValue = lens _pialvValue (\s a -> s {_pialvValue = a})

instance FromXML Phase1IntegrityAlgorithmsListValue where
  parseXML x =
    Phase1IntegrityAlgorithmsListValue' <$> (x .@? "value")

instance Hashable Phase1IntegrityAlgorithmsListValue

instance NFData Phase1IntegrityAlgorithmsListValue
