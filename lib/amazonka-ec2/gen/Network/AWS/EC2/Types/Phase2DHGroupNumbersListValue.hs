{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Phase2DHGroupNumbersListValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Phase2DHGroupNumbersListValue where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The Diffie-Hellmann group number for phase 2 IKE negotiations.
--
--
--
-- /See:/ 'phase2DHGroupNumbersListValue' smart constructor.
newtype Phase2DHGroupNumbersListValue = Phase2DHGroupNumbersListValue'
  { _pValue ::
      Maybe Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Phase2DHGroupNumbersListValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pValue' - The Diffie-Hellmann group number.
phase2DHGroupNumbersListValue ::
  Phase2DHGroupNumbersListValue
phase2DHGroupNumbersListValue =
  Phase2DHGroupNumbersListValue' {_pValue = Nothing}

-- | The Diffie-Hellmann group number.
pValue :: Lens' Phase2DHGroupNumbersListValue (Maybe Int)
pValue = lens _pValue (\s a -> s {_pValue = a})

instance FromXML Phase2DHGroupNumbersListValue where
  parseXML x = Phase2DHGroupNumbersListValue' <$> (x .@? "value")

instance Hashable Phase2DHGroupNumbersListValue

instance NFData Phase2DHGroupNumbersListValue
