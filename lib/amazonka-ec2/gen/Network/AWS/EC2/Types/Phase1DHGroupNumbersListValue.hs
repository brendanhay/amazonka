{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Phase1DHGroupNumbersListValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Phase1DHGroupNumbersListValue where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The Diffie-Hellmann group number for phase 1 IKE negotiations.
--
--
--
-- /See:/ 'phase1DHGroupNumbersListValue' smart constructor.
newtype Phase1DHGroupNumbersListValue = Phase1DHGroupNumbersListValue'
  { _pdhgnlvValue ::
      Maybe Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Phase1DHGroupNumbersListValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdhgnlvValue' - The Diffie-Hellmann group number.
phase1DHGroupNumbersListValue ::
  Phase1DHGroupNumbersListValue
phase1DHGroupNumbersListValue =
  Phase1DHGroupNumbersListValue' {_pdhgnlvValue = Nothing}

-- | The Diffie-Hellmann group number.
pdhgnlvValue :: Lens' Phase1DHGroupNumbersListValue (Maybe Int)
pdhgnlvValue = lens _pdhgnlvValue (\s a -> s {_pdhgnlvValue = a})

instance FromXML Phase1DHGroupNumbersListValue where
  parseXML x = Phase1DHGroupNumbersListValue' <$> (x .@? "value")

instance Hashable Phase1DHGroupNumbersListValue

instance NFData Phase1DHGroupNumbersListValue
