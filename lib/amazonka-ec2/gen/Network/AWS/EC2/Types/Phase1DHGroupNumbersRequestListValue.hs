{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Phase1DHGroupNumbersRequestListValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Phase1DHGroupNumbersRequestListValue where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies a Diffie-Hellman group number for the VPN tunnel for phase 1 IKE negotiations.
--
--
--
-- /See:/ 'phase1DHGroupNumbersRequestListValue' smart constructor.
newtype Phase1DHGroupNumbersRequestListValue = Phase1DHGroupNumbersRequestListValue'
  { _pdhgnrlvValue ::
      Maybe Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Phase1DHGroupNumbersRequestListValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdhgnrlvValue' - The Diffie-Hellmann group number.
phase1DHGroupNumbersRequestListValue ::
  Phase1DHGroupNumbersRequestListValue
phase1DHGroupNumbersRequestListValue =
  Phase1DHGroupNumbersRequestListValue' {_pdhgnrlvValue = Nothing}

-- | The Diffie-Hellmann group number.
pdhgnrlvValue :: Lens' Phase1DHGroupNumbersRequestListValue (Maybe Int)
pdhgnrlvValue = lens _pdhgnrlvValue (\s a -> s {_pdhgnrlvValue = a})

instance Hashable Phase1DHGroupNumbersRequestListValue

instance NFData Phase1DHGroupNumbersRequestListValue

instance ToQuery Phase1DHGroupNumbersRequestListValue where
  toQuery Phase1DHGroupNumbersRequestListValue' {..} =
    mconcat ["Value" =: _pdhgnrlvValue]
