{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.AvailabilityOptionsStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.AvailabilityOptionsStatus where

import Network.AWS.CloudSearch.Types.OptionStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The status and configuration of the domain's availability options.
--
--
--
-- /See:/ 'availabilityOptionsStatus' smart constructor.
data AvailabilityOptionsStatus = AvailabilityOptionsStatus'
  { _aosOptions ::
      !Bool,
    _aosStatus :: !OptionStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AvailabilityOptionsStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aosOptions' - The availability options configured for the domain.
--
-- * 'aosStatus' - Undocumented member.
availabilityOptionsStatus ::
  -- | 'aosOptions'
  Bool ->
  -- | 'aosStatus'
  OptionStatus ->
  AvailabilityOptionsStatus
availabilityOptionsStatus pOptions_ pStatus_ =
  AvailabilityOptionsStatus'
    { _aosOptions = pOptions_,
      _aosStatus = pStatus_
    }

-- | The availability options configured for the domain.
aosOptions :: Lens' AvailabilityOptionsStatus Bool
aosOptions = lens _aosOptions (\s a -> s {_aosOptions = a})

-- | Undocumented member.
aosStatus :: Lens' AvailabilityOptionsStatus OptionStatus
aosStatus = lens _aosStatus (\s a -> s {_aosStatus = a})

instance FromXML AvailabilityOptionsStatus where
  parseXML x =
    AvailabilityOptionsStatus'
      <$> (x .@ "Options") <*> (x .@ "Status")

instance Hashable AvailabilityOptionsStatus

instance NFData AvailabilityOptionsStatus
