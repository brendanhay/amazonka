{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.ProvisionedCapacityDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.ProvisionedCapacityDescription where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The definition for a provisioned capacity unit.
--
--
--
-- /See:/ 'provisionedCapacityDescription' smart constructor.
data ProvisionedCapacityDescription = ProvisionedCapacityDescription'
  { _pcdCapacityId ::
      !(Maybe Text),
    _pcdStartDate ::
      !(Maybe Text),
    _pcdExpirationDate ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProvisionedCapacityDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcdCapacityId' - The ID that identifies the provisioned capacity unit.
--
-- * 'pcdStartDate' - The date that the provisioned capacity unit was purchased, in Universal Coordinated Time (UTC).
--
-- * 'pcdExpirationDate' - The date that the provisioned capacity unit expires, in Universal Coordinated Time (UTC).
provisionedCapacityDescription ::
  ProvisionedCapacityDescription
provisionedCapacityDescription =
  ProvisionedCapacityDescription'
    { _pcdCapacityId = Nothing,
      _pcdStartDate = Nothing,
      _pcdExpirationDate = Nothing
    }

-- | The ID that identifies the provisioned capacity unit.
pcdCapacityId :: Lens' ProvisionedCapacityDescription (Maybe Text)
pcdCapacityId = lens _pcdCapacityId (\s a -> s {_pcdCapacityId = a})

-- | The date that the provisioned capacity unit was purchased, in Universal Coordinated Time (UTC).
pcdStartDate :: Lens' ProvisionedCapacityDescription (Maybe Text)
pcdStartDate = lens _pcdStartDate (\s a -> s {_pcdStartDate = a})

-- | The date that the provisioned capacity unit expires, in Universal Coordinated Time (UTC).
pcdExpirationDate :: Lens' ProvisionedCapacityDescription (Maybe Text)
pcdExpirationDate = lens _pcdExpirationDate (\s a -> s {_pcdExpirationDate = a})

instance FromJSON ProvisionedCapacityDescription where
  parseJSON =
    withObject
      "ProvisionedCapacityDescription"
      ( \x ->
          ProvisionedCapacityDescription'
            <$> (x .:? "CapacityId")
            <*> (x .:? "StartDate")
            <*> (x .:? "ExpirationDate")
      )

instance Hashable ProvisionedCapacityDescription

instance NFData ProvisionedCapacityDescription
