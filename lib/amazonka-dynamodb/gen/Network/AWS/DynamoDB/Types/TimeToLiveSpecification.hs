{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.TimeToLiveSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.TimeToLiveSpecification where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the settings used to enable or disable Time to Live (TTL) for the specified table.
--
--
--
-- /See:/ 'timeToLiveSpecification' smart constructor.
data TimeToLiveSpecification = TimeToLiveSpecification'
  { _ttlsEnabled ::
      !Bool,
    _ttlsAttributeName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TimeToLiveSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ttlsEnabled' - Indicates whether TTL is to be enabled (true) or disabled (false) on the table.
--
-- * 'ttlsAttributeName' - The name of the TTL attribute used to store the expiration time for items in the table.
timeToLiveSpecification ::
  -- | 'ttlsEnabled'
  Bool ->
  -- | 'ttlsAttributeName'
  Text ->
  TimeToLiveSpecification
timeToLiveSpecification pEnabled_ pAttributeName_ =
  TimeToLiveSpecification'
    { _ttlsEnabled = pEnabled_,
      _ttlsAttributeName = pAttributeName_
    }

-- | Indicates whether TTL is to be enabled (true) or disabled (false) on the table.
ttlsEnabled :: Lens' TimeToLiveSpecification Bool
ttlsEnabled = lens _ttlsEnabled (\s a -> s {_ttlsEnabled = a})

-- | The name of the TTL attribute used to store the expiration time for items in the table.
ttlsAttributeName :: Lens' TimeToLiveSpecification Text
ttlsAttributeName = lens _ttlsAttributeName (\s a -> s {_ttlsAttributeName = a})

instance FromJSON TimeToLiveSpecification where
  parseJSON =
    withObject
      "TimeToLiveSpecification"
      ( \x ->
          TimeToLiveSpecification'
            <$> (x .: "Enabled") <*> (x .: "AttributeName")
      )

instance Hashable TimeToLiveSpecification

instance NFData TimeToLiveSpecification

instance ToJSON TimeToLiveSpecification where
  toJSON TimeToLiveSpecification' {..} =
    object
      ( catMaybes
          [ Just ("Enabled" .= _ttlsEnabled),
            Just ("AttributeName" .= _ttlsAttributeName)
          ]
      )
