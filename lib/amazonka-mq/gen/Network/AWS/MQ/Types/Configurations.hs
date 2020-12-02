{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.Configurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.Configurations where

import Network.AWS.Lens
import Network.AWS.MQ.Types.ConfigurationId
import Network.AWS.Prelude

-- | Broker configuration information
--
-- /See:/ 'configurations' smart constructor.
data Configurations = Configurations'
  { _cPending ::
      !(Maybe ConfigurationId),
    _cHistory :: !(Maybe [ConfigurationId]),
    _cCurrent :: !(Maybe ConfigurationId)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Configurations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cPending' - The pending configuration of the broker.
--
-- * 'cHistory' - The history of configurations applied to the broker.
--
-- * 'cCurrent' - The current configuration of the broker.
configurations ::
  Configurations
configurations =
  Configurations'
    { _cPending = Nothing,
      _cHistory = Nothing,
      _cCurrent = Nothing
    }

-- | The pending configuration of the broker.
cPending :: Lens' Configurations (Maybe ConfigurationId)
cPending = lens _cPending (\s a -> s {_cPending = a})

-- | The history of configurations applied to the broker.
cHistory :: Lens' Configurations [ConfigurationId]
cHistory = lens _cHistory (\s a -> s {_cHistory = a}) . _Default . _Coerce

-- | The current configuration of the broker.
cCurrent :: Lens' Configurations (Maybe ConfigurationId)
cCurrent = lens _cCurrent (\s a -> s {_cCurrent = a})

instance FromJSON Configurations where
  parseJSON =
    withObject
      "Configurations"
      ( \x ->
          Configurations'
            <$> (x .:? "pending")
            <*> (x .:? "history" .!= mempty)
            <*> (x .:? "current")
      )

instance Hashable Configurations

instance NFData Configurations
