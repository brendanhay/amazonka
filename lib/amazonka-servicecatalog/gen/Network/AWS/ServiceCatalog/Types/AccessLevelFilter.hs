{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.AccessLevelFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.AccessLevelFilter where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.ServiceCatalog.Types.AccessLevelFilterKey

-- | The access level to use to filter results.
--
--
--
-- /See:/ 'accessLevelFilter' smart constructor.
data AccessLevelFilter = AccessLevelFilter'
  { _alfValue ::
      !(Maybe Text),
    _alfKey :: !(Maybe AccessLevelFilterKey)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AccessLevelFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'alfValue' - The user to which the access level applies. The only supported value is @Self@ .
--
-- * 'alfKey' - The access level.     * @Account@ - Filter results based on the account.     * @Role@ - Filter results based on the federated role of the specified user.     * @User@ - Filter results based on the specified user.
accessLevelFilter ::
  AccessLevelFilter
accessLevelFilter =
  AccessLevelFilter' {_alfValue = Nothing, _alfKey = Nothing}

-- | The user to which the access level applies. The only supported value is @Self@ .
alfValue :: Lens' AccessLevelFilter (Maybe Text)
alfValue = lens _alfValue (\s a -> s {_alfValue = a})

-- | The access level.     * @Account@ - Filter results based on the account.     * @Role@ - Filter results based on the federated role of the specified user.     * @User@ - Filter results based on the specified user.
alfKey :: Lens' AccessLevelFilter (Maybe AccessLevelFilterKey)
alfKey = lens _alfKey (\s a -> s {_alfKey = a})

instance Hashable AccessLevelFilter

instance NFData AccessLevelFilter

instance ToJSON AccessLevelFilter where
  toJSON AccessLevelFilter' {..} =
    object
      (catMaybes [("Value" .=) <$> _alfValue, ("Key" .=) <$> _alfKey])
