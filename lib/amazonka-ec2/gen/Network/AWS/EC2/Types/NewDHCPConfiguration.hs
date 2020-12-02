{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NewDHCPConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NewDHCPConfiguration where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | /See:/ 'newDHCPConfiguration' smart constructor.
data NewDHCPConfiguration = NewDHCPConfiguration'
  { _ndcValues ::
      !(Maybe [Text]),
    _ndcKey :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NewDHCPConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ndcValues' - Undocumented member.
--
-- * 'ndcKey' - Undocumented member.
newDHCPConfiguration ::
  NewDHCPConfiguration
newDHCPConfiguration =
  NewDHCPConfiguration' {_ndcValues = Nothing, _ndcKey = Nothing}

-- | Undocumented member.
ndcValues :: Lens' NewDHCPConfiguration [Text]
ndcValues = lens _ndcValues (\s a -> s {_ndcValues = a}) . _Default . _Coerce

-- | Undocumented member.
ndcKey :: Lens' NewDHCPConfiguration (Maybe Text)
ndcKey = lens _ndcKey (\s a -> s {_ndcKey = a})

instance Hashable NewDHCPConfiguration

instance NFData NewDHCPConfiguration

instance ToQuery NewDHCPConfiguration where
  toQuery NewDHCPConfiguration' {..} =
    mconcat
      [toQuery (toQueryList "Value" <$> _ndcValues), "Key" =: _ndcKey]
