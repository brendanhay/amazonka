{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.Limit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.Limit where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies how many protections of a given type you can create.
--
--
--
-- /See:/ 'limit' smart constructor.
data Limit = Limit'
  { _lMax :: !(Maybe Integer),
    _lType :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Limit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lMax' - The maximum number of protections that can be created for the specified @Type@ .
--
-- * 'lType' - The type of protection.
limit ::
  Limit
limit = Limit' {_lMax = Nothing, _lType = Nothing}

-- | The maximum number of protections that can be created for the specified @Type@ .
lMax :: Lens' Limit (Maybe Integer)
lMax = lens _lMax (\s a -> s {_lMax = a})

-- | The type of protection.
lType :: Lens' Limit (Maybe Text)
lType = lens _lType (\s a -> s {_lType = a})

instance FromJSON Limit where
  parseJSON =
    withObject
      "Limit"
      (\x -> Limit' <$> (x .:? "Max") <*> (x .:? "Type"))

instance Hashable Limit

instance NFData Limit
