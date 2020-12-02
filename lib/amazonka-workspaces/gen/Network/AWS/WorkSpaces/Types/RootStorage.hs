{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.RootStorage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.RootStorage where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the root volume for a WorkSpace bundle.
--
--
--
-- /See:/ 'rootStorage' smart constructor.
newtype RootStorage = RootStorage' {_rsCapacity :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RootStorage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsCapacity' - The size of the root volume.
rootStorage ::
  RootStorage
rootStorage = RootStorage' {_rsCapacity = Nothing}

-- | The size of the root volume.
rsCapacity :: Lens' RootStorage (Maybe Text)
rsCapacity = lens _rsCapacity (\s a -> s {_rsCapacity = a})

instance FromJSON RootStorage where
  parseJSON =
    withObject
      "RootStorage"
      (\x -> RootStorage' <$> (x .:? "Capacity"))

instance Hashable RootStorage

instance NFData RootStorage
