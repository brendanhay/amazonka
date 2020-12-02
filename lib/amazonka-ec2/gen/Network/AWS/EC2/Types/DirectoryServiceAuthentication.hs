{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DirectoryServiceAuthentication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DirectoryServiceAuthentication where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an Active Directory.
--
--
--
-- /See:/ 'directoryServiceAuthentication' smart constructor.
newtype DirectoryServiceAuthentication = DirectoryServiceAuthentication'
  { _dsaDirectoryId ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DirectoryServiceAuthentication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsaDirectoryId' - The ID of the Active Directory used for authentication.
directoryServiceAuthentication ::
  DirectoryServiceAuthentication
directoryServiceAuthentication =
  DirectoryServiceAuthentication' {_dsaDirectoryId = Nothing}

-- | The ID of the Active Directory used for authentication.
dsaDirectoryId :: Lens' DirectoryServiceAuthentication (Maybe Text)
dsaDirectoryId = lens _dsaDirectoryId (\s a -> s {_dsaDirectoryId = a})

instance FromXML DirectoryServiceAuthentication where
  parseXML x =
    DirectoryServiceAuthentication' <$> (x .@? "directoryId")

instance Hashable DirectoryServiceAuthentication

instance NFData DirectoryServiceAuthentication
