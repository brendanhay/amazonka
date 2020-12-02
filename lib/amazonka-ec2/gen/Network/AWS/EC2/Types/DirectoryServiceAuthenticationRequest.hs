{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DirectoryServiceAuthenticationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DirectoryServiceAuthenticationRequest where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the Active Directory to be used for client authentication.
--
--
--
-- /See:/ 'directoryServiceAuthenticationRequest' smart constructor.
newtype DirectoryServiceAuthenticationRequest = DirectoryServiceAuthenticationRequest'
  { _dsarDirectoryId ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DirectoryServiceAuthenticationRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsarDirectoryId' - The ID of the Active Directory to be used for authentication.
directoryServiceAuthenticationRequest ::
  DirectoryServiceAuthenticationRequest
directoryServiceAuthenticationRequest =
  DirectoryServiceAuthenticationRequest'
    { _dsarDirectoryId =
        Nothing
    }

-- | The ID of the Active Directory to be used for authentication.
dsarDirectoryId :: Lens' DirectoryServiceAuthenticationRequest (Maybe Text)
dsarDirectoryId = lens _dsarDirectoryId (\s a -> s {_dsarDirectoryId = a})

instance Hashable DirectoryServiceAuthenticationRequest

instance NFData DirectoryServiceAuthenticationRequest

instance ToQuery DirectoryServiceAuthenticationRequest where
  toQuery DirectoryServiceAuthenticationRequest' {..} =
    mconcat ["DirectoryId" =: _dsarDirectoryId]
