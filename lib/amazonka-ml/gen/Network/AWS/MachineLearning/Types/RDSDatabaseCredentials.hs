{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.RDSDatabaseCredentials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.RDSDatabaseCredentials where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The database credentials to connect to a database on an RDS DB instance.
--
--
--
-- /See:/ 'rdsDatabaseCredentials' smart constructor.
data RDSDatabaseCredentials = RDSDatabaseCredentials'
  { _rdsdcUsername ::
      !Text,
    _rdsdcPassword :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RDSDatabaseCredentials' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdsdcUsername' - Undocumented member.
--
-- * 'rdsdcPassword' - Undocumented member.
rdsDatabaseCredentials ::
  -- | 'rdsdcUsername'
  Text ->
  -- | 'rdsdcPassword'
  Text ->
  RDSDatabaseCredentials
rdsDatabaseCredentials pUsername_ pPassword_ =
  RDSDatabaseCredentials'
    { _rdsdcUsername = pUsername_,
      _rdsdcPassword = pPassword_
    }

-- | Undocumented member.
rdsdcUsername :: Lens' RDSDatabaseCredentials Text
rdsdcUsername = lens _rdsdcUsername (\s a -> s {_rdsdcUsername = a})

-- | Undocumented member.
rdsdcPassword :: Lens' RDSDatabaseCredentials Text
rdsdcPassword = lens _rdsdcPassword (\s a -> s {_rdsdcPassword = a})

instance Hashable RDSDatabaseCredentials

instance NFData RDSDatabaseCredentials

instance ToJSON RDSDatabaseCredentials where
  toJSON RDSDatabaseCredentials' {..} =
    object
      ( catMaybes
          [ Just ("Username" .= _rdsdcUsername),
            Just ("Password" .= _rdsdcPassword)
          ]
      )
