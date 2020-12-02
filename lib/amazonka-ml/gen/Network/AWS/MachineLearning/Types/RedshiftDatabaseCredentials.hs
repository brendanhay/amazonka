{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.RedshiftDatabaseCredentials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.RedshiftDatabaseCredentials where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the database credentials for connecting to a database on an Amazon Redshift cluster.
--
--
--
-- /See:/ 'redshiftDatabaseCredentials' smart constructor.
data RedshiftDatabaseCredentials = RedshiftDatabaseCredentials'
  { _rdcUsername ::
      !Text,
    _rdcPassword :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RedshiftDatabaseCredentials' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdcUsername' - Undocumented member.
--
-- * 'rdcPassword' - Undocumented member.
redshiftDatabaseCredentials ::
  -- | 'rdcUsername'
  Text ->
  -- | 'rdcPassword'
  Text ->
  RedshiftDatabaseCredentials
redshiftDatabaseCredentials pUsername_ pPassword_ =
  RedshiftDatabaseCredentials'
    { _rdcUsername = pUsername_,
      _rdcPassword = pPassword_
    }

-- | Undocumented member.
rdcUsername :: Lens' RedshiftDatabaseCredentials Text
rdcUsername = lens _rdcUsername (\s a -> s {_rdcUsername = a})

-- | Undocumented member.
rdcPassword :: Lens' RedshiftDatabaseCredentials Text
rdcPassword = lens _rdcPassword (\s a -> s {_rdcPassword = a})

instance Hashable RedshiftDatabaseCredentials

instance NFData RedshiftDatabaseCredentials

instance ToJSON RedshiftDatabaseCredentials where
  toJSON RedshiftDatabaseCredentials' {..} =
    object
      ( catMaybes
          [ Just ("Username" .= _rdcUsername),
            Just ("Password" .= _rdcPassword)
          ]
      )
