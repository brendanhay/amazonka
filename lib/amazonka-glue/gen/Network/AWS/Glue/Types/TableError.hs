{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TableError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TableError where

import Network.AWS.Glue.Types.ErrorDetail
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An error record for table operations.
--
--
--
-- /See:/ 'tableError' smart constructor.
data TableError = TableError'
  { _teTableName :: !(Maybe Text),
    _teErrorDetail :: !(Maybe ErrorDetail)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TableError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'teTableName' - The name of the table. For Hive compatibility, this must be entirely lowercase.
--
-- * 'teErrorDetail' - The details about the error.
tableError ::
  TableError
tableError =
  TableError' {_teTableName = Nothing, _teErrorDetail = Nothing}

-- | The name of the table. For Hive compatibility, this must be entirely lowercase.
teTableName :: Lens' TableError (Maybe Text)
teTableName = lens _teTableName (\s a -> s {_teTableName = a})

-- | The details about the error.
teErrorDetail :: Lens' TableError (Maybe ErrorDetail)
teErrorDetail = lens _teErrorDetail (\s a -> s {_teErrorDetail = a})

instance FromJSON TableError where
  parseJSON =
    withObject
      "TableError"
      ( \x ->
          TableError' <$> (x .:? "TableName") <*> (x .:? "ErrorDetail")
      )

instance Hashable TableError

instance NFData TableError
