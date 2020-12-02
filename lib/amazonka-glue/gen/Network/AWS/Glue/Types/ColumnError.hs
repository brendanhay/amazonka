{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ColumnError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ColumnError where

import Network.AWS.Glue.Types.ErrorDetail
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Encapsulates a column name that failed and the reason for failure.
--
--
--
-- /See:/ 'columnError' smart constructor.
data ColumnError = ColumnError'
  { _ceError :: !(Maybe ErrorDetail),
    _ceColumnName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ColumnError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ceError' - An error message with the reason for the failure of an operation.
--
-- * 'ceColumnName' - The name of the column that failed.
columnError ::
  ColumnError
columnError =
  ColumnError' {_ceError = Nothing, _ceColumnName = Nothing}

-- | An error message with the reason for the failure of an operation.
ceError :: Lens' ColumnError (Maybe ErrorDetail)
ceError = lens _ceError (\s a -> s {_ceError = a})

-- | The name of the column that failed.
ceColumnName :: Lens' ColumnError (Maybe Text)
ceColumnName = lens _ceColumnName (\s a -> s {_ceColumnName = a})

instance FromJSON ColumnError where
  parseJSON =
    withObject
      "ColumnError"
      (\x -> ColumnError' <$> (x .:? "Error") <*> (x .:? "ColumnName"))

instance Hashable ColumnError

instance NFData ColumnError
