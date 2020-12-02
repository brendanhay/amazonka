{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TableVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TableVersion where

import Network.AWS.Glue.Types.Table
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies a version of a table.
--
--
--
-- /See:/ 'tableVersion' smart constructor.
data TableVersion = TableVersion'
  { _tvVersionId :: !(Maybe Text),
    _tvTable :: !(Maybe Table)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TableVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tvVersionId' - The ID value that identifies this table version. A @VersionId@ is a string representation of an integer. Each version is incremented by 1.
--
-- * 'tvTable' - The table in question.
tableVersion ::
  TableVersion
tableVersion =
  TableVersion' {_tvVersionId = Nothing, _tvTable = Nothing}

-- | The ID value that identifies this table version. A @VersionId@ is a string representation of an integer. Each version is incremented by 1.
tvVersionId :: Lens' TableVersion (Maybe Text)
tvVersionId = lens _tvVersionId (\s a -> s {_tvVersionId = a})

-- | The table in question.
tvTable :: Lens' TableVersion (Maybe Table)
tvTable = lens _tvTable (\s a -> s {_tvTable = a})

instance FromJSON TableVersion where
  parseJSON =
    withObject
      "TableVersion"
      (\x -> TableVersion' <$> (x .:? "VersionId") <*> (x .:? "Table"))

instance Hashable TableVersion

instance NFData TableVersion
