{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.DeleteGlobalSecondaryIndexAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.DeleteGlobalSecondaryIndexAction where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a global secondary index to be deleted from an existing table.
--
--
--
-- /See:/ 'deleteGlobalSecondaryIndexAction' smart constructor.
newtype DeleteGlobalSecondaryIndexAction = DeleteGlobalSecondaryIndexAction'
  { _dgsiaIndexName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteGlobalSecondaryIndexAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgsiaIndexName' - The name of the global secondary index to be deleted.
deleteGlobalSecondaryIndexAction ::
  -- | 'dgsiaIndexName'
  Text ->
  DeleteGlobalSecondaryIndexAction
deleteGlobalSecondaryIndexAction pIndexName_ =
  DeleteGlobalSecondaryIndexAction' {_dgsiaIndexName = pIndexName_}

-- | The name of the global secondary index to be deleted.
dgsiaIndexName :: Lens' DeleteGlobalSecondaryIndexAction Text
dgsiaIndexName = lens _dgsiaIndexName (\s a -> s {_dgsiaIndexName = a})

instance Hashable DeleteGlobalSecondaryIndexAction

instance NFData DeleteGlobalSecondaryIndexAction

instance ToJSON DeleteGlobalSecondaryIndexAction where
  toJSON DeleteGlobalSecondaryIndexAction' {..} =
    object (catMaybes [Just ("IndexName" .= _dgsiaIndexName)])
