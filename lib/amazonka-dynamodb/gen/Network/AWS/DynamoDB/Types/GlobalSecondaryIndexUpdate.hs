{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.GlobalSecondaryIndexUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.GlobalSecondaryIndexUpdate where

import Network.AWS.DynamoDB.Types.CreateGlobalSecondaryIndexAction
import Network.AWS.DynamoDB.Types.DeleteGlobalSecondaryIndexAction
import Network.AWS.DynamoDB.Types.UpdateGlobalSecondaryIndexAction
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents one of the following:
--
--
--     * A new global secondary index to be added to an existing table.
--
--     * New provisioned throughput parameters for an existing global secondary index.
--
--     * An existing global secondary index to be removed from an existing table.
--
--
--
--
-- /See:/ 'globalSecondaryIndexUpdate' smart constructor.
data GlobalSecondaryIndexUpdate = GlobalSecondaryIndexUpdate'
  { _gsiuCreate ::
      !( Maybe
           CreateGlobalSecondaryIndexAction
       ),
    _gsiuDelete ::
      !( Maybe
           DeleteGlobalSecondaryIndexAction
       ),
    _gsiuUpdate ::
      !( Maybe
           UpdateGlobalSecondaryIndexAction
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GlobalSecondaryIndexUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsiuCreate' - The parameters required for creating a global secondary index on an existing table:     * @IndexName @      * @KeySchema @      * @AttributeDefinitions @      * @Projection @      * @ProvisionedThroughput @
--
-- * 'gsiuDelete' - The name of an existing global secondary index to be removed.
--
-- * 'gsiuUpdate' - The name of an existing global secondary index, along with new provisioned throughput settings to be applied to that index.
globalSecondaryIndexUpdate ::
  GlobalSecondaryIndexUpdate
globalSecondaryIndexUpdate =
  GlobalSecondaryIndexUpdate'
    { _gsiuCreate = Nothing,
      _gsiuDelete = Nothing,
      _gsiuUpdate = Nothing
    }

-- | The parameters required for creating a global secondary index on an existing table:     * @IndexName @      * @KeySchema @      * @AttributeDefinitions @      * @Projection @      * @ProvisionedThroughput @
gsiuCreate :: Lens' GlobalSecondaryIndexUpdate (Maybe CreateGlobalSecondaryIndexAction)
gsiuCreate = lens _gsiuCreate (\s a -> s {_gsiuCreate = a})

-- | The name of an existing global secondary index to be removed.
gsiuDelete :: Lens' GlobalSecondaryIndexUpdate (Maybe DeleteGlobalSecondaryIndexAction)
gsiuDelete = lens _gsiuDelete (\s a -> s {_gsiuDelete = a})

-- | The name of an existing global secondary index, along with new provisioned throughput settings to be applied to that index.
gsiuUpdate :: Lens' GlobalSecondaryIndexUpdate (Maybe UpdateGlobalSecondaryIndexAction)
gsiuUpdate = lens _gsiuUpdate (\s a -> s {_gsiuUpdate = a})

instance Hashable GlobalSecondaryIndexUpdate

instance NFData GlobalSecondaryIndexUpdate

instance ToJSON GlobalSecondaryIndexUpdate where
  toJSON GlobalSecondaryIndexUpdate' {..} =
    object
      ( catMaybes
          [ ("Create" .=) <$> _gsiuCreate,
            ("Delete" .=) <$> _gsiuDelete,
            ("Update" .=) <$> _gsiuUpdate
          ]
      )
