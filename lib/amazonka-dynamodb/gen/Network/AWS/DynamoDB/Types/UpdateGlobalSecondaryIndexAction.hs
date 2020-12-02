{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.UpdateGlobalSecondaryIndexAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.UpdateGlobalSecondaryIndexAction where

import Network.AWS.DynamoDB.Types.ProvisionedThroughput
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the new provisioned throughput settings to be applied to a global secondary index.
--
--
--
-- /See:/ 'updateGlobalSecondaryIndexAction' smart constructor.
data UpdateGlobalSecondaryIndexAction = UpdateGlobalSecondaryIndexAction'
  { _ugsiaIndexName ::
      !Text,
    _ugsiaProvisionedThroughput ::
      !ProvisionedThroughput
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateGlobalSecondaryIndexAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugsiaIndexName' - The name of the global secondary index to be updated.
--
-- * 'ugsiaProvisionedThroughput' - Represents the provisioned throughput settings for the specified global secondary index. For current minimum and maximum provisioned throughput values, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas> in the /Amazon DynamoDB Developer Guide/ .
updateGlobalSecondaryIndexAction ::
  -- | 'ugsiaIndexName'
  Text ->
  -- | 'ugsiaProvisionedThroughput'
  ProvisionedThroughput ->
  UpdateGlobalSecondaryIndexAction
updateGlobalSecondaryIndexAction
  pIndexName_
  pProvisionedThroughput_ =
    UpdateGlobalSecondaryIndexAction'
      { _ugsiaIndexName = pIndexName_,
        _ugsiaProvisionedThroughput = pProvisionedThroughput_
      }

-- | The name of the global secondary index to be updated.
ugsiaIndexName :: Lens' UpdateGlobalSecondaryIndexAction Text
ugsiaIndexName = lens _ugsiaIndexName (\s a -> s {_ugsiaIndexName = a})

-- | Represents the provisioned throughput settings for the specified global secondary index. For current minimum and maximum provisioned throughput values, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas> in the /Amazon DynamoDB Developer Guide/ .
ugsiaProvisionedThroughput :: Lens' UpdateGlobalSecondaryIndexAction ProvisionedThroughput
ugsiaProvisionedThroughput = lens _ugsiaProvisionedThroughput (\s a -> s {_ugsiaProvisionedThroughput = a})

instance Hashable UpdateGlobalSecondaryIndexAction

instance NFData UpdateGlobalSecondaryIndexAction

instance ToJSON UpdateGlobalSecondaryIndexAction where
  toJSON UpdateGlobalSecondaryIndexAction' {..} =
    object
      ( catMaybes
          [ Just ("IndexName" .= _ugsiaIndexName),
            Just ("ProvisionedThroughput" .= _ugsiaProvisionedThroughput)
          ]
      )
