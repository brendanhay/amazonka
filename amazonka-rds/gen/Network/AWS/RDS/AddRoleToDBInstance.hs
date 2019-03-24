{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.AddRoleToDBInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates an AWS Identity and Access Management (IAM) role with a DB instance.
--
--
module Network.AWS.RDS.AddRoleToDBInstance
    (
    -- * Creating a Request
      addRoleToDBInstance
    , AddRoleToDBInstance
    -- * Request Lenses
    , artdiDBInstanceIdentifier
    , artdiRoleARN
    , artdiFeatureName

    -- * Destructuring the Response
    , addRoleToDBInstanceResponse
    , AddRoleToDBInstanceResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'addRoleToDBInstance' smart constructor.
data AddRoleToDBInstance = AddRoleToDBInstance'
  { _artdiDBInstanceIdentifier :: !Text
  , _artdiRoleARN              :: !Text
  , _artdiFeatureName          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddRoleToDBInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'artdiDBInstanceIdentifier' - The name of the DB instance to associate the IAM role with.
--
-- * 'artdiRoleARN' - The Amazon Resource Name (ARN) of the IAM role to associate with the DB instance, for example @arn:aws:iam::123456789012:role/AccessRole@ .
--
-- * 'artdiFeatureName' - The name of the feature for the DB instance that the IAM role is to be associated with. For the list of supported feature names, see 'DBEngineVersion' .
addRoleToDBInstance
    :: Text -- ^ 'artdiDBInstanceIdentifier'
    -> Text -- ^ 'artdiRoleARN'
    -> Text -- ^ 'artdiFeatureName'
    -> AddRoleToDBInstance
addRoleToDBInstance pDBInstanceIdentifier_ pRoleARN_ pFeatureName_ =
  AddRoleToDBInstance'
    { _artdiDBInstanceIdentifier = pDBInstanceIdentifier_
    , _artdiRoleARN = pRoleARN_
    , _artdiFeatureName = pFeatureName_
    }


-- | The name of the DB instance to associate the IAM role with.
artdiDBInstanceIdentifier :: Lens' AddRoleToDBInstance Text
artdiDBInstanceIdentifier = lens _artdiDBInstanceIdentifier (\ s a -> s{_artdiDBInstanceIdentifier = a})

-- | The Amazon Resource Name (ARN) of the IAM role to associate with the DB instance, for example @arn:aws:iam::123456789012:role/AccessRole@ .
artdiRoleARN :: Lens' AddRoleToDBInstance Text
artdiRoleARN = lens _artdiRoleARN (\ s a -> s{_artdiRoleARN = a})

-- | The name of the feature for the DB instance that the IAM role is to be associated with. For the list of supported feature names, see 'DBEngineVersion' .
artdiFeatureName :: Lens' AddRoleToDBInstance Text
artdiFeatureName = lens _artdiFeatureName (\ s a -> s{_artdiFeatureName = a})

instance AWSRequest AddRoleToDBInstance where
        type Rs AddRoleToDBInstance =
             AddRoleToDBInstanceResponse
        request = postQuery rds
        response = receiveNull AddRoleToDBInstanceResponse'

instance Hashable AddRoleToDBInstance where

instance NFData AddRoleToDBInstance where

instance ToHeaders AddRoleToDBInstance where
        toHeaders = const mempty

instance ToPath AddRoleToDBInstance where
        toPath = const "/"

instance ToQuery AddRoleToDBInstance where
        toQuery AddRoleToDBInstance'{..}
          = mconcat
              ["Action" =: ("AddRoleToDBInstance" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "DBInstanceIdentifier" =: _artdiDBInstanceIdentifier,
               "RoleArn" =: _artdiRoleARN,
               "FeatureName" =: _artdiFeatureName]

-- | /See:/ 'addRoleToDBInstanceResponse' smart constructor.
data AddRoleToDBInstanceResponse =
  AddRoleToDBInstanceResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddRoleToDBInstanceResponse' with the minimum fields required to make a request.
--
addRoleToDBInstanceResponse
    :: AddRoleToDBInstanceResponse
addRoleToDBInstanceResponse = AddRoleToDBInstanceResponse'


instance NFData AddRoleToDBInstanceResponse where
