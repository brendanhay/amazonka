{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.BulkDeployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.BulkDeployment where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a bulk deployment. You cannot start a new bulk deployment while another one is still running or in a non-terminal state.
--
-- /See:/ 'bulkDeployment' smart constructor.
data BulkDeployment = BulkDeployment'
  { _bdBulkDeploymentARN ::
      !(Maybe Text),
    _bdBulkDeploymentId :: !(Maybe Text),
    _bdCreatedAt :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BulkDeployment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdBulkDeploymentARN' - The ARN of the bulk deployment.
--
-- * 'bdBulkDeploymentId' - The ID of the bulk deployment.
--
-- * 'bdCreatedAt' - The time, in ISO format, when the deployment was created.
bulkDeployment ::
  BulkDeployment
bulkDeployment =
  BulkDeployment'
    { _bdBulkDeploymentARN = Nothing,
      _bdBulkDeploymentId = Nothing,
      _bdCreatedAt = Nothing
    }

-- | The ARN of the bulk deployment.
bdBulkDeploymentARN :: Lens' BulkDeployment (Maybe Text)
bdBulkDeploymentARN = lens _bdBulkDeploymentARN (\s a -> s {_bdBulkDeploymentARN = a})

-- | The ID of the bulk deployment.
bdBulkDeploymentId :: Lens' BulkDeployment (Maybe Text)
bdBulkDeploymentId = lens _bdBulkDeploymentId (\s a -> s {_bdBulkDeploymentId = a})

-- | The time, in ISO format, when the deployment was created.
bdCreatedAt :: Lens' BulkDeployment (Maybe Text)
bdCreatedAt = lens _bdCreatedAt (\s a -> s {_bdCreatedAt = a})

instance FromJSON BulkDeployment where
  parseJSON =
    withObject
      "BulkDeployment"
      ( \x ->
          BulkDeployment'
            <$> (x .:? "BulkDeploymentArn")
            <*> (x .:? "BulkDeploymentId")
            <*> (x .:? "CreatedAt")
      )

instance Hashable BulkDeployment

instance NFData BulkDeployment
