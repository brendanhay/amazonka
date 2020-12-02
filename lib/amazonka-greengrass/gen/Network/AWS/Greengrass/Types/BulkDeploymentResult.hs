{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.BulkDeploymentResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.BulkDeploymentResult where

import Network.AWS.Greengrass.Types.DeploymentType
import Network.AWS.Greengrass.Types.ErrorDetail
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about an individual group deployment in a bulk deployment operation.
--
-- /See:/ 'bulkDeploymentResult' smart constructor.
data BulkDeploymentResult = BulkDeploymentResult'
  { _bdrDeploymentId ::
      !(Maybe Text),
    _bdrDeploymentARN :: !(Maybe Text),
    _bdrCreatedAt :: !(Maybe Text),
    _bdrDeploymentType :: !(Maybe DeploymentType),
    _bdrErrorDetails :: !(Maybe [ErrorDetail]),
    _bdrGroupARN :: !(Maybe Text),
    _bdrDeploymentStatus :: !(Maybe Text),
    _bdrErrorMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BulkDeploymentResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdrDeploymentId' - The ID of the group deployment.
--
-- * 'bdrDeploymentARN' - The ARN of the group deployment.
--
-- * 'bdrCreatedAt' - The time, in ISO format, when the deployment was created.
--
-- * 'bdrDeploymentType' - The type of the deployment.
--
-- * 'bdrErrorDetails' - Details about the error.
--
-- * 'bdrGroupARN' - The ARN of the Greengrass group.
--
-- * 'bdrDeploymentStatus' - The current status of the group deployment: ''InProgress'', ''Building'', ''Success'', or ''Failure''.
--
-- * 'bdrErrorMessage' - The error message for a failed deployment
bulkDeploymentResult ::
  BulkDeploymentResult
bulkDeploymentResult =
  BulkDeploymentResult'
    { _bdrDeploymentId = Nothing,
      _bdrDeploymentARN = Nothing,
      _bdrCreatedAt = Nothing,
      _bdrDeploymentType = Nothing,
      _bdrErrorDetails = Nothing,
      _bdrGroupARN = Nothing,
      _bdrDeploymentStatus = Nothing,
      _bdrErrorMessage = Nothing
    }

-- | The ID of the group deployment.
bdrDeploymentId :: Lens' BulkDeploymentResult (Maybe Text)
bdrDeploymentId = lens _bdrDeploymentId (\s a -> s {_bdrDeploymentId = a})

-- | The ARN of the group deployment.
bdrDeploymentARN :: Lens' BulkDeploymentResult (Maybe Text)
bdrDeploymentARN = lens _bdrDeploymentARN (\s a -> s {_bdrDeploymentARN = a})

-- | The time, in ISO format, when the deployment was created.
bdrCreatedAt :: Lens' BulkDeploymentResult (Maybe Text)
bdrCreatedAt = lens _bdrCreatedAt (\s a -> s {_bdrCreatedAt = a})

-- | The type of the deployment.
bdrDeploymentType :: Lens' BulkDeploymentResult (Maybe DeploymentType)
bdrDeploymentType = lens _bdrDeploymentType (\s a -> s {_bdrDeploymentType = a})

-- | Details about the error.
bdrErrorDetails :: Lens' BulkDeploymentResult [ErrorDetail]
bdrErrorDetails = lens _bdrErrorDetails (\s a -> s {_bdrErrorDetails = a}) . _Default . _Coerce

-- | The ARN of the Greengrass group.
bdrGroupARN :: Lens' BulkDeploymentResult (Maybe Text)
bdrGroupARN = lens _bdrGroupARN (\s a -> s {_bdrGroupARN = a})

-- | The current status of the group deployment: ''InProgress'', ''Building'', ''Success'', or ''Failure''.
bdrDeploymentStatus :: Lens' BulkDeploymentResult (Maybe Text)
bdrDeploymentStatus = lens _bdrDeploymentStatus (\s a -> s {_bdrDeploymentStatus = a})

-- | The error message for a failed deployment
bdrErrorMessage :: Lens' BulkDeploymentResult (Maybe Text)
bdrErrorMessage = lens _bdrErrorMessage (\s a -> s {_bdrErrorMessage = a})

instance FromJSON BulkDeploymentResult where
  parseJSON =
    withObject
      "BulkDeploymentResult"
      ( \x ->
          BulkDeploymentResult'
            <$> (x .:? "DeploymentId")
            <*> (x .:? "DeploymentArn")
            <*> (x .:? "CreatedAt")
            <*> (x .:? "DeploymentType")
            <*> (x .:? "ErrorDetails" .!= mempty)
            <*> (x .:? "GroupArn")
            <*> (x .:? "DeploymentStatus")
            <*> (x .:? "ErrorMessage")
      )

instance Hashable BulkDeploymentResult

instance NFData BulkDeploymentResult
