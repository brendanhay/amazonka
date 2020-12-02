{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.StartBulkDeployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deploys multiple groups in one operation. This action starts the bulk deployment of a specified set of group versions. Each group version deployment will be triggered with an adaptive rate that has a fixed upper limit. We recommend that you include an ''X-Amzn-Client-Token'' token in every ''StartBulkDeployment'' request. These requests are idempotent with respect to the token and the request parameters.
module Network.AWS.Greengrass.StartBulkDeployment
  ( -- * Creating a Request
    startBulkDeployment,
    StartBulkDeployment,

    -- * Request Lenses
    sbdAmznClientToken,
    sbdTags,
    sbdExecutionRoleARN,
    sbdInputFileURI,

    -- * Destructuring the Response
    startBulkDeploymentResponse,
    StartBulkDeploymentResponse,

    -- * Response Lenses
    srsBulkDeploymentARN,
    srsBulkDeploymentId,
    srsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startBulkDeployment' smart constructor.
data StartBulkDeployment = StartBulkDeployment'
  { _sbdAmznClientToken ::
      !(Maybe Text),
    _sbdTags :: !(Maybe (Map Text (Text))),
    _sbdExecutionRoleARN :: !Text,
    _sbdInputFileURI :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartBulkDeployment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sbdAmznClientToken' - A client token used to correlate requests and responses.
--
-- * 'sbdTags' - Tag(s) to add to the new resource.
--
-- * 'sbdExecutionRoleARN' - The ARN of the execution role to associate with the bulk deployment operation. This IAM role must allow the ''greengrass:CreateDeployment'' action for all group versions that are listed in the input file. This IAM role must have access to the S3 bucket containing the input file.
--
-- * 'sbdInputFileURI' - The URI of the input file contained in the S3 bucket. The execution role must have ''getObject'' permissions on this bucket to access the input file. The input file is a JSON-serialized, line delimited file with UTF-8 encoding that provides a list of group and version IDs and the deployment type. This file must be less than 100 MB. Currently, AWS IoT Greengrass supports only ''NewDeployment'' deployment types.
startBulkDeployment ::
  -- | 'sbdExecutionRoleARN'
  Text ->
  -- | 'sbdInputFileURI'
  Text ->
  StartBulkDeployment
startBulkDeployment pExecutionRoleARN_ pInputFileURI_ =
  StartBulkDeployment'
    { _sbdAmznClientToken = Nothing,
      _sbdTags = Nothing,
      _sbdExecutionRoleARN = pExecutionRoleARN_,
      _sbdInputFileURI = pInputFileURI_
    }

-- | A client token used to correlate requests and responses.
sbdAmznClientToken :: Lens' StartBulkDeployment (Maybe Text)
sbdAmznClientToken = lens _sbdAmznClientToken (\s a -> s {_sbdAmznClientToken = a})

-- | Tag(s) to add to the new resource.
sbdTags :: Lens' StartBulkDeployment (HashMap Text (Text))
sbdTags = lens _sbdTags (\s a -> s {_sbdTags = a}) . _Default . _Map

-- | The ARN of the execution role to associate with the bulk deployment operation. This IAM role must allow the ''greengrass:CreateDeployment'' action for all group versions that are listed in the input file. This IAM role must have access to the S3 bucket containing the input file.
sbdExecutionRoleARN :: Lens' StartBulkDeployment Text
sbdExecutionRoleARN = lens _sbdExecutionRoleARN (\s a -> s {_sbdExecutionRoleARN = a})

-- | The URI of the input file contained in the S3 bucket. The execution role must have ''getObject'' permissions on this bucket to access the input file. The input file is a JSON-serialized, line delimited file with UTF-8 encoding that provides a list of group and version IDs and the deployment type. This file must be less than 100 MB. Currently, AWS IoT Greengrass supports only ''NewDeployment'' deployment types.
sbdInputFileURI :: Lens' StartBulkDeployment Text
sbdInputFileURI = lens _sbdInputFileURI (\s a -> s {_sbdInputFileURI = a})

instance AWSRequest StartBulkDeployment where
  type Rs StartBulkDeployment = StartBulkDeploymentResponse
  request = postJSON greengrass
  response =
    receiveJSON
      ( \s h x ->
          StartBulkDeploymentResponse'
            <$> (x .?> "BulkDeploymentArn")
            <*> (x .?> "BulkDeploymentId")
            <*> (pure (fromEnum s))
      )

instance Hashable StartBulkDeployment

instance NFData StartBulkDeployment

instance ToHeaders StartBulkDeployment where
  toHeaders StartBulkDeployment' {..} =
    mconcat
      [ "X-Amzn-Client-Token" =# _sbdAmznClientToken,
        "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
      ]

instance ToJSON StartBulkDeployment where
  toJSON StartBulkDeployment' {..} =
    object
      ( catMaybes
          [ ("tags" .=) <$> _sbdTags,
            Just ("ExecutionRoleArn" .= _sbdExecutionRoleARN),
            Just ("InputFileUri" .= _sbdInputFileURI)
          ]
      )

instance ToPath StartBulkDeployment where
  toPath = const "/greengrass/bulk/deployments"

instance ToQuery StartBulkDeployment where
  toQuery = const mempty

-- | /See:/ 'startBulkDeploymentResponse' smart constructor.
data StartBulkDeploymentResponse = StartBulkDeploymentResponse'
  { _srsBulkDeploymentARN ::
      !(Maybe Text),
    _srsBulkDeploymentId ::
      !(Maybe Text),
    _srsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartBulkDeploymentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsBulkDeploymentARN' - The ARN of the bulk deployment.
--
-- * 'srsBulkDeploymentId' - The ID of the bulk deployment.
--
-- * 'srsResponseStatus' - -- | The response status code.
startBulkDeploymentResponse ::
  -- | 'srsResponseStatus'
  Int ->
  StartBulkDeploymentResponse
startBulkDeploymentResponse pResponseStatus_ =
  StartBulkDeploymentResponse'
    { _srsBulkDeploymentARN = Nothing,
      _srsBulkDeploymentId = Nothing,
      _srsResponseStatus = pResponseStatus_
    }

-- | The ARN of the bulk deployment.
srsBulkDeploymentARN :: Lens' StartBulkDeploymentResponse (Maybe Text)
srsBulkDeploymentARN = lens _srsBulkDeploymentARN (\s a -> s {_srsBulkDeploymentARN = a})

-- | The ID of the bulk deployment.
srsBulkDeploymentId :: Lens' StartBulkDeploymentResponse (Maybe Text)
srsBulkDeploymentId = lens _srsBulkDeploymentId (\s a -> s {_srsBulkDeploymentId = a})

-- | -- | The response status code.
srsResponseStatus :: Lens' StartBulkDeploymentResponse Int
srsResponseStatus = lens _srsResponseStatus (\s a -> s {_srsResponseStatus = a})

instance NFData StartBulkDeploymentResponse
