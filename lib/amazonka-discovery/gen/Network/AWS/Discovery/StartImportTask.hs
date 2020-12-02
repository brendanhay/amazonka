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
-- Module      : Network.AWS.Discovery.StartImportTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an import task, which allows you to import details of your on-premises environment directly into AWS Migration Hub without having to use the Application Discovery Service (ADS) tools such as the Discovery Connector or Discovery Agent. This gives you the option to perform migration assessment and planning directly from your imported data, including the ability to group your devices as applications and track their migration status.
--
--
-- To start an import request, do this:
--
--     * Download the specially formatted comma separated value (CSV) import template, which you can find here: <https://s3-us-west-2.amazonaws.com/templates-7cffcf56-bd96-4b1c-b45b-a5b42f282e46/import_template.csv https://s3-us-west-2.amazonaws.com/templates-7cffcf56-bd96-4b1c-b45b-a5b42f282e46/import_template.csv> .
--
--     * Fill out the template with your server and application data.
--
--     * Upload your import file to an Amazon S3 bucket, and make a note of it's Object URL. Your import file must be in the CSV format.
--
--     * Use the console or the @StartImportTask@ command with the AWS CLI or one of the AWS SDKs to import the records from your file.
--
--
--
-- For more information, including step-by-step procedures, see <https://docs.aws.amazon.com/application-discovery/latest/userguide/discovery-import.html Migration Hub Import> in the /AWS Application Discovery Service User Guide/ .
module Network.AWS.Discovery.StartImportTask
  ( -- * Creating a Request
    startImportTask,
    StartImportTask,

    -- * Request Lenses
    sitClientRequestToken,
    sitName,
    sitImportURL,

    -- * Destructuring the Response
    startImportTaskResponse,
    StartImportTaskResponse,

    -- * Response Lenses
    sitrsTask,
    sitrsResponseStatus,
  )
where

import Network.AWS.Discovery.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startImportTask' smart constructor.
data StartImportTask = StartImportTask'
  { _sitClientRequestToken ::
      !(Maybe Text),
    _sitName :: !Text,
    _sitImportURL :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartImportTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sitClientRequestToken' - Optional. A unique token that you can provide to prevent the same import request from occurring more than once. If you don't provide a token, a token is automatically generated. Sending more than one @StartImportTask@ request with the same client request token will return information about the original import task with that client request token.
--
-- * 'sitName' - A descriptive name for this request. You can use this name to filter future requests related to this import task, such as identifying applications and servers that were included in this import task. We recommend that you use a meaningful name for each import task.
--
-- * 'sitImportURL' - The URL for your import file that you've uploaded to Amazon S3.
startImportTask ::
  -- | 'sitName'
  Text ->
  -- | 'sitImportURL'
  Text ->
  StartImportTask
startImportTask pName_ pImportURL_ =
  StartImportTask'
    { _sitClientRequestToken = Nothing,
      _sitName = pName_,
      _sitImportURL = pImportURL_
    }

-- | Optional. A unique token that you can provide to prevent the same import request from occurring more than once. If you don't provide a token, a token is automatically generated. Sending more than one @StartImportTask@ request with the same client request token will return information about the original import task with that client request token.
sitClientRequestToken :: Lens' StartImportTask (Maybe Text)
sitClientRequestToken = lens _sitClientRequestToken (\s a -> s {_sitClientRequestToken = a})

-- | A descriptive name for this request. You can use this name to filter future requests related to this import task, such as identifying applications and servers that were included in this import task. We recommend that you use a meaningful name for each import task.
sitName :: Lens' StartImportTask Text
sitName = lens _sitName (\s a -> s {_sitName = a})

-- | The URL for your import file that you've uploaded to Amazon S3.
sitImportURL :: Lens' StartImportTask Text
sitImportURL = lens _sitImportURL (\s a -> s {_sitImportURL = a})

instance AWSRequest StartImportTask where
  type Rs StartImportTask = StartImportTaskResponse
  request = postJSON discovery
  response =
    receiveJSON
      ( \s h x ->
          StartImportTaskResponse'
            <$> (x .?> "task") <*> (pure (fromEnum s))
      )

instance Hashable StartImportTask

instance NFData StartImportTask

instance ToHeaders StartImportTask where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSPoseidonService_V2015_11_01.StartImportTask" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StartImportTask where
  toJSON StartImportTask' {..} =
    object
      ( catMaybes
          [ ("clientRequestToken" .=) <$> _sitClientRequestToken,
            Just ("name" .= _sitName),
            Just ("importUrl" .= _sitImportURL)
          ]
      )

instance ToPath StartImportTask where
  toPath = const "/"

instance ToQuery StartImportTask where
  toQuery = const mempty

-- | /See:/ 'startImportTaskResponse' smart constructor.
data StartImportTaskResponse = StartImportTaskResponse'
  { _sitrsTask ::
      !(Maybe ImportTask),
    _sitrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartImportTaskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sitrsTask' - An array of information related to the import task request including status information, times, IDs, the Amazon S3 Object URL for the import file, and more.
--
-- * 'sitrsResponseStatus' - -- | The response status code.
startImportTaskResponse ::
  -- | 'sitrsResponseStatus'
  Int ->
  StartImportTaskResponse
startImportTaskResponse pResponseStatus_ =
  StartImportTaskResponse'
    { _sitrsTask = Nothing,
      _sitrsResponseStatus = pResponseStatus_
    }

-- | An array of information related to the import task request including status information, times, IDs, the Amazon S3 Object URL for the import file, and more.
sitrsTask :: Lens' StartImportTaskResponse (Maybe ImportTask)
sitrsTask = lens _sitrsTask (\s a -> s {_sitrsTask = a})

-- | -- | The response status code.
sitrsResponseStatus :: Lens' StartImportTaskResponse Int
sitrsResponseStatus = lens _sitrsResponseStatus (\s a -> s {_sitrsResponseStatus = a})

instance NFData StartImportTaskResponse
