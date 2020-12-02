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
-- Module      : Network.AWS.Translate.UpdateParallelData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a previously created parallel data resource by importing a new input file from Amazon S3.
module Network.AWS.Translate.UpdateParallelData
  ( -- * Creating a Request
    updateParallelData,
    UpdateParallelData,

    -- * Request Lenses
    updDescription,
    updName,
    updParallelDataConfig,
    updClientToken,

    -- * Destructuring the Response
    updateParallelDataResponse,
    UpdateParallelDataResponse,

    -- * Response Lenses
    updrsStatus,
    updrsName,
    updrsLatestUpdateAttemptAt,
    updrsLatestUpdateAttemptStatus,
    updrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Translate.Types

-- | /See:/ 'updateParallelData' smart constructor.
data UpdateParallelData = UpdateParallelData'
  { _updDescription ::
      !(Maybe Text),
    _updName :: !Text,
    _updParallelDataConfig :: !ParallelDataConfig,
    _updClientToken :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateParallelData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'updDescription' - A custom description for the parallel data resource in Amazon Translate.
--
-- * 'updName' - The name of the parallel data resource being updated.
--
-- * 'updParallelDataConfig' - Specifies the format and S3 location of the parallel data input file.
--
-- * 'updClientToken' - A unique identifier for the request. This token is automatically generated when you use Amazon Translate through an AWS SDK.
updateParallelData ::
  -- | 'updName'
  Text ->
  -- | 'updParallelDataConfig'
  ParallelDataConfig ->
  -- | 'updClientToken'
  Text ->
  UpdateParallelData
updateParallelData pName_ pParallelDataConfig_ pClientToken_ =
  UpdateParallelData'
    { _updDescription = Nothing,
      _updName = pName_,
      _updParallelDataConfig = pParallelDataConfig_,
      _updClientToken = pClientToken_
    }

-- | A custom description for the parallel data resource in Amazon Translate.
updDescription :: Lens' UpdateParallelData (Maybe Text)
updDescription = lens _updDescription (\s a -> s {_updDescription = a})

-- | The name of the parallel data resource being updated.
updName :: Lens' UpdateParallelData Text
updName = lens _updName (\s a -> s {_updName = a})

-- | Specifies the format and S3 location of the parallel data input file.
updParallelDataConfig :: Lens' UpdateParallelData ParallelDataConfig
updParallelDataConfig = lens _updParallelDataConfig (\s a -> s {_updParallelDataConfig = a})

-- | A unique identifier for the request. This token is automatically generated when you use Amazon Translate through an AWS SDK.
updClientToken :: Lens' UpdateParallelData Text
updClientToken = lens _updClientToken (\s a -> s {_updClientToken = a})

instance AWSRequest UpdateParallelData where
  type Rs UpdateParallelData = UpdateParallelDataResponse
  request = postJSON translate
  response =
    receiveJSON
      ( \s h x ->
          UpdateParallelDataResponse'
            <$> (x .?> "Status")
            <*> (x .?> "Name")
            <*> (x .?> "LatestUpdateAttemptAt")
            <*> (x .?> "LatestUpdateAttemptStatus")
            <*> (pure (fromEnum s))
      )

instance Hashable UpdateParallelData

instance NFData UpdateParallelData

instance ToHeaders UpdateParallelData where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSShineFrontendService_20170701.UpdateParallelData" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateParallelData where
  toJSON UpdateParallelData' {..} =
    object
      ( catMaybes
          [ ("Description" .=) <$> _updDescription,
            Just ("Name" .= _updName),
            Just ("ParallelDataConfig" .= _updParallelDataConfig),
            Just ("ClientToken" .= _updClientToken)
          ]
      )

instance ToPath UpdateParallelData where
  toPath = const "/"

instance ToQuery UpdateParallelData where
  toQuery = const mempty

-- | /See:/ 'updateParallelDataResponse' smart constructor.
data UpdateParallelDataResponse = UpdateParallelDataResponse'
  { _updrsStatus ::
      !(Maybe ParallelDataStatus),
    _updrsName :: !(Maybe Text),
    _updrsLatestUpdateAttemptAt ::
      !(Maybe POSIX),
    _updrsLatestUpdateAttemptStatus ::
      !(Maybe ParallelDataStatus),
    _updrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateParallelDataResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'updrsStatus' - The status of the parallel data resource that you are attempting to update. Your update request is accepted only if this status is either @ACTIVE@ or @FAILED@ .
--
-- * 'updrsName' - The name of the parallel data resource being updated.
--
-- * 'updrsLatestUpdateAttemptAt' - The time that the most recent update was attempted.
--
-- * 'updrsLatestUpdateAttemptStatus' - The status of the parallel data update attempt. When the updated parallel data resource is ready for you to use, the status is @ACTIVE@ .
--
-- * 'updrsResponseStatus' - -- | The response status code.
updateParallelDataResponse ::
  -- | 'updrsResponseStatus'
  Int ->
  UpdateParallelDataResponse
updateParallelDataResponse pResponseStatus_ =
  UpdateParallelDataResponse'
    { _updrsStatus = Nothing,
      _updrsName = Nothing,
      _updrsLatestUpdateAttemptAt = Nothing,
      _updrsLatestUpdateAttemptStatus = Nothing,
      _updrsResponseStatus = pResponseStatus_
    }

-- | The status of the parallel data resource that you are attempting to update. Your update request is accepted only if this status is either @ACTIVE@ or @FAILED@ .
updrsStatus :: Lens' UpdateParallelDataResponse (Maybe ParallelDataStatus)
updrsStatus = lens _updrsStatus (\s a -> s {_updrsStatus = a})

-- | The name of the parallel data resource being updated.
updrsName :: Lens' UpdateParallelDataResponse (Maybe Text)
updrsName = lens _updrsName (\s a -> s {_updrsName = a})

-- | The time that the most recent update was attempted.
updrsLatestUpdateAttemptAt :: Lens' UpdateParallelDataResponse (Maybe UTCTime)
updrsLatestUpdateAttemptAt = lens _updrsLatestUpdateAttemptAt (\s a -> s {_updrsLatestUpdateAttemptAt = a}) . mapping _Time

-- | The status of the parallel data update attempt. When the updated parallel data resource is ready for you to use, the status is @ACTIVE@ .
updrsLatestUpdateAttemptStatus :: Lens' UpdateParallelDataResponse (Maybe ParallelDataStatus)
updrsLatestUpdateAttemptStatus = lens _updrsLatestUpdateAttemptStatus (\s a -> s {_updrsLatestUpdateAttemptStatus = a})

-- | -- | The response status code.
updrsResponseStatus :: Lens' UpdateParallelDataResponse Int
updrsResponseStatus = lens _updrsResponseStatus (\s a -> s {_updrsResponseStatus = a})

instance NFData UpdateParallelDataResponse
