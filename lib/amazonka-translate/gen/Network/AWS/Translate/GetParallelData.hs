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
-- Module      : Network.AWS.Translate.GetParallelData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about a parallel data resource.
module Network.AWS.Translate.GetParallelData
  ( -- * Creating a Request
    getParallelData,
    GetParallelData,

    -- * Request Lenses
    gpdName,

    -- * Destructuring the Response
    getParallelDataResponse,
    GetParallelDataResponse,

    -- * Response Lenses
    gpdrsParallelDataProperties,
    gpdrsDataLocation,
    gpdrsAuxiliaryDataLocation,
    gpdrsLatestUpdateAttemptAuxiliaryDataLocation,
    gpdrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Translate.Types

-- | /See:/ 'getParallelData' smart constructor.
newtype GetParallelData = GetParallelData' {_gpdName :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetParallelData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpdName' - The name of the parallel data resource that is being retrieved.
getParallelData ::
  -- | 'gpdName'
  Text ->
  GetParallelData
getParallelData pName_ = GetParallelData' {_gpdName = pName_}

-- | The name of the parallel data resource that is being retrieved.
gpdName :: Lens' GetParallelData Text
gpdName = lens _gpdName (\s a -> s {_gpdName = a})

instance AWSRequest GetParallelData where
  type Rs GetParallelData = GetParallelDataResponse
  request = postJSON translate
  response =
    receiveJSON
      ( \s h x ->
          GetParallelDataResponse'
            <$> (x .?> "ParallelDataProperties")
            <*> (x .?> "DataLocation")
            <*> (x .?> "AuxiliaryDataLocation")
            <*> (x .?> "LatestUpdateAttemptAuxiliaryDataLocation")
            <*> (pure (fromEnum s))
      )

instance Hashable GetParallelData

instance NFData GetParallelData

instance ToHeaders GetParallelData where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSShineFrontendService_20170701.GetParallelData" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetParallelData where
  toJSON GetParallelData' {..} =
    object (catMaybes [Just ("Name" .= _gpdName)])

instance ToPath GetParallelData where
  toPath = const "/"

instance ToQuery GetParallelData where
  toQuery = const mempty

-- | /See:/ 'getParallelDataResponse' smart constructor.
data GetParallelDataResponse = GetParallelDataResponse'
  { _gpdrsParallelDataProperties ::
      !(Maybe ParallelDataProperties),
    _gpdrsDataLocation ::
      !(Maybe ParallelDataDataLocation),
    _gpdrsAuxiliaryDataLocation ::
      !(Maybe ParallelDataDataLocation),
    _gpdrsLatestUpdateAttemptAuxiliaryDataLocation ::
      !(Maybe ParallelDataDataLocation),
    _gpdrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetParallelDataResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpdrsParallelDataProperties' - The properties of the parallel data resource that is being retrieved.
--
-- * 'gpdrsDataLocation' - The location of the most recent parallel data input file that was successfully imported into Amazon Translate. The location is returned as a presigned URL that has a 30 minute expiration.
--
-- * 'gpdrsAuxiliaryDataLocation' - The Amazon S3 location of a file that provides any errors or warnings that were produced by your input file. This file was created when Amazon Translate attempted to create a parallel data resource. The location is returned as a presigned URL to that has a 30 minute expiration.
--
-- * 'gpdrsLatestUpdateAttemptAuxiliaryDataLocation' - The Amazon S3 location of a file that provides any errors or warnings that were produced by your input file. This file was created when Amazon Translate attempted to update a parallel data resource. The location is returned as a presigned URL to that has a 30 minute expiration.
--
-- * 'gpdrsResponseStatus' - -- | The response status code.
getParallelDataResponse ::
  -- | 'gpdrsResponseStatus'
  Int ->
  GetParallelDataResponse
getParallelDataResponse pResponseStatus_ =
  GetParallelDataResponse'
    { _gpdrsParallelDataProperties = Nothing,
      _gpdrsDataLocation = Nothing,
      _gpdrsAuxiliaryDataLocation = Nothing,
      _gpdrsLatestUpdateAttemptAuxiliaryDataLocation = Nothing,
      _gpdrsResponseStatus = pResponseStatus_
    }

-- | The properties of the parallel data resource that is being retrieved.
gpdrsParallelDataProperties :: Lens' GetParallelDataResponse (Maybe ParallelDataProperties)
gpdrsParallelDataProperties = lens _gpdrsParallelDataProperties (\s a -> s {_gpdrsParallelDataProperties = a})

-- | The location of the most recent parallel data input file that was successfully imported into Amazon Translate. The location is returned as a presigned URL that has a 30 minute expiration.
gpdrsDataLocation :: Lens' GetParallelDataResponse (Maybe ParallelDataDataLocation)
gpdrsDataLocation = lens _gpdrsDataLocation (\s a -> s {_gpdrsDataLocation = a})

-- | The Amazon S3 location of a file that provides any errors or warnings that were produced by your input file. This file was created when Amazon Translate attempted to create a parallel data resource. The location is returned as a presigned URL to that has a 30 minute expiration.
gpdrsAuxiliaryDataLocation :: Lens' GetParallelDataResponse (Maybe ParallelDataDataLocation)
gpdrsAuxiliaryDataLocation = lens _gpdrsAuxiliaryDataLocation (\s a -> s {_gpdrsAuxiliaryDataLocation = a})

-- | The Amazon S3 location of a file that provides any errors or warnings that were produced by your input file. This file was created when Amazon Translate attempted to update a parallel data resource. The location is returned as a presigned URL to that has a 30 minute expiration.
gpdrsLatestUpdateAttemptAuxiliaryDataLocation :: Lens' GetParallelDataResponse (Maybe ParallelDataDataLocation)
gpdrsLatestUpdateAttemptAuxiliaryDataLocation = lens _gpdrsLatestUpdateAttemptAuxiliaryDataLocation (\s a -> s {_gpdrsLatestUpdateAttemptAuxiliaryDataLocation = a})

-- | -- | The response status code.
gpdrsResponseStatus :: Lens' GetParallelDataResponse Int
gpdrsResponseStatus = lens _gpdrsResponseStatus (\s a -> s {_gpdrsResponseStatus = a})

instance NFData GetParallelDataResponse
