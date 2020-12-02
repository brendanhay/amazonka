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
-- Module      : Network.AWS.Translate.CreateParallelData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a parallel data resource in Amazon Translate by importing an input file from Amazon S3. Parallel data files contain examples of source phrases and their translations from your translation memory. By adding parallel data, you can influence the style, tone, and word choice in your translation output.
module Network.AWS.Translate.CreateParallelData
  ( -- * Creating a Request
    createParallelData,
    CreateParallelData,

    -- * Request Lenses
    cpdEncryptionKey,
    cpdDescription,
    cpdName,
    cpdParallelDataConfig,
    cpdClientToken,

    -- * Destructuring the Response
    createParallelDataResponse,
    CreateParallelDataResponse,

    -- * Response Lenses
    cpdrsStatus,
    cpdrsName,
    cpdrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Translate.Types

-- | /See:/ 'createParallelData' smart constructor.
data CreateParallelData = CreateParallelData'
  { _cpdEncryptionKey ::
      !(Maybe EncryptionKey),
    _cpdDescription :: !(Maybe Text),
    _cpdName :: !Text,
    _cpdParallelDataConfig :: !ParallelDataConfig,
    _cpdClientToken :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateParallelData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpdEncryptionKey' - Undocumented member.
--
-- * 'cpdDescription' - A custom description for the parallel data resource in Amazon Translate.
--
-- * 'cpdName' - A custom name for the parallel data resource in Amazon Translate. You must assign a name that is unique in the account and region.
--
-- * 'cpdParallelDataConfig' - Specifies the format and S3 location of the parallel data input file.
--
-- * 'cpdClientToken' - A unique identifier for the request. This token is automatically generated when you use Amazon Translate through an AWS SDK.
createParallelData ::
  -- | 'cpdName'
  Text ->
  -- | 'cpdParallelDataConfig'
  ParallelDataConfig ->
  -- | 'cpdClientToken'
  Text ->
  CreateParallelData
createParallelData pName_ pParallelDataConfig_ pClientToken_ =
  CreateParallelData'
    { _cpdEncryptionKey = Nothing,
      _cpdDescription = Nothing,
      _cpdName = pName_,
      _cpdParallelDataConfig = pParallelDataConfig_,
      _cpdClientToken = pClientToken_
    }

-- | Undocumented member.
cpdEncryptionKey :: Lens' CreateParallelData (Maybe EncryptionKey)
cpdEncryptionKey = lens _cpdEncryptionKey (\s a -> s {_cpdEncryptionKey = a})

-- | A custom description for the parallel data resource in Amazon Translate.
cpdDescription :: Lens' CreateParallelData (Maybe Text)
cpdDescription = lens _cpdDescription (\s a -> s {_cpdDescription = a})

-- | A custom name for the parallel data resource in Amazon Translate. You must assign a name that is unique in the account and region.
cpdName :: Lens' CreateParallelData Text
cpdName = lens _cpdName (\s a -> s {_cpdName = a})

-- | Specifies the format and S3 location of the parallel data input file.
cpdParallelDataConfig :: Lens' CreateParallelData ParallelDataConfig
cpdParallelDataConfig = lens _cpdParallelDataConfig (\s a -> s {_cpdParallelDataConfig = a})

-- | A unique identifier for the request. This token is automatically generated when you use Amazon Translate through an AWS SDK.
cpdClientToken :: Lens' CreateParallelData Text
cpdClientToken = lens _cpdClientToken (\s a -> s {_cpdClientToken = a})

instance AWSRequest CreateParallelData where
  type Rs CreateParallelData = CreateParallelDataResponse
  request = postJSON translate
  response =
    receiveJSON
      ( \s h x ->
          CreateParallelDataResponse'
            <$> (x .?> "Status") <*> (x .?> "Name") <*> (pure (fromEnum s))
      )

instance Hashable CreateParallelData

instance NFData CreateParallelData

instance ToHeaders CreateParallelData where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSShineFrontendService_20170701.CreateParallelData" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateParallelData where
  toJSON CreateParallelData' {..} =
    object
      ( catMaybes
          [ ("EncryptionKey" .=) <$> _cpdEncryptionKey,
            ("Description" .=) <$> _cpdDescription,
            Just ("Name" .= _cpdName),
            Just ("ParallelDataConfig" .= _cpdParallelDataConfig),
            Just ("ClientToken" .= _cpdClientToken)
          ]
      )

instance ToPath CreateParallelData where
  toPath = const "/"

instance ToQuery CreateParallelData where
  toQuery = const mempty

-- | /See:/ 'createParallelDataResponse' smart constructor.
data CreateParallelDataResponse = CreateParallelDataResponse'
  { _cpdrsStatus ::
      !(Maybe ParallelDataStatus),
    _cpdrsName :: !(Maybe Text),
    _cpdrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateParallelDataResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpdrsStatus' - The status of the parallel data resource. When the resource is ready for you to use, the status is @ACTIVE@ .
--
-- * 'cpdrsName' - The custom name that you assigned to the parallel data resource.
--
-- * 'cpdrsResponseStatus' - -- | The response status code.
createParallelDataResponse ::
  -- | 'cpdrsResponseStatus'
  Int ->
  CreateParallelDataResponse
createParallelDataResponse pResponseStatus_ =
  CreateParallelDataResponse'
    { _cpdrsStatus = Nothing,
      _cpdrsName = Nothing,
      _cpdrsResponseStatus = pResponseStatus_
    }

-- | The status of the parallel data resource. When the resource is ready for you to use, the status is @ACTIVE@ .
cpdrsStatus :: Lens' CreateParallelDataResponse (Maybe ParallelDataStatus)
cpdrsStatus = lens _cpdrsStatus (\s a -> s {_cpdrsStatus = a})

-- | The custom name that you assigned to the parallel data resource.
cpdrsName :: Lens' CreateParallelDataResponse (Maybe Text)
cpdrsName = lens _cpdrsName (\s a -> s {_cpdrsName = a})

-- | -- | The response status code.
cpdrsResponseStatus :: Lens' CreateParallelDataResponse Int
cpdrsResponseStatus = lens _cpdrsResponseStatus (\s a -> s {_cpdrsResponseStatus = a})

instance NFData CreateParallelDataResponse
