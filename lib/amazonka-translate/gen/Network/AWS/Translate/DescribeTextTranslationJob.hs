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
-- Module      : Network.AWS.Translate.DescribeTextTranslationJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with an asycnhronous batch translation job including name, ID, status, source and target languages, input/output S3 buckets, and so on.
module Network.AWS.Translate.DescribeTextTranslationJob
  ( -- * Creating a Request
    describeTextTranslationJob,
    DescribeTextTranslationJob,

    -- * Request Lenses
    dttjJobId,

    -- * Destructuring the Response
    describeTextTranslationJobResponse,
    DescribeTextTranslationJobResponse,

    -- * Response Lenses
    dttjrsTextTranslationJobProperties,
    dttjrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Translate.Types

-- | /See:/ 'describeTextTranslationJob' smart constructor.
newtype DescribeTextTranslationJob = DescribeTextTranslationJob'
  { _dttjJobId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeTextTranslationJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dttjJobId' - The identifier that Amazon Translate generated for the job. The 'StartTextTranslationJob' operation returns this identifier in its response.
describeTextTranslationJob ::
  -- | 'dttjJobId'
  Text ->
  DescribeTextTranslationJob
describeTextTranslationJob pJobId_ =
  DescribeTextTranslationJob' {_dttjJobId = pJobId_}

-- | The identifier that Amazon Translate generated for the job. The 'StartTextTranslationJob' operation returns this identifier in its response.
dttjJobId :: Lens' DescribeTextTranslationJob Text
dttjJobId = lens _dttjJobId (\s a -> s {_dttjJobId = a})

instance AWSRequest DescribeTextTranslationJob where
  type
    Rs DescribeTextTranslationJob =
      DescribeTextTranslationJobResponse
  request = postJSON translate
  response =
    receiveJSON
      ( \s h x ->
          DescribeTextTranslationJobResponse'
            <$> (x .?> "TextTranslationJobProperties") <*> (pure (fromEnum s))
      )

instance Hashable DescribeTextTranslationJob

instance NFData DescribeTextTranslationJob

instance ToHeaders DescribeTextTranslationJob where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSShineFrontendService_20170701.DescribeTextTranslationJob" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeTextTranslationJob where
  toJSON DescribeTextTranslationJob' {..} =
    object (catMaybes [Just ("JobId" .= _dttjJobId)])

instance ToPath DescribeTextTranslationJob where
  toPath = const "/"

instance ToQuery DescribeTextTranslationJob where
  toQuery = const mempty

-- | /See:/ 'describeTextTranslationJobResponse' smart constructor.
data DescribeTextTranslationJobResponse = DescribeTextTranslationJobResponse'
  { _dttjrsTextTranslationJobProperties ::
      !( Maybe
           TextTranslationJobProperties
       ),
    _dttjrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeTextTranslationJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dttjrsTextTranslationJobProperties' - An object that contains the properties associated with an asynchronous batch translation job.
--
-- * 'dttjrsResponseStatus' - -- | The response status code.
describeTextTranslationJobResponse ::
  -- | 'dttjrsResponseStatus'
  Int ->
  DescribeTextTranslationJobResponse
describeTextTranslationJobResponse pResponseStatus_ =
  DescribeTextTranslationJobResponse'
    { _dttjrsTextTranslationJobProperties =
        Nothing,
      _dttjrsResponseStatus = pResponseStatus_
    }

-- | An object that contains the properties associated with an asynchronous batch translation job.
dttjrsTextTranslationJobProperties :: Lens' DescribeTextTranslationJobResponse (Maybe TextTranslationJobProperties)
dttjrsTextTranslationJobProperties = lens _dttjrsTextTranslationJobProperties (\s a -> s {_dttjrsTextTranslationJobProperties = a})

-- | -- | The response status code.
dttjrsResponseStatus :: Lens' DescribeTextTranslationJobResponse Int
dttjrsResponseStatus = lens _dttjrsResponseStatus (\s a -> s {_dttjrsResponseStatus = a})

instance NFData DescribeTextTranslationJobResponse
