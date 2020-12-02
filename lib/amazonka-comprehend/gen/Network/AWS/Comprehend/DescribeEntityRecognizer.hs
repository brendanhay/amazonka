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
-- Module      : Network.AWS.Comprehend.DescribeEntityRecognizer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides details about an entity recognizer including status, S3 buckets containing training data, recognizer metadata, metrics, and so on.
module Network.AWS.Comprehend.DescribeEntityRecognizer
  ( -- * Creating a Request
    describeEntityRecognizer,
    DescribeEntityRecognizer,

    -- * Request Lenses
    dEntityRecognizerARN,

    -- * Destructuring the Response
    describeEntityRecognizerResponse,
    DescribeEntityRecognizerResponse,

    -- * Response Lenses
    drsEntityRecognizerProperties,
    drsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeEntityRecognizer' smart constructor.
newtype DescribeEntityRecognizer = DescribeEntityRecognizer'
  { _dEntityRecognizerARN ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeEntityRecognizer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dEntityRecognizerARN' - The Amazon Resource Name (ARN) that identifies the entity recognizer.
describeEntityRecognizer ::
  -- | 'dEntityRecognizerARN'
  Text ->
  DescribeEntityRecognizer
describeEntityRecognizer pEntityRecognizerARN_ =
  DescribeEntityRecognizer'
    { _dEntityRecognizerARN =
        pEntityRecognizerARN_
    }

-- | The Amazon Resource Name (ARN) that identifies the entity recognizer.
dEntityRecognizerARN :: Lens' DescribeEntityRecognizer Text
dEntityRecognizerARN = lens _dEntityRecognizerARN (\s a -> s {_dEntityRecognizerARN = a})

instance AWSRequest DescribeEntityRecognizer where
  type Rs DescribeEntityRecognizer = DescribeEntityRecognizerResponse
  request = postJSON comprehend
  response =
    receiveJSON
      ( \s h x ->
          DescribeEntityRecognizerResponse'
            <$> (x .?> "EntityRecognizerProperties") <*> (pure (fromEnum s))
      )

instance Hashable DescribeEntityRecognizer

instance NFData DescribeEntityRecognizer

instance ToHeaders DescribeEntityRecognizer where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Comprehend_20171127.DescribeEntityRecognizer" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeEntityRecognizer where
  toJSON DescribeEntityRecognizer' {..} =
    object
      (catMaybes [Just ("EntityRecognizerArn" .= _dEntityRecognizerARN)])

instance ToPath DescribeEntityRecognizer where
  toPath = const "/"

instance ToQuery DescribeEntityRecognizer where
  toQuery = const mempty

-- | /See:/ 'describeEntityRecognizerResponse' smart constructor.
data DescribeEntityRecognizerResponse = DescribeEntityRecognizerResponse'
  { _drsEntityRecognizerProperties ::
      !( Maybe
           EntityRecognizerProperties
       ),
    _drsResponseStatus ::
      !Int
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeEntityRecognizerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsEntityRecognizerProperties' - Describes information associated with an entity recognizer.
--
-- * 'drsResponseStatus' - -- | The response status code.
describeEntityRecognizerResponse ::
  -- | 'drsResponseStatus'
  Int ->
  DescribeEntityRecognizerResponse
describeEntityRecognizerResponse pResponseStatus_ =
  DescribeEntityRecognizerResponse'
    { _drsEntityRecognizerProperties =
        Nothing,
      _drsResponseStatus = pResponseStatus_
    }

-- | Describes information associated with an entity recognizer.
drsEntityRecognizerProperties :: Lens' DescribeEntityRecognizerResponse (Maybe EntityRecognizerProperties)
drsEntityRecognizerProperties = lens _drsEntityRecognizerProperties (\s a -> s {_drsEntityRecognizerProperties = a})

-- | -- | The response status code.
drsResponseStatus :: Lens' DescribeEntityRecognizerResponse Int
drsResponseStatus = lens _drsResponseStatus (\s a -> s {_drsResponseStatus = a})

instance NFData DescribeEntityRecognizerResponse
