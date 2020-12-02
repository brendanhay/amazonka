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
-- Module      : Network.AWS.Transcribe.DescribeLanguageModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a single custom language model. Use this information to see details about the language model in your AWS account. You can also see whether the base language model used to create your custom language model has been updated. If Amazon Transcribe has updated the base model, you can create a new custom language model using the updated base model. If the language model wasn't created, you can use this operation to understand why Amazon Transcribe couldn't create it.
module Network.AWS.Transcribe.DescribeLanguageModel
  ( -- * Creating a Request
    describeLanguageModel,
    DescribeLanguageModel,

    -- * Request Lenses
    dModelName,

    -- * Destructuring the Response
    describeLanguageModelResponse,
    DescribeLanguageModelResponse,

    -- * Response Lenses
    dlmrsLanguageModel,
    dlmrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'describeLanguageModel' smart constructor.
newtype DescribeLanguageModel = DescribeLanguageModel'
  { _dModelName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeLanguageModel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dModelName' - The name of the custom language model you submit to get more information.
describeLanguageModel ::
  -- | 'dModelName'
  Text ->
  DescribeLanguageModel
describeLanguageModel pModelName_ =
  DescribeLanguageModel' {_dModelName = pModelName_}

-- | The name of the custom language model you submit to get more information.
dModelName :: Lens' DescribeLanguageModel Text
dModelName = lens _dModelName (\s a -> s {_dModelName = a})

instance AWSRequest DescribeLanguageModel where
  type Rs DescribeLanguageModel = DescribeLanguageModelResponse
  request = postJSON transcribe
  response =
    receiveJSON
      ( \s h x ->
          DescribeLanguageModelResponse'
            <$> (x .?> "LanguageModel") <*> (pure (fromEnum s))
      )

instance Hashable DescribeLanguageModel

instance NFData DescribeLanguageModel

instance ToHeaders DescribeLanguageModel where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Transcribe.DescribeLanguageModel" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeLanguageModel where
  toJSON DescribeLanguageModel' {..} =
    object (catMaybes [Just ("ModelName" .= _dModelName)])

instance ToPath DescribeLanguageModel where
  toPath = const "/"

instance ToQuery DescribeLanguageModel where
  toQuery = const mempty

-- | /See:/ 'describeLanguageModelResponse' smart constructor.
data DescribeLanguageModelResponse = DescribeLanguageModelResponse'
  { _dlmrsLanguageModel ::
      !(Maybe LanguageModel),
    _dlmrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeLanguageModelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlmrsLanguageModel' - The name of the custom language model you requested more information about.
--
-- * 'dlmrsResponseStatus' - -- | The response status code.
describeLanguageModelResponse ::
  -- | 'dlmrsResponseStatus'
  Int ->
  DescribeLanguageModelResponse
describeLanguageModelResponse pResponseStatus_ =
  DescribeLanguageModelResponse'
    { _dlmrsLanguageModel = Nothing,
      _dlmrsResponseStatus = pResponseStatus_
    }

-- | The name of the custom language model you requested more information about.
dlmrsLanguageModel :: Lens' DescribeLanguageModelResponse (Maybe LanguageModel)
dlmrsLanguageModel = lens _dlmrsLanguageModel (\s a -> s {_dlmrsLanguageModel = a})

-- | -- | The response status code.
dlmrsResponseStatus :: Lens' DescribeLanguageModelResponse Int
dlmrsResponseStatus = lens _dlmrsResponseStatus (\s a -> s {_dlmrsResponseStatus = a})

instance NFData DescribeLanguageModelResponse
