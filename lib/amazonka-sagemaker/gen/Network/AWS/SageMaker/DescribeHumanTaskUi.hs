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
-- Module      : Network.AWS.SageMaker.DescribeHumanTaskUi
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the requested human task user interface (worker task template).
module Network.AWS.SageMaker.DescribeHumanTaskUi
  ( -- * Creating a Request
    describeHumanTaskUi,
    DescribeHumanTaskUi,

    -- * Request Lenses
    dHumanTaskUiName,

    -- * Destructuring the Response
    describeHumanTaskUiResponse,
    DescribeHumanTaskUiResponse,

    -- * Response Lenses
    dhtuhrsHumanTaskUiStatus,
    dhtuhrsResponseStatus,
    dhtuhrsHumanTaskUiARN,
    dhtuhrsHumanTaskUiName,
    dhtuhrsCreationTime,
    dhtuhrsUiTemplate,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'describeHumanTaskUi' smart constructor.
newtype DescribeHumanTaskUi = DescribeHumanTaskUi'
  { _dHumanTaskUiName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeHumanTaskUi' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dHumanTaskUiName' - The name of the human task user interface (worker task template) you want information about.
describeHumanTaskUi ::
  -- | 'dHumanTaskUiName'
  Text ->
  DescribeHumanTaskUi
describeHumanTaskUi pHumanTaskUiName_ =
  DescribeHumanTaskUi' {_dHumanTaskUiName = pHumanTaskUiName_}

-- | The name of the human task user interface (worker task template) you want information about.
dHumanTaskUiName :: Lens' DescribeHumanTaskUi Text
dHumanTaskUiName = lens _dHumanTaskUiName (\s a -> s {_dHumanTaskUiName = a})

instance AWSRequest DescribeHumanTaskUi where
  type Rs DescribeHumanTaskUi = DescribeHumanTaskUiResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          DescribeHumanTaskUiResponse'
            <$> (x .?> "HumanTaskUiStatus")
            <*> (pure (fromEnum s))
            <*> (x .:> "HumanTaskUiArn")
            <*> (x .:> "HumanTaskUiName")
            <*> (x .:> "CreationTime")
            <*> (x .:> "UiTemplate")
      )

instance Hashable DescribeHumanTaskUi

instance NFData DescribeHumanTaskUi

instance ToHeaders DescribeHumanTaskUi where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.DescribeHumanTaskUi" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeHumanTaskUi where
  toJSON DescribeHumanTaskUi' {..} =
    object
      (catMaybes [Just ("HumanTaskUiName" .= _dHumanTaskUiName)])

instance ToPath DescribeHumanTaskUi where
  toPath = const "/"

instance ToQuery DescribeHumanTaskUi where
  toQuery = const mempty

-- | /See:/ 'describeHumanTaskUiResponse' smart constructor.
data DescribeHumanTaskUiResponse = DescribeHumanTaskUiResponse'
  { _dhtuhrsHumanTaskUiStatus ::
      !(Maybe HumanTaskUiStatus),
    _dhtuhrsResponseStatus :: !Int,
    _dhtuhrsHumanTaskUiARN :: !Text,
    _dhtuhrsHumanTaskUiName :: !Text,
    _dhtuhrsCreationTime :: !POSIX,
    _dhtuhrsUiTemplate ::
      !UiTemplateInfo
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeHumanTaskUiResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhtuhrsHumanTaskUiStatus' - The status of the human task user interface (worker task template). Valid values are listed below.
--
-- * 'dhtuhrsResponseStatus' - -- | The response status code.
--
-- * 'dhtuhrsHumanTaskUiARN' - The Amazon Resource Name (ARN) of the human task user interface (worker task template).
--
-- * 'dhtuhrsHumanTaskUiName' - The name of the human task user interface (worker task template).
--
-- * 'dhtuhrsCreationTime' - The timestamp when the human task user interface was created.
--
-- * 'dhtuhrsUiTemplate' - Undocumented member.
describeHumanTaskUiResponse ::
  -- | 'dhtuhrsResponseStatus'
  Int ->
  -- | 'dhtuhrsHumanTaskUiARN'
  Text ->
  -- | 'dhtuhrsHumanTaskUiName'
  Text ->
  -- | 'dhtuhrsCreationTime'
  UTCTime ->
  -- | 'dhtuhrsUiTemplate'
  UiTemplateInfo ->
  DescribeHumanTaskUiResponse
describeHumanTaskUiResponse
  pResponseStatus_
  pHumanTaskUiARN_
  pHumanTaskUiName_
  pCreationTime_
  pUiTemplate_ =
    DescribeHumanTaskUiResponse'
      { _dhtuhrsHumanTaskUiStatus = Nothing,
        _dhtuhrsResponseStatus = pResponseStatus_,
        _dhtuhrsHumanTaskUiARN = pHumanTaskUiARN_,
        _dhtuhrsHumanTaskUiName = pHumanTaskUiName_,
        _dhtuhrsCreationTime = _Time # pCreationTime_,
        _dhtuhrsUiTemplate = pUiTemplate_
      }

-- | The status of the human task user interface (worker task template). Valid values are listed below.
dhtuhrsHumanTaskUiStatus :: Lens' DescribeHumanTaskUiResponse (Maybe HumanTaskUiStatus)
dhtuhrsHumanTaskUiStatus = lens _dhtuhrsHumanTaskUiStatus (\s a -> s {_dhtuhrsHumanTaskUiStatus = a})

-- | -- | The response status code.
dhtuhrsResponseStatus :: Lens' DescribeHumanTaskUiResponse Int
dhtuhrsResponseStatus = lens _dhtuhrsResponseStatus (\s a -> s {_dhtuhrsResponseStatus = a})

-- | The Amazon Resource Name (ARN) of the human task user interface (worker task template).
dhtuhrsHumanTaskUiARN :: Lens' DescribeHumanTaskUiResponse Text
dhtuhrsHumanTaskUiARN = lens _dhtuhrsHumanTaskUiARN (\s a -> s {_dhtuhrsHumanTaskUiARN = a})

-- | The name of the human task user interface (worker task template).
dhtuhrsHumanTaskUiName :: Lens' DescribeHumanTaskUiResponse Text
dhtuhrsHumanTaskUiName = lens _dhtuhrsHumanTaskUiName (\s a -> s {_dhtuhrsHumanTaskUiName = a})

-- | The timestamp when the human task user interface was created.
dhtuhrsCreationTime :: Lens' DescribeHumanTaskUiResponse UTCTime
dhtuhrsCreationTime = lens _dhtuhrsCreationTime (\s a -> s {_dhtuhrsCreationTime = a}) . _Time

-- | Undocumented member.
dhtuhrsUiTemplate :: Lens' DescribeHumanTaskUiResponse UiTemplateInfo
dhtuhrsUiTemplate = lens _dhtuhrsUiTemplate (\s a -> s {_dhtuhrsUiTemplate = a})

instance NFData DescribeHumanTaskUiResponse
