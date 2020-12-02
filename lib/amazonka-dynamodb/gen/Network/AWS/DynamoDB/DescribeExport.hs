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
-- Module      : Network.AWS.DynamoDB.DescribeExport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an existing table export.
module Network.AWS.DynamoDB.DescribeExport
  ( -- * Creating a Request
    describeExport,
    DescribeExport,

    -- * Request Lenses
    deExportARN,

    -- * Destructuring the Response
    describeExportResponse,
    DescribeExportResponse,

    -- * Response Lenses
    deersExportDescription,
    deersResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeExport' smart constructor.
newtype DescribeExport = DescribeExport' {_deExportARN :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeExport' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deExportARN' - The Amazon Resource Name (ARN) associated with the export.
describeExport ::
  -- | 'deExportARN'
  Text ->
  DescribeExport
describeExport pExportARN_ =
  DescribeExport' {_deExportARN = pExportARN_}

-- | The Amazon Resource Name (ARN) associated with the export.
deExportARN :: Lens' DescribeExport Text
deExportARN = lens _deExportARN (\s a -> s {_deExportARN = a})

instance AWSRequest DescribeExport where
  type Rs DescribeExport = DescribeExportResponse
  request = postJSON dynamoDB
  response =
    receiveJSON
      ( \s h x ->
          DescribeExportResponse'
            <$> (x .?> "ExportDescription") <*> (pure (fromEnum s))
      )

instance Hashable DescribeExport

instance NFData DescribeExport

instance ToHeaders DescribeExport where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("DynamoDB_20120810.DescribeExport" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.0" :: ByteString)
          ]
      )

instance ToJSON DescribeExport where
  toJSON DescribeExport' {..} =
    object (catMaybes [Just ("ExportArn" .= _deExportARN)])

instance ToPath DescribeExport where
  toPath = const "/"

instance ToQuery DescribeExport where
  toQuery = const mempty

-- | /See:/ 'describeExportResponse' smart constructor.
data DescribeExportResponse = DescribeExportResponse'
  { _deersExportDescription ::
      !(Maybe ExportDescription),
    _deersResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeExportResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deersExportDescription' - Represents the properties of the export.
--
-- * 'deersResponseStatus' - -- | The response status code.
describeExportResponse ::
  -- | 'deersResponseStatus'
  Int ->
  DescribeExportResponse
describeExportResponse pResponseStatus_ =
  DescribeExportResponse'
    { _deersExportDescription = Nothing,
      _deersResponseStatus = pResponseStatus_
    }

-- | Represents the properties of the export.
deersExportDescription :: Lens' DescribeExportResponse (Maybe ExportDescription)
deersExportDescription = lens _deersExportDescription (\s a -> s {_deersExportDescription = a})

-- | -- | The response status code.
deersResponseStatus :: Lens' DescribeExportResponse Int
deersResponseStatus = lens _deersResponseStatus (\s a -> s {_deersResponseStatus = a})

instance NFData DescribeExportResponse
