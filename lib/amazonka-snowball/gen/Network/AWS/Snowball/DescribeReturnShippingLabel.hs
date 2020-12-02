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
-- Module      : Network.AWS.Snowball.DescribeReturnShippingLabel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Information on the shipping label of a Snow device that is being returned to AWS.
module Network.AWS.Snowball.DescribeReturnShippingLabel
  ( -- * Creating a Request
    describeReturnShippingLabel,
    DescribeReturnShippingLabel,

    -- * Request Lenses
    drslJobId,

    -- * Destructuring the Response
    describeReturnShippingLabelResponse,
    DescribeReturnShippingLabelResponse,

    -- * Response Lenses
    drslrsStatus,
    drslrsExpirationDate,
    drslrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Snowball.Types

-- | /See:/ 'describeReturnShippingLabel' smart constructor.
newtype DescribeReturnShippingLabel = DescribeReturnShippingLabel'
  { _drslJobId ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeReturnShippingLabel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drslJobId' - The automatically generated ID for a job, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
describeReturnShippingLabel ::
  DescribeReturnShippingLabel
describeReturnShippingLabel =
  DescribeReturnShippingLabel' {_drslJobId = Nothing}

-- | The automatically generated ID for a job, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
drslJobId :: Lens' DescribeReturnShippingLabel (Maybe Text)
drslJobId = lens _drslJobId (\s a -> s {_drslJobId = a})

instance AWSRequest DescribeReturnShippingLabel where
  type
    Rs DescribeReturnShippingLabel =
      DescribeReturnShippingLabelResponse
  request = postJSON snowball
  response =
    receiveJSON
      ( \s h x ->
          DescribeReturnShippingLabelResponse'
            <$> (x .?> "Status")
            <*> (x .?> "ExpirationDate")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeReturnShippingLabel

instance NFData DescribeReturnShippingLabel

instance ToHeaders DescribeReturnShippingLabel where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSIESnowballJobManagementService.DescribeReturnShippingLabel" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeReturnShippingLabel where
  toJSON DescribeReturnShippingLabel' {..} =
    object (catMaybes [("JobId" .=) <$> _drslJobId])

instance ToPath DescribeReturnShippingLabel where
  toPath = const "/"

instance ToQuery DescribeReturnShippingLabel where
  toQuery = const mempty

-- | /See:/ 'describeReturnShippingLabelResponse' smart constructor.
data DescribeReturnShippingLabelResponse = DescribeReturnShippingLabelResponse'
  { _drslrsStatus ::
      !( Maybe
           ShippingLabelStatus
       ),
    _drslrsExpirationDate ::
      !(Maybe POSIX),
    _drslrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeReturnShippingLabelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drslrsStatus' - The status information of the task on a Snow device that is being returned to AWS.
--
-- * 'drslrsExpirationDate' - The expiration date of the current return shipping label.
--
-- * 'drslrsResponseStatus' - -- | The response status code.
describeReturnShippingLabelResponse ::
  -- | 'drslrsResponseStatus'
  Int ->
  DescribeReturnShippingLabelResponse
describeReturnShippingLabelResponse pResponseStatus_ =
  DescribeReturnShippingLabelResponse'
    { _drslrsStatus = Nothing,
      _drslrsExpirationDate = Nothing,
      _drslrsResponseStatus = pResponseStatus_
    }

-- | The status information of the task on a Snow device that is being returned to AWS.
drslrsStatus :: Lens' DescribeReturnShippingLabelResponse (Maybe ShippingLabelStatus)
drslrsStatus = lens _drslrsStatus (\s a -> s {_drslrsStatus = a})

-- | The expiration date of the current return shipping label.
drslrsExpirationDate :: Lens' DescribeReturnShippingLabelResponse (Maybe UTCTime)
drslrsExpirationDate = lens _drslrsExpirationDate (\s a -> s {_drslrsExpirationDate = a}) . mapping _Time

-- | -- | The response status code.
drslrsResponseStatus :: Lens' DescribeReturnShippingLabelResponse Int
drslrsResponseStatus = lens _drslrsResponseStatus (\s a -> s {_drslrsResponseStatus = a})

instance NFData DescribeReturnShippingLabelResponse
