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
-- Module      : Network.AWS.DirectConnect.DescribeLocations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the AWS Direct Connect locations in the current AWS Region. These are the locations that can be selected when calling 'CreateConnection' or 'CreateInterconnect' .
module Network.AWS.DirectConnect.DescribeLocations
  ( -- * Creating a Request
    describeLocations,
    DescribeLocations,

    -- * Destructuring the Response
    describeLocationsResponse,
    DescribeLocationsResponse,

    -- * Response Lenses
    dlsrsLocations,
    dlsrsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeLocations' smart constructor.
data DescribeLocations = DescribeLocations'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeLocations' with the minimum fields required to make a request.
describeLocations ::
  DescribeLocations
describeLocations = DescribeLocations'

instance AWSRequest DescribeLocations where
  type Rs DescribeLocations = DescribeLocationsResponse
  request = postJSON directConnect
  response =
    receiveJSON
      ( \s h x ->
          DescribeLocationsResponse'
            <$> (x .?> "locations" .!@ mempty) <*> (pure (fromEnum s))
      )

instance Hashable DescribeLocations

instance NFData DescribeLocations

instance ToHeaders DescribeLocations where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("OvertureService.DescribeLocations" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeLocations where
  toJSON = const (Object mempty)

instance ToPath DescribeLocations where
  toPath = const "/"

instance ToQuery DescribeLocations where
  toQuery = const mempty

-- | /See:/ 'describeLocationsResponse' smart constructor.
data DescribeLocationsResponse = DescribeLocationsResponse'
  { _dlsrsLocations ::
      !(Maybe [Location]),
    _dlsrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeLocationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlsrsLocations' - The locations.
--
-- * 'dlsrsResponseStatus' - -- | The response status code.
describeLocationsResponse ::
  -- | 'dlsrsResponseStatus'
  Int ->
  DescribeLocationsResponse
describeLocationsResponse pResponseStatus_ =
  DescribeLocationsResponse'
    { _dlsrsLocations = Nothing,
      _dlsrsResponseStatus = pResponseStatus_
    }

-- | The locations.
dlsrsLocations :: Lens' DescribeLocationsResponse [Location]
dlsrsLocations = lens _dlsrsLocations (\s a -> s {_dlsrsLocations = a}) . _Default . _Coerce

-- | -- | The response status code.
dlsrsResponseStatus :: Lens' DescribeLocationsResponse Int
dlsrsResponseStatus = lens _dlsrsResponseStatus (\s a -> s {_dlsrsResponseStatus = a})

instance NFData DescribeLocationsResponse
