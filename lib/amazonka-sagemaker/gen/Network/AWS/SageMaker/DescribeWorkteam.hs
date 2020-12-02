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
-- Module      : Network.AWS.SageMaker.DescribeWorkteam
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specific work team. You can see information such as the create date, the last updated date, membership information, and the work team's Amazon Resource Name (ARN).
module Network.AWS.SageMaker.DescribeWorkteam
  ( -- * Creating a Request
    describeWorkteam,
    DescribeWorkteam,

    -- * Request Lenses
    dWorkteamName,

    -- * Destructuring the Response
    describeWorkteamResponse,
    DescribeWorkteamResponse,

    -- * Response Lenses
    dwwrsResponseStatus,
    dwwrsWorkteam,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'describeWorkteam' smart constructor.
newtype DescribeWorkteam = DescribeWorkteam'
  { _dWorkteamName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeWorkteam' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dWorkteamName' - The name of the work team to return a description of.
describeWorkteam ::
  -- | 'dWorkteamName'
  Text ->
  DescribeWorkteam
describeWorkteam pWorkteamName_ =
  DescribeWorkteam' {_dWorkteamName = pWorkteamName_}

-- | The name of the work team to return a description of.
dWorkteamName :: Lens' DescribeWorkteam Text
dWorkteamName = lens _dWorkteamName (\s a -> s {_dWorkteamName = a})

instance AWSRequest DescribeWorkteam where
  type Rs DescribeWorkteam = DescribeWorkteamResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          DescribeWorkteamResponse'
            <$> (pure (fromEnum s)) <*> (x .:> "Workteam")
      )

instance Hashable DescribeWorkteam

instance NFData DescribeWorkteam

instance ToHeaders DescribeWorkteam where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.DescribeWorkteam" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeWorkteam where
  toJSON DescribeWorkteam' {..} =
    object (catMaybes [Just ("WorkteamName" .= _dWorkteamName)])

instance ToPath DescribeWorkteam where
  toPath = const "/"

instance ToQuery DescribeWorkteam where
  toQuery = const mempty

-- | /See:/ 'describeWorkteamResponse' smart constructor.
data DescribeWorkteamResponse = DescribeWorkteamResponse'
  { _dwwrsResponseStatus ::
      !Int,
    _dwwrsWorkteam :: !Workteam
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeWorkteamResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dwwrsResponseStatus' - -- | The response status code.
--
-- * 'dwwrsWorkteam' - A @Workteam@ instance that contains information about the work team.
describeWorkteamResponse ::
  -- | 'dwwrsResponseStatus'
  Int ->
  -- | 'dwwrsWorkteam'
  Workteam ->
  DescribeWorkteamResponse
describeWorkteamResponse pResponseStatus_ pWorkteam_ =
  DescribeWorkteamResponse'
    { _dwwrsResponseStatus =
        pResponseStatus_,
      _dwwrsWorkteam = pWorkteam_
    }

-- | -- | The response status code.
dwwrsResponseStatus :: Lens' DescribeWorkteamResponse Int
dwwrsResponseStatus = lens _dwwrsResponseStatus (\s a -> s {_dwwrsResponseStatus = a})

-- | A @Workteam@ instance that contains information about the work team.
dwwrsWorkteam :: Lens' DescribeWorkteamResponse Workteam
dwwrsWorkteam = lens _dwwrsWorkteam (\s a -> s {_dwwrsWorkteam = a})

instance NFData DescribeWorkteamResponse
