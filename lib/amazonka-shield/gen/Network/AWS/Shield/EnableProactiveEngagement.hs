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
-- Module      : Network.AWS.Shield.EnableProactiveEngagement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Authorizes the DDoS Response Team (DRT) to use email and phone to notify contacts about escalations to the DRT and to initiate proactive customer support.
module Network.AWS.Shield.EnableProactiveEngagement
  ( -- * Creating a Request
    enableProactiveEngagement,
    EnableProactiveEngagement,

    -- * Destructuring the Response
    enableProactiveEngagementResponse,
    EnableProactiveEngagementResponse,

    -- * Response Lenses
    epersResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Shield.Types

-- | /See:/ 'enableProactiveEngagement' smart constructor.
data EnableProactiveEngagement = EnableProactiveEngagement'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EnableProactiveEngagement' with the minimum fields required to make a request.
enableProactiveEngagement ::
  EnableProactiveEngagement
enableProactiveEngagement = EnableProactiveEngagement'

instance AWSRequest EnableProactiveEngagement where
  type
    Rs EnableProactiveEngagement =
      EnableProactiveEngagementResponse
  request = postJSON shield
  response =
    receiveEmpty
      ( \s h x ->
          EnableProactiveEngagementResponse' <$> (pure (fromEnum s))
      )

instance Hashable EnableProactiveEngagement

instance NFData EnableProactiveEngagement

instance ToHeaders EnableProactiveEngagement where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSShield_20160616.EnableProactiveEngagement" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON EnableProactiveEngagement where
  toJSON = const (Object mempty)

instance ToPath EnableProactiveEngagement where
  toPath = const "/"

instance ToQuery EnableProactiveEngagement where
  toQuery = const mempty

-- | /See:/ 'enableProactiveEngagementResponse' smart constructor.
newtype EnableProactiveEngagementResponse = EnableProactiveEngagementResponse'
  { _epersResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EnableProactiveEngagementResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'epersResponseStatus' - -- | The response status code.
enableProactiveEngagementResponse ::
  -- | 'epersResponseStatus'
  Int ->
  EnableProactiveEngagementResponse
enableProactiveEngagementResponse pResponseStatus_ =
  EnableProactiveEngagementResponse'
    { _epersResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
epersResponseStatus :: Lens' EnableProactiveEngagementResponse Int
epersResponseStatus = lens _epersResponseStatus (\s a -> s {_epersResponseStatus = a})

instance NFData EnableProactiveEngagementResponse
