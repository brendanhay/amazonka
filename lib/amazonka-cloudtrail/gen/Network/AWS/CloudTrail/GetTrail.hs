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
-- Module      : Network.AWS.CloudTrail.GetTrail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns settings information for a specified trail.
module Network.AWS.CloudTrail.GetTrail
  ( -- * Creating a Request
    getTrail,
    GetTrail,

    -- * Request Lenses
    gtName,

    -- * Destructuring the Response
    getTrailResponse,
    GetTrailResponse,

    -- * Response Lenses
    gtrsTrail,
    gtrsResponseStatus,
  )
where

import Network.AWS.CloudTrail.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getTrail' smart constructor.
newtype GetTrail = GetTrail' {_gtName :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetTrail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtName' - The name or the Amazon Resource Name (ARN) of the trail for which you want to retrieve settings information.
getTrail ::
  -- | 'gtName'
  Text ->
  GetTrail
getTrail pName_ = GetTrail' {_gtName = pName_}

-- | The name or the Amazon Resource Name (ARN) of the trail for which you want to retrieve settings information.
gtName :: Lens' GetTrail Text
gtName = lens _gtName (\s a -> s {_gtName = a})

instance AWSRequest GetTrail where
  type Rs GetTrail = GetTrailResponse
  request = postJSON cloudTrail
  response =
    receiveJSON
      ( \s h x ->
          GetTrailResponse' <$> (x .?> "Trail") <*> (pure (fromEnum s))
      )

instance Hashable GetTrail

instance NFData GetTrail

instance ToHeaders GetTrail where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.GetTrail" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetTrail where
  toJSON GetTrail' {..} =
    object (catMaybes [Just ("Name" .= _gtName)])

instance ToPath GetTrail where
  toPath = const "/"

instance ToQuery GetTrail where
  toQuery = const mempty

-- | /See:/ 'getTrailResponse' smart constructor.
data GetTrailResponse = GetTrailResponse'
  { _gtrsTrail ::
      !(Maybe Trail),
    _gtrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetTrailResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtrsTrail' - Undocumented member.
--
-- * 'gtrsResponseStatus' - -- | The response status code.
getTrailResponse ::
  -- | 'gtrsResponseStatus'
  Int ->
  GetTrailResponse
getTrailResponse pResponseStatus_ =
  GetTrailResponse'
    { _gtrsTrail = Nothing,
      _gtrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
gtrsTrail :: Lens' GetTrailResponse (Maybe Trail)
gtrsTrail = lens _gtrsTrail (\s a -> s {_gtrsTrail = a})

-- | -- | The response status code.
gtrsResponseStatus :: Lens' GetTrailResponse Int
gtrsResponseStatus = lens _gtrsResponseStatus (\s a -> s {_gtrsResponseStatus = a})

instance NFData GetTrailResponse
