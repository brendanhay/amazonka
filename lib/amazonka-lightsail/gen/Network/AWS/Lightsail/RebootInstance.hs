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
-- Module      : Network.AWS.Lightsail.RebootInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restarts a specific instance.
--
--
-- The @reboot instance@ operation supports tag-based access control via resource tags applied to the resource identified by @instance name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.RebootInstance
  ( -- * Creating a Request
    rebootInstance,
    RebootInstance,

    -- * Request Lenses
    riInstanceName,

    -- * Destructuring the Response
    rebootInstanceResponse,
    RebootInstanceResponse,

    -- * Response Lenses
    rirsOperations,
    rirsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'rebootInstance' smart constructor.
newtype RebootInstance = RebootInstance' {_riInstanceName :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RebootInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'riInstanceName' - The name of the instance to reboot.
rebootInstance ::
  -- | 'riInstanceName'
  Text ->
  RebootInstance
rebootInstance pInstanceName_ =
  RebootInstance' {_riInstanceName = pInstanceName_}

-- | The name of the instance to reboot.
riInstanceName :: Lens' RebootInstance Text
riInstanceName = lens _riInstanceName (\s a -> s {_riInstanceName = a})

instance AWSRequest RebootInstance where
  type Rs RebootInstance = RebootInstanceResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          RebootInstanceResponse'
            <$> (x .?> "operations" .!@ mempty) <*> (pure (fromEnum s))
      )

instance Hashable RebootInstance

instance NFData RebootInstance

instance ToHeaders RebootInstance where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Lightsail_20161128.RebootInstance" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON RebootInstance where
  toJSON RebootInstance' {..} =
    object (catMaybes [Just ("instanceName" .= _riInstanceName)])

instance ToPath RebootInstance where
  toPath = const "/"

instance ToQuery RebootInstance where
  toQuery = const mempty

-- | /See:/ 'rebootInstanceResponse' smart constructor.
data RebootInstanceResponse = RebootInstanceResponse'
  { _rirsOperations ::
      !(Maybe [Operation]),
    _rirsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RebootInstanceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rirsOperations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- * 'rirsResponseStatus' - -- | The response status code.
rebootInstanceResponse ::
  -- | 'rirsResponseStatus'
  Int ->
  RebootInstanceResponse
rebootInstanceResponse pResponseStatus_ =
  RebootInstanceResponse'
    { _rirsOperations = Nothing,
      _rirsResponseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
rirsOperations :: Lens' RebootInstanceResponse [Operation]
rirsOperations = lens _rirsOperations (\s a -> s {_rirsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
rirsResponseStatus :: Lens' RebootInstanceResponse Int
rirsResponseStatus = lens _rirsResponseStatus (\s a -> s {_rirsResponseStatus = a})

instance NFData RebootInstanceResponse
