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
-- Module      : Network.AWS.Lightsail.CreateCloudFormationStack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS CloudFormation stack, which creates a new Amazon EC2 instance from an exported Amazon Lightsail snapshot. This operation results in a CloudFormation stack record that can be used to track the AWS CloudFormation stack created. Use the @get cloud formation stack records@ operation to get a list of the CloudFormation stacks created.
--
--
-- /Important:/ Wait until after your new Amazon EC2 instance is created before running the @create cloud formation stack@ operation again with the same export snapshot record.
module Network.AWS.Lightsail.CreateCloudFormationStack
  ( -- * Creating a Request
    createCloudFormationStack,
    CreateCloudFormationStack,

    -- * Request Lenses
    ccfsInstances,

    -- * Destructuring the Response
    createCloudFormationStackResponse,
    CreateCloudFormationStackResponse,

    -- * Response Lenses
    ccfsrsOperations,
    ccfsrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createCloudFormationStack' smart constructor.
newtype CreateCloudFormationStack = CreateCloudFormationStack'
  { _ccfsInstances ::
      [InstanceEntry]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateCloudFormationStack' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccfsInstances' - An array of parameters that will be used to create the new Amazon EC2 instance. You can only pass one instance entry at a time in this array. You will get an invalid parameter error if you pass more than one instance entry in this array.
createCloudFormationStack ::
  CreateCloudFormationStack
createCloudFormationStack =
  CreateCloudFormationStack' {_ccfsInstances = mempty}

-- | An array of parameters that will be used to create the new Amazon EC2 instance. You can only pass one instance entry at a time in this array. You will get an invalid parameter error if you pass more than one instance entry in this array.
ccfsInstances :: Lens' CreateCloudFormationStack [InstanceEntry]
ccfsInstances = lens _ccfsInstances (\s a -> s {_ccfsInstances = a}) . _Coerce

instance AWSRequest CreateCloudFormationStack where
  type
    Rs CreateCloudFormationStack =
      CreateCloudFormationStackResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          CreateCloudFormationStackResponse'
            <$> (x .?> "operations" .!@ mempty) <*> (pure (fromEnum s))
      )

instance Hashable CreateCloudFormationStack

instance NFData CreateCloudFormationStack

instance ToHeaders CreateCloudFormationStack where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Lightsail_20161128.CreateCloudFormationStack" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateCloudFormationStack where
  toJSON CreateCloudFormationStack' {..} =
    object (catMaybes [Just ("instances" .= _ccfsInstances)])

instance ToPath CreateCloudFormationStack where
  toPath = const "/"

instance ToQuery CreateCloudFormationStack where
  toQuery = const mempty

-- | /See:/ 'createCloudFormationStackResponse' smart constructor.
data CreateCloudFormationStackResponse = CreateCloudFormationStackResponse'
  { _ccfsrsOperations ::
      !(Maybe [Operation]),
    _ccfsrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateCloudFormationStackResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccfsrsOperations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- * 'ccfsrsResponseStatus' - -- | The response status code.
createCloudFormationStackResponse ::
  -- | 'ccfsrsResponseStatus'
  Int ->
  CreateCloudFormationStackResponse
createCloudFormationStackResponse pResponseStatus_ =
  CreateCloudFormationStackResponse'
    { _ccfsrsOperations = Nothing,
      _ccfsrsResponseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
ccfsrsOperations :: Lens' CreateCloudFormationStackResponse [Operation]
ccfsrsOperations = lens _ccfsrsOperations (\s a -> s {_ccfsrsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
ccfsrsResponseStatus :: Lens' CreateCloudFormationStackResponse Int
ccfsrsResponseStatus = lens _ccfsrsResponseStatus (\s a -> s {_ccfsrsResponseStatus = a})

instance NFData CreateCloudFormationStackResponse
