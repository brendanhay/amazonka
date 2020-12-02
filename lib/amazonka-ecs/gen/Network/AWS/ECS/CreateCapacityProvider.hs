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
-- Module      : Network.AWS.ECS.CreateCapacityProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new capacity provider. Capacity providers are associated with an Amazon ECS cluster and are used in capacity provider strategies to facilitate cluster auto scaling.
--
--
-- Only capacity providers using an Auto Scaling group can be created. Amazon ECS tasks on AWS Fargate use the @FARGATE@ and @FARGATE_SPOT@ capacity providers which are already created and available to all accounts in Regions supported by AWS Fargate.
module Network.AWS.ECS.CreateCapacityProvider
  ( -- * Creating a Request
    createCapacityProvider,
    CreateCapacityProvider,

    -- * Request Lenses
    ccpTags,
    ccpName,
    ccpAutoScalingGroupProvider,

    -- * Destructuring the Response
    createCapacityProviderResponse,
    CreateCapacityProviderResponse,

    -- * Response Lenses
    ccprsCapacityProvider,
    ccprsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createCapacityProvider' smart constructor.
data CreateCapacityProvider = CreateCapacityProvider'
  { _ccpTags ::
      !(Maybe [Tag]),
    _ccpName :: !Text,
    _ccpAutoScalingGroupProvider ::
      !AutoScalingGroupProvider
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateCapacityProvider' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccpTags' - The metadata that you apply to the capacity provider to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define. The following basic restrictions apply to tags:     * Maximum number of tags per resource - 50     * For each resource, each tag key must be unique, and each tag key can have only one value.     * Maximum key length - 128 Unicode characters in UTF-8     * Maximum value length - 256 Unicode characters in UTF-8     * If your tagging schema is used across multiple services and resources, remember that other services may have restrictions on allowed characters. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following characters: + - = . _ : / @.     * Tag keys and values are case-sensitive.     * Do not use @aws:@ , @AWS:@ , or any upper or lowercase combination of such as a prefix for either keys or values as it is reserved for AWS use. You cannot edit or delete tag keys or values with this prefix. Tags with this prefix do not count against your tags per resource limit.
--
-- * 'ccpName' - The name of the capacity provider. Up to 255 characters are allowed, including letters (upper and lowercase), numbers, underscores, and hyphens. The name cannot be prefixed with "@aws@ ", "@ecs@ ", or "@fargate@ ".
--
-- * 'ccpAutoScalingGroupProvider' - The details of the Auto Scaling group for the capacity provider.
createCapacityProvider ::
  -- | 'ccpName'
  Text ->
  -- | 'ccpAutoScalingGroupProvider'
  AutoScalingGroupProvider ->
  CreateCapacityProvider
createCapacityProvider pName_ pAutoScalingGroupProvider_ =
  CreateCapacityProvider'
    { _ccpTags = Nothing,
      _ccpName = pName_,
      _ccpAutoScalingGroupProvider = pAutoScalingGroupProvider_
    }

-- | The metadata that you apply to the capacity provider to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define. The following basic restrictions apply to tags:     * Maximum number of tags per resource - 50     * For each resource, each tag key must be unique, and each tag key can have only one value.     * Maximum key length - 128 Unicode characters in UTF-8     * Maximum value length - 256 Unicode characters in UTF-8     * If your tagging schema is used across multiple services and resources, remember that other services may have restrictions on allowed characters. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following characters: + - = . _ : / @.     * Tag keys and values are case-sensitive.     * Do not use @aws:@ , @AWS:@ , or any upper or lowercase combination of such as a prefix for either keys or values as it is reserved for AWS use. You cannot edit or delete tag keys or values with this prefix. Tags with this prefix do not count against your tags per resource limit.
ccpTags :: Lens' CreateCapacityProvider [Tag]
ccpTags = lens _ccpTags (\s a -> s {_ccpTags = a}) . _Default . _Coerce

-- | The name of the capacity provider. Up to 255 characters are allowed, including letters (upper and lowercase), numbers, underscores, and hyphens. The name cannot be prefixed with "@aws@ ", "@ecs@ ", or "@fargate@ ".
ccpName :: Lens' CreateCapacityProvider Text
ccpName = lens _ccpName (\s a -> s {_ccpName = a})

-- | The details of the Auto Scaling group for the capacity provider.
ccpAutoScalingGroupProvider :: Lens' CreateCapacityProvider AutoScalingGroupProvider
ccpAutoScalingGroupProvider = lens _ccpAutoScalingGroupProvider (\s a -> s {_ccpAutoScalingGroupProvider = a})

instance AWSRequest CreateCapacityProvider where
  type Rs CreateCapacityProvider = CreateCapacityProviderResponse
  request = postJSON ecs
  response =
    receiveJSON
      ( \s h x ->
          CreateCapacityProviderResponse'
            <$> (x .?> "capacityProvider") <*> (pure (fromEnum s))
      )

instance Hashable CreateCapacityProvider

instance NFData CreateCapacityProvider

instance ToHeaders CreateCapacityProvider where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AmazonEC2ContainerServiceV20141113.CreateCapacityProvider" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateCapacityProvider where
  toJSON CreateCapacityProvider' {..} =
    object
      ( catMaybes
          [ ("tags" .=) <$> _ccpTags,
            Just ("name" .= _ccpName),
            Just ("autoScalingGroupProvider" .= _ccpAutoScalingGroupProvider)
          ]
      )

instance ToPath CreateCapacityProvider where
  toPath = const "/"

instance ToQuery CreateCapacityProvider where
  toQuery = const mempty

-- | /See:/ 'createCapacityProviderResponse' smart constructor.
data CreateCapacityProviderResponse = CreateCapacityProviderResponse'
  { _ccprsCapacityProvider ::
      !(Maybe CapacityProvider),
    _ccprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateCapacityProviderResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccprsCapacityProvider' - The full description of the new capacity provider.
--
-- * 'ccprsResponseStatus' - -- | The response status code.
createCapacityProviderResponse ::
  -- | 'ccprsResponseStatus'
  Int ->
  CreateCapacityProviderResponse
createCapacityProviderResponse pResponseStatus_ =
  CreateCapacityProviderResponse'
    { _ccprsCapacityProvider = Nothing,
      _ccprsResponseStatus = pResponseStatus_
    }

-- | The full description of the new capacity provider.
ccprsCapacityProvider :: Lens' CreateCapacityProviderResponse (Maybe CapacityProvider)
ccprsCapacityProvider = lens _ccprsCapacityProvider (\s a -> s {_ccprsCapacityProvider = a})

-- | -- | The response status code.
ccprsResponseStatus :: Lens' CreateCapacityProviderResponse Int
ccprsResponseStatus = lens _ccprsResponseStatus (\s a -> s {_ccprsResponseStatus = a})

instance NFData CreateCapacityProviderResponse
