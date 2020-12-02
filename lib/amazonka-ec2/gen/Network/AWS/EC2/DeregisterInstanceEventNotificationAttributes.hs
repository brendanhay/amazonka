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
-- Module      : Network.AWS.EC2.DeregisterInstanceEventNotificationAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters tag keys to prevent tags that have the specified tag keys from being included in scheduled event notifications for resources in the Region.
module Network.AWS.EC2.DeregisterInstanceEventNotificationAttributes
  ( -- * Creating a Request
    deregisterInstanceEventNotificationAttributes,
    DeregisterInstanceEventNotificationAttributes,

    -- * Request Lenses
    dienaInstanceTagAttribute,
    dienaDryRun,

    -- * Destructuring the Response
    deregisterInstanceEventNotificationAttributesResponse,
    DeregisterInstanceEventNotificationAttributesResponse,

    -- * Response Lenses
    dienarsInstanceTagAttribute,
    dienarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deregisterInstanceEventNotificationAttributes' smart constructor.
data DeregisterInstanceEventNotificationAttributes = DeregisterInstanceEventNotificationAttributes'
  { _dienaInstanceTagAttribute ::
      !( Maybe
           DeregisterInstanceTagAttributeRequest
       ),
    _dienaDryRun ::
      !( Maybe
           Bool
       )
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DeregisterInstanceEventNotificationAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dienaInstanceTagAttribute' - Information about the tag keys to deregister.
--
-- * 'dienaDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
deregisterInstanceEventNotificationAttributes ::
  DeregisterInstanceEventNotificationAttributes
deregisterInstanceEventNotificationAttributes =
  DeregisterInstanceEventNotificationAttributes'
    { _dienaInstanceTagAttribute =
        Nothing,
      _dienaDryRun = Nothing
    }

-- | Information about the tag keys to deregister.
dienaInstanceTagAttribute :: Lens' DeregisterInstanceEventNotificationAttributes (Maybe DeregisterInstanceTagAttributeRequest)
dienaInstanceTagAttribute = lens _dienaInstanceTagAttribute (\s a -> s {_dienaInstanceTagAttribute = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dienaDryRun :: Lens' DeregisterInstanceEventNotificationAttributes (Maybe Bool)
dienaDryRun = lens _dienaDryRun (\s a -> s {_dienaDryRun = a})

instance AWSRequest DeregisterInstanceEventNotificationAttributes where
  type
    Rs DeregisterInstanceEventNotificationAttributes =
      DeregisterInstanceEventNotificationAttributesResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DeregisterInstanceEventNotificationAttributesResponse'
            <$> (x .@? "instanceTagAttribute") <*> (pure (fromEnum s))
      )

instance Hashable DeregisterInstanceEventNotificationAttributes

instance NFData DeregisterInstanceEventNotificationAttributes

instance ToHeaders DeregisterInstanceEventNotificationAttributes where
  toHeaders = const mempty

instance ToPath DeregisterInstanceEventNotificationAttributes where
  toPath = const "/"

instance ToQuery DeregisterInstanceEventNotificationAttributes where
  toQuery DeregisterInstanceEventNotificationAttributes' {..} =
    mconcat
      [ "Action"
          =: ("DeregisterInstanceEventNotificationAttributes" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "InstanceTagAttribute" =: _dienaInstanceTagAttribute,
        "DryRun" =: _dienaDryRun
      ]

-- | /See:/ 'deregisterInstanceEventNotificationAttributesResponse' smart constructor.
data DeregisterInstanceEventNotificationAttributesResponse = DeregisterInstanceEventNotificationAttributesResponse'
  { _dienarsInstanceTagAttribute ::
      !( Maybe
           InstanceTagNotificationAttribute
       ),
    _dienarsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DeregisterInstanceEventNotificationAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dienarsInstanceTagAttribute' - The resulting set of tag keys.
--
-- * 'dienarsResponseStatus' - -- | The response status code.
deregisterInstanceEventNotificationAttributesResponse ::
  -- | 'dienarsResponseStatus'
  Int ->
  DeregisterInstanceEventNotificationAttributesResponse
deregisterInstanceEventNotificationAttributesResponse
  pResponseStatus_ =
    DeregisterInstanceEventNotificationAttributesResponse'
      { _dienarsInstanceTagAttribute =
          Nothing,
        _dienarsResponseStatus =
          pResponseStatus_
      }

-- | The resulting set of tag keys.
dienarsInstanceTagAttribute :: Lens' DeregisterInstanceEventNotificationAttributesResponse (Maybe InstanceTagNotificationAttribute)
dienarsInstanceTagAttribute = lens _dienarsInstanceTagAttribute (\s a -> s {_dienarsInstanceTagAttribute = a})

-- | -- | The response status code.
dienarsResponseStatus :: Lens' DeregisterInstanceEventNotificationAttributesResponse Int
dienarsResponseStatus = lens _dienarsResponseStatus (\s a -> s {_dienarsResponseStatus = a})

instance
  NFData
    DeregisterInstanceEventNotificationAttributesResponse
