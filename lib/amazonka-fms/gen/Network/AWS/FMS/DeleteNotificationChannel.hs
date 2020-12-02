{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.DeleteNotificationChannel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an AWS Firewall Manager association with the IAM role and the Amazon Simple Notification Service (SNS) topic that is used to record AWS Firewall Manager SNS logs.
--
--
module Network.AWS.FMS.DeleteNotificationChannel
    (
    -- * Creating a Request
      deleteNotificationChannel
    , DeleteNotificationChannel

    -- * Destructuring the Response
    , deleteNotificationChannelResponse
    , DeleteNotificationChannelResponse
    ) where

import Network.AWS.FMS.Types
import Network.AWS.FMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteNotificationChannel' smart constructor.
data DeleteNotificationChannel =
  DeleteNotificationChannel'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteNotificationChannel' with the minimum fields required to make a request.
--
deleteNotificationChannel
    :: DeleteNotificationChannel
deleteNotificationChannel = DeleteNotificationChannel'


instance AWSRequest DeleteNotificationChannel where
        type Rs DeleteNotificationChannel =
             DeleteNotificationChannelResponse
        request = postJSON fms
        response
          = receiveNull DeleteNotificationChannelResponse'

instance Hashable DeleteNotificationChannel where

instance NFData DeleteNotificationChannel where

instance ToHeaders DeleteNotificationChannel where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSFMS_20180101.DeleteNotificationChannel" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteNotificationChannel where
        toJSON = const (Object mempty)

instance ToPath DeleteNotificationChannel where
        toPath = const "/"

instance ToQuery DeleteNotificationChannel where
        toQuery = const mempty

-- | /See:/ 'deleteNotificationChannelResponse' smart constructor.
data DeleteNotificationChannelResponse =
  DeleteNotificationChannelResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteNotificationChannelResponse' with the minimum fields required to make a request.
--
deleteNotificationChannelResponse
    :: DeleteNotificationChannelResponse
deleteNotificationChannelResponse = DeleteNotificationChannelResponse'


instance NFData DeleteNotificationChannelResponse
         where
