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
-- Module      : Network.AWS.StorageGateway.NotifyWhenUploaded
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends you notification through CloudWatch Events when all files written to your NFS file share have been uploaded to Amazon S3.
--
--
-- AWS Storage Gateway can send a notification through Amazon CloudWatch Events when all files written to your file share up to that point in time have been uploaded to Amazon S3. These files include files written to the NFS file share up to the time that you make a request for notification. When the upload is done, Storage Gateway sends you notification through an Amazon CloudWatch Event. You can configure CloudWatch Events to send the notification through event targets such as Amazon SNS or AWS Lambda function. This operation is only supported in the file gateway type.
--
-- For more information, see Getting File Upload Notification in the Storage Gateway User Guide (https://docs.aws.amazon.com/storagegateway/latest/userguide/monitoring-file-gateway.html#get-upload-notification).
--
module Network.AWS.StorageGateway.NotifyWhenUploaded
    (
    -- * Creating a Request
      notifyWhenUploaded
    , NotifyWhenUploaded
    -- * Request Lenses
    , nwuFileShareARN

    -- * Destructuring the Response
    , notifyWhenUploadedResponse
    , NotifyWhenUploadedResponse
    -- * Response Lenses
    , nwursFileShareARN
    , nwursNotificationId
    , nwursResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | /See:/ 'notifyWhenUploaded' smart constructor.
newtype NotifyWhenUploaded = NotifyWhenUploaded'
  { _nwuFileShareARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NotifyWhenUploaded' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nwuFileShareARN' - Undocumented member.
notifyWhenUploaded
    :: Text -- ^ 'nwuFileShareARN'
    -> NotifyWhenUploaded
notifyWhenUploaded pFileShareARN_ =
  NotifyWhenUploaded' {_nwuFileShareARN = pFileShareARN_}


-- | Undocumented member.
nwuFileShareARN :: Lens' NotifyWhenUploaded Text
nwuFileShareARN = lens _nwuFileShareARN (\ s a -> s{_nwuFileShareARN = a})

instance AWSRequest NotifyWhenUploaded where
        type Rs NotifyWhenUploaded =
             NotifyWhenUploadedResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 NotifyWhenUploadedResponse' <$>
                   (x .?> "FileShareARN") <*> (x .?> "NotificationId")
                     <*> (pure (fromEnum s)))

instance Hashable NotifyWhenUploaded where

instance NFData NotifyWhenUploaded where

instance ToHeaders NotifyWhenUploaded where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.NotifyWhenUploaded" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON NotifyWhenUploaded where
        toJSON NotifyWhenUploaded'{..}
          = object
              (catMaybes
                 [Just ("FileShareARN" .= _nwuFileShareARN)])

instance ToPath NotifyWhenUploaded where
        toPath = const "/"

instance ToQuery NotifyWhenUploaded where
        toQuery = const mempty

-- | /See:/ 'notifyWhenUploadedResponse' smart constructor.
data NotifyWhenUploadedResponse = NotifyWhenUploadedResponse'
  { _nwursFileShareARN   :: !(Maybe Text)
  , _nwursNotificationId :: !(Maybe Text)
  , _nwursResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NotifyWhenUploadedResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nwursFileShareARN' - Undocumented member.
--
-- * 'nwursNotificationId' - Undocumented member.
--
-- * 'nwursResponseStatus' - -- | The response status code.
notifyWhenUploadedResponse
    :: Int -- ^ 'nwursResponseStatus'
    -> NotifyWhenUploadedResponse
notifyWhenUploadedResponse pResponseStatus_ =
  NotifyWhenUploadedResponse'
    { _nwursFileShareARN = Nothing
    , _nwursNotificationId = Nothing
    , _nwursResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
nwursFileShareARN :: Lens' NotifyWhenUploadedResponse (Maybe Text)
nwursFileShareARN = lens _nwursFileShareARN (\ s a -> s{_nwursFileShareARN = a})

-- | Undocumented member.
nwursNotificationId :: Lens' NotifyWhenUploadedResponse (Maybe Text)
nwursNotificationId = lens _nwursNotificationId (\ s a -> s{_nwursNotificationId = a})

-- | -- | The response status code.
nwursResponseStatus :: Lens' NotifyWhenUploadedResponse Int
nwursResponseStatus = lens _nwursResponseStatus (\ s a -> s{_nwursResponseStatus = a})

instance NFData NotifyWhenUploadedResponse where
