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
-- Module      : Network.AWS.CloudWatchLogs.DeleteRetentionPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified retention policy.
--
--
-- Log events do not expire if they belong to log groups without a retention policy.
--
module Network.AWS.CloudWatchLogs.DeleteRetentionPolicy
    (
    -- * Creating a Request
      deleteRetentionPolicy
    , DeleteRetentionPolicy
    -- * Request Lenses
    , drpLogGroupName

    -- * Destructuring the Response
    , deleteRetentionPolicyResponse
    , DeleteRetentionPolicyResponse
    ) where

import Network.AWS.CloudWatchLogs.Types
import Network.AWS.CloudWatchLogs.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteRetentionPolicy' smart constructor.
newtype DeleteRetentionPolicy = DeleteRetentionPolicy'
  { _drpLogGroupName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRetentionPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drpLogGroupName' - The name of the log group.
deleteRetentionPolicy
    :: Text -- ^ 'drpLogGroupName'
    -> DeleteRetentionPolicy
deleteRetentionPolicy pLogGroupName_ =
  DeleteRetentionPolicy' {_drpLogGroupName = pLogGroupName_}


-- | The name of the log group.
drpLogGroupName :: Lens' DeleteRetentionPolicy Text
drpLogGroupName = lens _drpLogGroupName (\ s a -> s{_drpLogGroupName = a})

instance AWSRequest DeleteRetentionPolicy where
        type Rs DeleteRetentionPolicy =
             DeleteRetentionPolicyResponse
        request = postJSON cloudWatchLogs
        response = receiveNull DeleteRetentionPolicyResponse'

instance Hashable DeleteRetentionPolicy where

instance NFData DeleteRetentionPolicy where

instance ToHeaders DeleteRetentionPolicy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.DeleteRetentionPolicy" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteRetentionPolicy where
        toJSON DeleteRetentionPolicy'{..}
          = object
              (catMaybes
                 [Just ("logGroupName" .= _drpLogGroupName)])

instance ToPath DeleteRetentionPolicy where
        toPath = const "/"

instance ToQuery DeleteRetentionPolicy where
        toQuery = const mempty

-- | /See:/ 'deleteRetentionPolicyResponse' smart constructor.
data DeleteRetentionPolicyResponse =
  DeleteRetentionPolicyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRetentionPolicyResponse' with the minimum fields required to make a request.
--
deleteRetentionPolicyResponse
    :: DeleteRetentionPolicyResponse
deleteRetentionPolicyResponse = DeleteRetentionPolicyResponse'


instance NFData DeleteRetentionPolicyResponse where
