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
-- Module      : Network.AWS.CloudWatchLogs.PutRetentionPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the retention of the specified log group. A retention policy allows you to configure the number of days for which to retain log events in the specified log group.
--
--
module Network.AWS.CloudWatchLogs.PutRetentionPolicy
    (
    -- * Creating a Request
      putRetentionPolicy
    , PutRetentionPolicy
    -- * Request Lenses
    , prpLogGroupName
    , prpRetentionInDays

    -- * Destructuring the Response
    , putRetentionPolicyResponse
    , PutRetentionPolicyResponse
    ) where

import Network.AWS.CloudWatchLogs.Types
import Network.AWS.CloudWatchLogs.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putRetentionPolicy' smart constructor.
data PutRetentionPolicy = PutRetentionPolicy'
  { _prpLogGroupName    :: !Text
  , _prpRetentionInDays :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutRetentionPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prpLogGroupName' - The name of the log group.
--
-- * 'prpRetentionInDays' - Undocumented member.
putRetentionPolicy
    :: Text -- ^ 'prpLogGroupName'
    -> Int -- ^ 'prpRetentionInDays'
    -> PutRetentionPolicy
putRetentionPolicy pLogGroupName_ pRetentionInDays_ =
  PutRetentionPolicy'
    {_prpLogGroupName = pLogGroupName_, _prpRetentionInDays = pRetentionInDays_}


-- | The name of the log group.
prpLogGroupName :: Lens' PutRetentionPolicy Text
prpLogGroupName = lens _prpLogGroupName (\ s a -> s{_prpLogGroupName = a})

-- | Undocumented member.
prpRetentionInDays :: Lens' PutRetentionPolicy Int
prpRetentionInDays = lens _prpRetentionInDays (\ s a -> s{_prpRetentionInDays = a})

instance AWSRequest PutRetentionPolicy where
        type Rs PutRetentionPolicy =
             PutRetentionPolicyResponse
        request = postJSON cloudWatchLogs
        response = receiveNull PutRetentionPolicyResponse'

instance Hashable PutRetentionPolicy where

instance NFData PutRetentionPolicy where

instance ToHeaders PutRetentionPolicy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.PutRetentionPolicy" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutRetentionPolicy where
        toJSON PutRetentionPolicy'{..}
          = object
              (catMaybes
                 [Just ("logGroupName" .= _prpLogGroupName),
                  Just ("retentionInDays" .= _prpRetentionInDays)])

instance ToPath PutRetentionPolicy where
        toPath = const "/"

instance ToQuery PutRetentionPolicy where
        toQuery = const mempty

-- | /See:/ 'putRetentionPolicyResponse' smart constructor.
data PutRetentionPolicyResponse =
  PutRetentionPolicyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutRetentionPolicyResponse' with the minimum fields required to make a request.
--
putRetentionPolicyResponse
    :: PutRetentionPolicyResponse
putRetentionPolicyResponse = PutRetentionPolicyResponse'


instance NFData PutRetentionPolicyResponse where
