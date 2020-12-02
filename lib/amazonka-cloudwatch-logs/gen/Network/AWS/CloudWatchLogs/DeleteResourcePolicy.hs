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
-- Module      : Network.AWS.CloudWatchLogs.DeleteResourcePolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a resource policy from this account. This revokes the access of the identities in that policy to put log events to this account.
--
--
module Network.AWS.CloudWatchLogs.DeleteResourcePolicy
    (
    -- * Creating a Request
      deleteResourcePolicy
    , DeleteResourcePolicy
    -- * Request Lenses
    , drpPolicyName

    -- * Destructuring the Response
    , deleteResourcePolicyResponse
    , DeleteResourcePolicyResponse
    ) where

import Network.AWS.CloudWatchLogs.Types
import Network.AWS.CloudWatchLogs.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteResourcePolicy' smart constructor.
newtype DeleteResourcePolicy = DeleteResourcePolicy'
  { _drpPolicyName :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteResourcePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drpPolicyName' - The name of the policy to be revoked. This parameter is required.
deleteResourcePolicy
    :: DeleteResourcePolicy
deleteResourcePolicy = DeleteResourcePolicy' {_drpPolicyName = Nothing}


-- | The name of the policy to be revoked. This parameter is required.
drpPolicyName :: Lens' DeleteResourcePolicy (Maybe Text)
drpPolicyName = lens _drpPolicyName (\ s a -> s{_drpPolicyName = a})

instance AWSRequest DeleteResourcePolicy where
        type Rs DeleteResourcePolicy =
             DeleteResourcePolicyResponse
        request = postJSON cloudWatchLogs
        response = receiveNull DeleteResourcePolicyResponse'

instance Hashable DeleteResourcePolicy where

instance NFData DeleteResourcePolicy where

instance ToHeaders DeleteResourcePolicy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.DeleteResourcePolicy" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteResourcePolicy where
        toJSON DeleteResourcePolicy'{..}
          = object
              (catMaybes [("policyName" .=) <$> _drpPolicyName])

instance ToPath DeleteResourcePolicy where
        toPath = const "/"

instance ToQuery DeleteResourcePolicy where
        toQuery = const mempty

-- | /See:/ 'deleteResourcePolicyResponse' smart constructor.
data DeleteResourcePolicyResponse =
  DeleteResourcePolicyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteResourcePolicyResponse' with the minimum fields required to make a request.
--
deleteResourcePolicyResponse
    :: DeleteResourcePolicyResponse
deleteResourcePolicyResponse = DeleteResourcePolicyResponse'


instance NFData DeleteResourcePolicyResponse where
