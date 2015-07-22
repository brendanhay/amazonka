{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.RegisterElasticIP
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Registers an Elastic IP address with a specified stack. An address can
-- be registered with only one stack at a time. If the address is already
-- registered, you must first deregister it by calling DeregisterElasticIp.
-- For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_RegisterElasticIP.html>
module Network.AWS.OpsWorks.RegisterElasticIP
    (
    -- * Request
      RegisterElasticIP
    -- ** Request constructor
    , registerElasticIP
    -- ** Request lenses
    , reirqElasticIP
    , reirqStackId

    -- * Response
    , RegisterElasticIPResponse
    -- ** Response constructor
    , registerElasticIPResponse
    -- ** Response lenses
    , reirsElasticIP
    , reirsStatus
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'registerElasticIP' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'reirqElasticIP'
--
-- * 'reirqStackId'
data RegisterElasticIP = RegisterElasticIP'
    { _reirqElasticIP :: !Text
    , _reirqStackId   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RegisterElasticIP' smart constructor.
registerElasticIP :: Text -> Text -> RegisterElasticIP
registerElasticIP pElasticIP_ pStackId_ =
    RegisterElasticIP'
    { _reirqElasticIP = pElasticIP_
    , _reirqStackId = pStackId_
    }

-- | The Elastic IP address.
reirqElasticIP :: Lens' RegisterElasticIP Text
reirqElasticIP = lens _reirqElasticIP (\ s a -> s{_reirqElasticIP = a});

-- | The stack ID.
reirqStackId :: Lens' RegisterElasticIP Text
reirqStackId = lens _reirqStackId (\ s a -> s{_reirqStackId = a});

instance AWSRequest RegisterElasticIP where
        type Sv RegisterElasticIP = OpsWorks
        type Rs RegisterElasticIP = RegisterElasticIPResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 RegisterElasticIPResponse' <$>
                   (x .?> "ElasticIp") <*> (pure (fromEnum s)))

instance ToHeaders RegisterElasticIP where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.RegisterElasticIP" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RegisterElasticIP where
        toJSON RegisterElasticIP'{..}
          = object
              ["ElasticIp" .= _reirqElasticIP,
               "StackId" .= _reirqStackId]

instance ToPath RegisterElasticIP where
        toPath = const "/"

instance ToQuery RegisterElasticIP where
        toQuery = const mempty

-- | Contains the response to a @RegisterElasticIp@ request.
--
-- /See:/ 'registerElasticIPResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'reirsElasticIP'
--
-- * 'reirsStatus'
data RegisterElasticIPResponse = RegisterElasticIPResponse'
    { _reirsElasticIP :: !(Maybe Text)
    , _reirsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RegisterElasticIPResponse' smart constructor.
registerElasticIPResponse :: Int -> RegisterElasticIPResponse
registerElasticIPResponse pStatus_ =
    RegisterElasticIPResponse'
    { _reirsElasticIP = Nothing
    , _reirsStatus = pStatus_
    }

-- | The Elastic IP address.
reirsElasticIP :: Lens' RegisterElasticIPResponse (Maybe Text)
reirsElasticIP = lens _reirsElasticIP (\ s a -> s{_reirsElasticIP = a});

-- | FIXME: Undocumented member.
reirsStatus :: Lens' RegisterElasticIPResponse Int
reirsStatus = lens _reirsStatus (\ s a -> s{_reirsStatus = a});
