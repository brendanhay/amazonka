{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.UpdateElasticIP
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Updates a registered Elastic IP address\'s name. For more information,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_UpdateElasticIP.html>
module Network.AWS.OpsWorks.UpdateElasticIP
    (
    -- * Request
      UpdateElasticIP
    -- ** Request constructor
    , updateElasticIP
    -- ** Request lenses
    , ueiName
    , ueiElasticIP

    -- * Response
    , UpdateElasticIPResponse
    -- ** Response constructor
    , updateElasticIPResponse
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'updateElasticIP' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ueiName'
--
-- * 'ueiElasticIP'
data UpdateElasticIP = UpdateElasticIP'
    { _ueiName      :: !(Maybe Text)
    , _ueiElasticIP :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateElasticIP' smart constructor.
updateElasticIP :: Text -> UpdateElasticIP
updateElasticIP pElasticIP_ =
    UpdateElasticIP'
    { _ueiName = Nothing
    , _ueiElasticIP = pElasticIP_
    }

-- | The new name.
ueiName :: Lens' UpdateElasticIP (Maybe Text)
ueiName = lens _ueiName (\ s a -> s{_ueiName = a});

-- | The address.
ueiElasticIP :: Lens' UpdateElasticIP Text
ueiElasticIP = lens _ueiElasticIP (\ s a -> s{_ueiElasticIP = a});

instance AWSRequest UpdateElasticIP where
        type Sv UpdateElasticIP = OpsWorks
        type Rs UpdateElasticIP = UpdateElasticIPResponse
        request = postJSON "UpdateElasticIP"
        response = receiveNull UpdateElasticIPResponse'

instance ToHeaders UpdateElasticIP where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.UpdateElasticIP" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateElasticIP where
        toJSON UpdateElasticIP'{..}
          = object
              ["Name" .= _ueiName, "ElasticIp" .= _ueiElasticIP]

instance ToPath UpdateElasticIP where
        toPath = const "/"

instance ToQuery UpdateElasticIP where
        toQuery = const mempty

-- | /See:/ 'updateElasticIPResponse' smart constructor.
data UpdateElasticIPResponse =
    UpdateElasticIPResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateElasticIPResponse' smart constructor.
updateElasticIPResponse :: UpdateElasticIPResponse
updateElasticIPResponse = UpdateElasticIPResponse'
