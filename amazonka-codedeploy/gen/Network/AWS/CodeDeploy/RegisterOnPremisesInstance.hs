{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.RegisterOnPremisesInstance
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Registers an on-premises instance.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_RegisterOnPremisesInstance.html>
module Network.AWS.CodeDeploy.RegisterOnPremisesInstance
    (
    -- * Request
      RegisterOnPremisesInstance
    -- ** Request constructor
    , registerOnPremisesInstance
    -- ** Request lenses
    , ropirqInstanceName
    , ropirqIamUserARN

    -- * Response
    , RegisterOnPremisesInstanceResponse
    -- ** Response constructor
    , registerOnPremisesInstanceResponse
    ) where

import           Network.AWS.CodeDeploy.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of register on-premises instance operation.
--
-- /See:/ 'registerOnPremisesInstance' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ropirqInstanceName'
--
-- * 'ropirqIamUserARN'
data RegisterOnPremisesInstance = RegisterOnPremisesInstance'
    { _ropirqInstanceName :: !Text
    , _ropirqIamUserARN   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RegisterOnPremisesInstance' smart constructor.
registerOnPremisesInstance :: Text -> Text -> RegisterOnPremisesInstance
registerOnPremisesInstance pInstanceName_ pIamUserARN_ =
    RegisterOnPremisesInstance'
    { _ropirqInstanceName = pInstanceName_
    , _ropirqIamUserARN = pIamUserARN_
    }

-- | The name of the on-premises instance to register.
ropirqInstanceName :: Lens' RegisterOnPremisesInstance Text
ropirqInstanceName = lens _ropirqInstanceName (\ s a -> s{_ropirqInstanceName = a});

-- | The ARN of the IAM user to associate with the on-premises instance.
ropirqIamUserARN :: Lens' RegisterOnPremisesInstance Text
ropirqIamUserARN = lens _ropirqIamUserARN (\ s a -> s{_ropirqIamUserARN = a});

instance AWSRequest RegisterOnPremisesInstance where
        type Sv RegisterOnPremisesInstance = CodeDeploy
        type Rs RegisterOnPremisesInstance =
             RegisterOnPremisesInstanceResponse
        request = postJSON
        response
          = receiveNull RegisterOnPremisesInstanceResponse'

instance ToHeaders RegisterOnPremisesInstance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.RegisterOnPremisesInstance" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RegisterOnPremisesInstance where
        toJSON RegisterOnPremisesInstance'{..}
          = object
              ["instanceName" .= _ropirqInstanceName,
               "iamUserArn" .= _ropirqIamUserARN]

instance ToPath RegisterOnPremisesInstance where
        toPath = const "/"

instance ToQuery RegisterOnPremisesInstance where
        toQuery = const mempty

-- | /See:/ 'registerOnPremisesInstanceResponse' smart constructor.
data RegisterOnPremisesInstanceResponse =
    RegisterOnPremisesInstanceResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RegisterOnPremisesInstanceResponse' smart constructor.
registerOnPremisesInstanceResponse :: RegisterOnPremisesInstanceResponse
registerOnPremisesInstanceResponse = RegisterOnPremisesInstanceResponse'
