{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

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
    , ropiInstanceName
    , ropiIamUserARN

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
-- * 'ropiInstanceName'
--
-- * 'ropiIamUserARN'
data RegisterOnPremisesInstance = RegisterOnPremisesInstance'
    { _ropiInstanceName :: !Text
    , _ropiIamUserARN   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RegisterOnPremisesInstance' smart constructor.
registerOnPremisesInstance :: Text -> Text -> RegisterOnPremisesInstance
registerOnPremisesInstance pInstanceName pIamUserARN =
    RegisterOnPremisesInstance'
    { _ropiInstanceName = pInstanceName
    , _ropiIamUserARN = pIamUserARN
    }

-- | The name of the on-premises instance to register.
ropiInstanceName :: Lens' RegisterOnPremisesInstance Text
ropiInstanceName = lens _ropiInstanceName (\ s a -> s{_ropiInstanceName = a});

-- | The ARN of the IAM user to associate with the on-premises instance.
ropiIamUserARN :: Lens' RegisterOnPremisesInstance Text
ropiIamUserARN = lens _ropiIamUserARN (\ s a -> s{_ropiIamUserARN = a});

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
              ["instanceName" .= _ropiInstanceName,
               "iamUserArn" .= _ropiIamUserARN]

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
