{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CodeDeploy.RegisterOnPremisesInstance
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Registers an on-premises instance.
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

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CodeDeploy.Types

-- | /See:/ 'registerOnPremisesInstance' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ropiInstanceName'
--
-- * 'ropiIamUserARN'
data RegisterOnPremisesInstance = RegisterOnPremisesInstance'{_ropiInstanceName :: Text, _ropiIamUserARN :: Text} deriving (Eq, Read, Show)

-- | 'RegisterOnPremisesInstance' smart constructor.
registerOnPremisesInstance :: Text -> Text -> RegisterOnPremisesInstance
registerOnPremisesInstance pInstanceName pIamUserARN = RegisterOnPremisesInstance'{_ropiInstanceName = pInstanceName, _ropiIamUserARN = pIamUserARN};

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
data RegisterOnPremisesInstanceResponse = RegisterOnPremisesInstanceResponse' deriving (Eq, Read, Show)

-- | 'RegisterOnPremisesInstanceResponse' smart constructor.
registerOnPremisesInstanceResponse :: RegisterOnPremisesInstanceResponse
registerOnPremisesInstanceResponse = RegisterOnPremisesInstanceResponse';
