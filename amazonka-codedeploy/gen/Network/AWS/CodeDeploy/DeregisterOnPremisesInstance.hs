{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CodeDeploy.DeregisterOnPremisesInstance
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

-- | Deregisters an on-premises instance.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_DeregisterOnPremisesInstance.html>
module Network.AWS.CodeDeploy.DeregisterOnPremisesInstance
    (
    -- * Request
      DeregisterOnPremisesInstance
    -- ** Request constructor
    , deregisterOnPremisesInstance
    -- ** Request lenses
    , dopiInstanceName

    -- * Response
    , DeregisterOnPremisesInstanceResponse
    -- ** Response constructor
    , deregisterOnPremisesInstanceResponse
    ) where

import           Network.AWS.CodeDeploy.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a deregister on-premises instance operation.
--
-- /See:/ 'deregisterOnPremisesInstance' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dopiInstanceName'
newtype DeregisterOnPremisesInstance = DeregisterOnPremisesInstance'
    { _dopiInstanceName :: Text
    } deriving (Eq,Read,Show)

-- | 'DeregisterOnPremisesInstance' smart constructor.
deregisterOnPremisesInstance :: Text -> DeregisterOnPremisesInstance
deregisterOnPremisesInstance pInstanceName =
    DeregisterOnPremisesInstance'
    { _dopiInstanceName = pInstanceName
    }

-- | The name of the on-premises instance to deregister.
dopiInstanceName :: Lens' DeregisterOnPremisesInstance Text
dopiInstanceName = lens _dopiInstanceName (\ s a -> s{_dopiInstanceName = a});

instance AWSRequest DeregisterOnPremisesInstance
         where
        type Sv DeregisterOnPremisesInstance = CodeDeploy
        type Rs DeregisterOnPremisesInstance =
             DeregisterOnPremisesInstanceResponse
        request = postJSON
        response
          = receiveNull DeregisterOnPremisesInstanceResponse'

instance ToHeaders DeregisterOnPremisesInstance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.DeregisterOnPremisesInstance"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeregisterOnPremisesInstance where
        toJSON DeregisterOnPremisesInstance'{..}
          = object ["instanceName" .= _dopiInstanceName]

instance ToPath DeregisterOnPremisesInstance where
        toPath = const "/"

instance ToQuery DeregisterOnPremisesInstance where
        toQuery = const mempty

-- | /See:/ 'deregisterOnPremisesInstanceResponse' smart constructor.
data DeregisterOnPremisesInstanceResponse =
    DeregisterOnPremisesInstanceResponse'
    deriving (Eq,Read,Show)

-- | 'DeregisterOnPremisesInstanceResponse' smart constructor.
deregisterOnPremisesInstanceResponse :: DeregisterOnPremisesInstanceResponse
deregisterOnPremisesInstanceResponse = DeregisterOnPremisesInstanceResponse'
