{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CodeDeploy.GetOnPremisesInstance
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

-- | Gets information about an on-premises instance.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_GetOnPremisesInstance.html>
module Network.AWS.CodeDeploy.GetOnPremisesInstance
    (
    -- * Request
      GetOnPremisesInstance
    -- ** Request constructor
    , getOnPremisesInstance
    -- ** Request lenses
    , gopiInstanceName

    -- * Response
    , GetOnPremisesInstanceResponse
    -- ** Response constructor
    , getOnPremisesInstanceResponse
    -- ** Response lenses
    , gopirInstanceInfo
    ) where

import Network.AWS.CodeDeploy.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getOnPremisesInstance' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gopiInstanceName'
newtype GetOnPremisesInstance = GetOnPremisesInstance'{_gopiInstanceName :: Text} deriving (Eq, Read, Show)

-- | 'GetOnPremisesInstance' smart constructor.
getOnPremisesInstance :: Text -> GetOnPremisesInstance
getOnPremisesInstance pInstanceName = GetOnPremisesInstance'{_gopiInstanceName = pInstanceName};

-- | The name of the on-premises instance to get information about
gopiInstanceName :: Lens' GetOnPremisesInstance Text
gopiInstanceName = lens _gopiInstanceName (\ s a -> s{_gopiInstanceName = a});

instance AWSRequest GetOnPremisesInstance where
        type Sv GetOnPremisesInstance = CodeDeploy
        type Rs GetOnPremisesInstance =
             GetOnPremisesInstanceResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetOnPremisesInstanceResponse' <$>
                   (x .?> "instanceInfo"))

instance ToHeaders GetOnPremisesInstance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.GetOnPremisesInstance" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetOnPremisesInstance where
        toJSON GetOnPremisesInstance'{..}
          = object ["instanceName" .= _gopiInstanceName]

instance ToPath GetOnPremisesInstance where
        toPath = const "/"

instance ToQuery GetOnPremisesInstance where
        toQuery = const mempty

-- | /See:/ 'getOnPremisesInstanceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gopirInstanceInfo'
newtype GetOnPremisesInstanceResponse = GetOnPremisesInstanceResponse'{_gopirInstanceInfo :: Maybe InstanceInfo} deriving (Eq, Read, Show)

-- | 'GetOnPremisesInstanceResponse' smart constructor.
getOnPremisesInstanceResponse :: GetOnPremisesInstanceResponse
getOnPremisesInstanceResponse = GetOnPremisesInstanceResponse'{_gopirInstanceInfo = Nothing};

-- | Information about the on-premises instance.
gopirInstanceInfo :: Lens' GetOnPremisesInstanceResponse (Maybe InstanceInfo)
gopirInstanceInfo = lens _gopirInstanceInfo (\ s a -> s{_gopirInstanceInfo = a});
