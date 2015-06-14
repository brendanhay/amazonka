{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CodeDeploy.UpdateApplication
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

-- | Changes an existing application\'s name.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_UpdateApplication.html>
module Network.AWS.CodeDeploy.UpdateApplication
    (
    -- * Request
      UpdateApplication
    -- ** Request constructor
    , updateApplication
    -- ** Request lenses
    , uaNewApplicationName
    , uaApplicationName

    -- * Response
    , UpdateApplicationResponse
    -- ** Response constructor
    , updateApplicationResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CodeDeploy.Types

-- | /See:/ 'updateApplication' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uaNewApplicationName'
--
-- * 'uaApplicationName'
data UpdateApplication = UpdateApplication'{_uaNewApplicationName :: Text, _uaApplicationName :: Text} deriving (Eq, Read, Show)

-- | 'UpdateApplication' smart constructor.
updateApplication :: Text -> Text -> UpdateApplication
updateApplication pNewApplicationName pApplicationName = UpdateApplication'{_uaNewApplicationName = pNewApplicationName, _uaApplicationName = pApplicationName};

-- | The new name that you want to change the application to.
uaNewApplicationName :: Lens' UpdateApplication Text
uaNewApplicationName = lens _uaNewApplicationName (\ s a -> s{_uaNewApplicationName = a});

-- | The current name of the application that you want to change.
uaApplicationName :: Lens' UpdateApplication Text
uaApplicationName = lens _uaApplicationName (\ s a -> s{_uaApplicationName = a});

instance AWSRequest UpdateApplication where
        type Sv UpdateApplication = CodeDeploy
        type Rs UpdateApplication = UpdateApplicationResponse
        request = postJSON
        response = receiveNull UpdateApplicationResponse'

instance ToHeaders UpdateApplication where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.UpdateApplication" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateApplication where
        toJSON UpdateApplication'{..}
          = object
              ["newApplicationName" .= _uaNewApplicationName,
               "applicationName" .= _uaApplicationName]

instance ToPath UpdateApplication where
        toPath = const "/"

instance ToQuery UpdateApplication where
        toQuery = const mempty

-- | /See:/ 'updateApplicationResponse' smart constructor.
data UpdateApplicationResponse = UpdateApplicationResponse' deriving (Eq, Read, Show)

-- | 'UpdateApplicationResponse' smart constructor.
updateApplicationResponse :: UpdateApplicationResponse
updateApplicationResponse = UpdateApplicationResponse';
