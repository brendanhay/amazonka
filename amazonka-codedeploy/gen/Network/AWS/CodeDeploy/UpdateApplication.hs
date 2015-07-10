{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.UpdateApplication
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Changes an existing application\'s name.
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

import           Network.AWS.CodeDeploy.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of an update application operation.
--
-- /See:/ 'updateApplication' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uaNewApplicationName'
--
-- * 'uaApplicationName'
data UpdateApplication = UpdateApplication'
    { _uaNewApplicationName :: !(Maybe Text)
    , _uaApplicationName    :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateApplication' smart constructor.
updateApplication :: UpdateApplication
updateApplication =
    UpdateApplication'
    { _uaNewApplicationName = Nothing
    , _uaApplicationName = Nothing
    }

-- | The new name that you want to change the application to.
uaNewApplicationName :: Lens' UpdateApplication (Maybe Text)
uaNewApplicationName = lens _uaNewApplicationName (\ s a -> s{_uaNewApplicationName = a});

-- | The current name of the application that you want to change.
uaApplicationName :: Lens' UpdateApplication (Maybe Text)
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
data UpdateApplicationResponse =
    UpdateApplicationResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateApplicationResponse' smart constructor.
updateApplicationResponse :: UpdateApplicationResponse
updateApplicationResponse = UpdateApplicationResponse'
