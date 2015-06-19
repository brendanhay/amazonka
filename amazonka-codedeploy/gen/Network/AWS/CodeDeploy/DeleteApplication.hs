{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CodeDeploy.DeleteApplication
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

-- | Deletes an application.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_DeleteApplication.html>
module Network.AWS.CodeDeploy.DeleteApplication
    (
    -- * Request
      DeleteApplication
    -- ** Request constructor
    , deleteApplication
    -- ** Request lenses
    , daApplicationName

    -- * Response
    , DeleteApplicationResponse
    -- ** Response constructor
    , deleteApplicationResponse
    ) where

import Network.AWS.CodeDeploy.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteApplication' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'daApplicationName'
newtype DeleteApplication = DeleteApplication'{_daApplicationName :: Text} deriving (Eq, Read, Show)

-- | 'DeleteApplication' smart constructor.
deleteApplication :: Text -> DeleteApplication
deleteApplication pApplicationName = DeleteApplication'{_daApplicationName = pApplicationName};

-- | The name of an existing AWS CodeDeploy application associated with the
-- applicable IAM user or AWS account.
daApplicationName :: Lens' DeleteApplication Text
daApplicationName = lens _daApplicationName (\ s a -> s{_daApplicationName = a});

instance AWSRequest DeleteApplication where
        type Sv DeleteApplication = CodeDeploy
        type Rs DeleteApplication = DeleteApplicationResponse
        request = postJSON
        response = receiveNull DeleteApplicationResponse'

instance ToHeaders DeleteApplication where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.DeleteApplication" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteApplication where
        toJSON DeleteApplication'{..}
          = object ["applicationName" .= _daApplicationName]

instance ToPath DeleteApplication where
        toPath = const "/"

instance ToQuery DeleteApplication where
        toQuery = const mempty

-- | /See:/ 'deleteApplicationResponse' smart constructor.
data DeleteApplicationResponse = DeleteApplicationResponse' deriving (Eq, Read, Show)

-- | 'DeleteApplicationResponse' smart constructor.
deleteApplicationResponse :: DeleteApplicationResponse
deleteApplicationResponse = DeleteApplicationResponse';
