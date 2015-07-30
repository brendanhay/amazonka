{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.DeleteApplication
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes an application.
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

import           Network.AWS.CodeDeploy.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a delete application operation.
--
-- /See:/ 'deleteApplication' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'daApplicationName'
newtype DeleteApplication = DeleteApplication'
    { _daApplicationName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteApplication' smart constructor.
deleteApplication :: Text -> DeleteApplication
deleteApplication pApplicationName_ =
    DeleteApplication'
    { _daApplicationName = pApplicationName_
    }

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
        toPath = const mempty

instance ToQuery DeleteApplication where
        toQuery = const mempty

-- | /See:/ 'deleteApplicationResponse' smart constructor.
data DeleteApplicationResponse =
    DeleteApplicationResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteApplicationResponse' smart constructor.
deleteApplicationResponse :: DeleteApplicationResponse
deleteApplicationResponse = DeleteApplicationResponse'
