{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.DeleteApplication
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an application.
--
--
module Network.AWS.CodeDeploy.DeleteApplication
    (
    -- * Creating a Request
      deleteApplication
    , DeleteApplication
    -- * Request Lenses
    , daApplicationName

    -- * Destructuring the Response
    , deleteApplicationResponse
    , DeleteApplicationResponse
    ) where

import Network.AWS.CodeDeploy.Types
import Network.AWS.CodeDeploy.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a DeleteApplication operation.
--
--
--
-- /See:/ 'deleteApplication' smart constructor.
newtype DeleteApplication = DeleteApplication'
  { _daApplicationName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteApplication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daApplicationName' - The name of an AWS CodeDeploy application associated with the applicable IAM user or AWS account.
deleteApplication
    :: Text -- ^ 'daApplicationName'
    -> DeleteApplication
deleteApplication pApplicationName_ =
  DeleteApplication' {_daApplicationName = pApplicationName_}


-- | The name of an AWS CodeDeploy application associated with the applicable IAM user or AWS account.
daApplicationName :: Lens' DeleteApplication Text
daApplicationName = lens _daApplicationName (\ s a -> s{_daApplicationName = a})

instance AWSRequest DeleteApplication where
        type Rs DeleteApplication = DeleteApplicationResponse
        request = postJSON codeDeploy
        response = receiveNull DeleteApplicationResponse'

instance Hashable DeleteApplication where

instance NFData DeleteApplication where

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
          = object
              (catMaybes
                 [Just ("applicationName" .= _daApplicationName)])

instance ToPath DeleteApplication where
        toPath = const "/"

instance ToQuery DeleteApplication where
        toQuery = const mempty

-- | /See:/ 'deleteApplicationResponse' smart constructor.
data DeleteApplicationResponse =
  DeleteApplicationResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteApplicationResponse' with the minimum fields required to make a request.
--
deleteApplicationResponse
    :: DeleteApplicationResponse
deleteApplicationResponse = DeleteApplicationResponse'


instance NFData DeleteApplicationResponse where
