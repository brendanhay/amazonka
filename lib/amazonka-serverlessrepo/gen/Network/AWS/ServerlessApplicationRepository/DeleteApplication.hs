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
-- Module      : Network.AWS.ServerlessApplicationRepository.DeleteApplication
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified application.
--
--
module Network.AWS.ServerlessApplicationRepository.DeleteApplication
    (
    -- * Creating a Request
      deleteApplication
    , DeleteApplication
    -- * Request Lenses
    , daApplicationId

    -- * Destructuring the Response
    , deleteApplicationResponse
    , DeleteApplicationResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServerlessApplicationRepository.Types
import Network.AWS.ServerlessApplicationRepository.Types.Product

-- | /See:/ 'deleteApplication' smart constructor.
newtype DeleteApplication = DeleteApplication'
  { _daApplicationId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteApplication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daApplicationId' - The ID of the application to get.
deleteApplication
    :: Text -- ^ 'daApplicationId'
    -> DeleteApplication
deleteApplication pApplicationId_ =
  DeleteApplication' {_daApplicationId = pApplicationId_}


-- | The ID of the application to get.
daApplicationId :: Lens' DeleteApplication Text
daApplicationId = lens _daApplicationId (\ s a -> s{_daApplicationId = a})

instance AWSRequest DeleteApplication where
        type Rs DeleteApplication = DeleteApplicationResponse
        request = delete serverlessApplicationRepository
        response = receiveNull DeleteApplicationResponse'

instance Hashable DeleteApplication where

instance NFData DeleteApplication where

instance ToHeaders DeleteApplication where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteApplication where
        toPath DeleteApplication'{..}
          = mconcat ["/applications/", toBS _daApplicationId]

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
