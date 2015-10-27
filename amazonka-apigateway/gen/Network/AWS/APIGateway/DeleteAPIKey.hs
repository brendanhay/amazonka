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
-- Module      : Network.AWS.APIGateway.DeleteAPIKey
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the ApiKey resource.
--
-- /See:/ <http://docs.aws.amazon.com/apigateway/api-reference/resource/DeleteAPIKey.html AWS API Reference> for DeleteAPIKey.
module Network.AWS.APIGateway.DeleteAPIKey
    (
    -- * Creating a Request
      deleteAPIKey
    , DeleteAPIKey
    -- * Request Lenses
    , dakApiKey

    -- * Destructuring the Response
    , deleteAPIKeyResponse
    , DeleteAPIKeyResponse
    ) where

import           Network.AWS.APIGateway.Types
import           Network.AWS.APIGateway.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | A request to delete the ApiKey resource.
--
-- /See:/ 'deleteAPIKey' smart constructor.
newtype DeleteAPIKey = DeleteAPIKey'
    { _dakApiKey :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteAPIKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dakApiKey'
deleteAPIKey
    :: Text -- ^ 'dakApiKey'
    -> DeleteAPIKey
deleteAPIKey pApiKey_ =
    DeleteAPIKey'
    { _dakApiKey = pApiKey_
    }

-- | The identifier of the ApiKey resource to be deleted.
dakApiKey :: Lens' DeleteAPIKey Text
dakApiKey = lens _dakApiKey (\ s a -> s{_dakApiKey = a});

instance AWSRequest DeleteAPIKey where
        type Rs DeleteAPIKey = DeleteAPIKeyResponse
        request = delete aPIGateway
        response = receiveNull DeleteAPIKeyResponse'

instance ToHeaders DeleteAPIKey where
        toHeaders = const mempty

instance ToPath DeleteAPIKey where
        toPath DeleteAPIKey'{..}
          = mconcat ["/apikeys/", toBS _dakApiKey]

instance ToQuery DeleteAPIKey where
        toQuery = const mempty

-- | /See:/ 'deleteAPIKeyResponse' smart constructor.
data DeleteAPIKeyResponse =
    DeleteAPIKeyResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteAPIKeyResponse' with the minimum fields required to make a request.
--
deleteAPIKeyResponse
    :: DeleteAPIKeyResponse
deleteAPIKeyResponse = DeleteAPIKeyResponse'
