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
-- Module      : Network.AWS.APIGateway.DeleteAuthorizer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing 'Authorizer' resource.
--
--
-- <http://docs.aws.amazon.com/cli/latest/reference/apigateway/delete-authorizer.html AWS CLI>
module Network.AWS.APIGateway.DeleteAuthorizer
    (
    -- * Creating a Request
      deleteAuthorizer
    , DeleteAuthorizer
    -- * Request Lenses
    , daRestAPIId
    , daAuthorizerId

    -- * Destructuring the Response
    , deleteAuthorizerResponse
    , DeleteAuthorizerResponse
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request to delete an existing 'Authorizer' resource.
--
--
--
-- /See:/ 'deleteAuthorizer' smart constructor.
data DeleteAuthorizer = DeleteAuthorizer'
  { _daRestAPIId    :: !Text
  , _daAuthorizerId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAuthorizer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'daAuthorizerId' - [Required] The identifier of the 'Authorizer' resource.
deleteAuthorizer
    :: Text -- ^ 'daRestAPIId'
    -> Text -- ^ 'daAuthorizerId'
    -> DeleteAuthorizer
deleteAuthorizer pRestAPIId_ pAuthorizerId_ =
  DeleteAuthorizer'
    {_daRestAPIId = pRestAPIId_, _daAuthorizerId = pAuthorizerId_}


-- | [Required] The string identifier of the associated 'RestApi' .
daRestAPIId :: Lens' DeleteAuthorizer Text
daRestAPIId = lens _daRestAPIId (\ s a -> s{_daRestAPIId = a})

-- | [Required] The identifier of the 'Authorizer' resource.
daAuthorizerId :: Lens' DeleteAuthorizer Text
daAuthorizerId = lens _daAuthorizerId (\ s a -> s{_daAuthorizerId = a})

instance AWSRequest DeleteAuthorizer where
        type Rs DeleteAuthorizer = DeleteAuthorizerResponse
        request = delete apiGateway
        response = receiveNull DeleteAuthorizerResponse'

instance Hashable DeleteAuthorizer where

instance NFData DeleteAuthorizer where

instance ToHeaders DeleteAuthorizer where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath DeleteAuthorizer where
        toPath DeleteAuthorizer'{..}
          = mconcat
              ["/restapis/", toBS _daRestAPIId, "/authorizers/",
               toBS _daAuthorizerId]

instance ToQuery DeleteAuthorizer where
        toQuery = const mempty

-- | /See:/ 'deleteAuthorizerResponse' smart constructor.
data DeleteAuthorizerResponse =
  DeleteAuthorizerResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAuthorizerResponse' with the minimum fields required to make a request.
--
deleteAuthorizerResponse
    :: DeleteAuthorizerResponse
deleteAuthorizerResponse = DeleteAuthorizerResponse'


instance NFData DeleteAuthorizerResponse where
