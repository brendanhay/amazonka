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
-- Module      : Network.AWS.APIGateway.UpdateAuthorizer
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing < Authorizer> resource.
module Network.AWS.APIGateway.UpdateAuthorizer
    (
    -- * Creating a Request
      updateAuthorizer
    , UpdateAuthorizer
    -- * Request Lenses
    , uaaPatchOperations
    , uaaRestAPIId
    , uaaAuthorizerId

    -- * Destructuring the Response
    , authorizer
    , Authorizer
    -- * Response Lenses
    , aAuthorizerURI
    , aIdentityValidationExpression
    , aName
    , aId
    , aAuthorizerResultTtlInSeconds
    , aAuthType
    , aType
    , aIdentitySource
    , aAuthorizerCredentials
    ) where

import           Network.AWS.APIGateway.Types
import           Network.AWS.APIGateway.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Request to update an existing < Authorizer> resource.
--
-- /See:/ 'updateAuthorizer' smart constructor.
data UpdateAuthorizer = UpdateAuthorizer'
    { _uaaPatchOperations :: !(Maybe [PatchOperation])
    , _uaaRestAPIId       :: !Text
    , _uaaAuthorizerId    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateAuthorizer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uaaPatchOperations'
--
-- * 'uaaRestAPIId'
--
-- * 'uaaAuthorizerId'
updateAuthorizer
    :: Text -- ^ 'uaaRestAPIId'
    -> Text -- ^ 'uaaAuthorizerId'
    -> UpdateAuthorizer
updateAuthorizer pRestAPIId_ pAuthorizerId_ =
    UpdateAuthorizer'
    { _uaaPatchOperations = Nothing
    , _uaaRestAPIId = pRestAPIId_
    , _uaaAuthorizerId = pAuthorizerId_
    }

-- | A list of operations describing the updates to apply to the specified resource. The patches are applied in the order specified in the list.
uaaPatchOperations :: Lens' UpdateAuthorizer [PatchOperation]
uaaPatchOperations = lens _uaaPatchOperations (\ s a -> s{_uaaPatchOperations = a}) . _Default . _Coerce;

-- | The < RestApi> identifier for the < Authorizer> resource.
uaaRestAPIId :: Lens' UpdateAuthorizer Text
uaaRestAPIId = lens _uaaRestAPIId (\ s a -> s{_uaaRestAPIId = a});

-- | The identifier of the < Authorizer> resource.
uaaAuthorizerId :: Lens' UpdateAuthorizer Text
uaaAuthorizerId = lens _uaaAuthorizerId (\ s a -> s{_uaaAuthorizerId = a});

instance AWSRequest UpdateAuthorizer where
        type Rs UpdateAuthorizer = Authorizer
        request = patchJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable UpdateAuthorizer

instance NFData UpdateAuthorizer

instance ToHeaders UpdateAuthorizer where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON UpdateAuthorizer where
        toJSON UpdateAuthorizer'{..}
          = object
              (catMaybes
                 [("patchOperations" .=) <$> _uaaPatchOperations])

instance ToPath UpdateAuthorizer where
        toPath UpdateAuthorizer'{..}
          = mconcat
              ["/restapis/", toBS _uaaRestAPIId, "/authorizers/",
               toBS _uaaAuthorizerId]

instance ToQuery UpdateAuthorizer where
        toQuery = const mempty
