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
-- Module      : Network.AWS.IoT.UpdateAuthorizer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an authorizer.
--
--
module Network.AWS.IoT.UpdateAuthorizer
    (
    -- * Creating a Request
      updateAuthorizer
    , UpdateAuthorizer
    -- * Request Lenses
    , uaStatus
    , uaAuthorizerFunctionARN
    , uaTokenSigningPublicKeys
    , uaTokenKeyName
    , uaAuthorizerName

    -- * Destructuring the Response
    , updateAuthorizerResponse
    , UpdateAuthorizerResponse
    -- * Response Lenses
    , uarsAuthorizerName
    , uarsAuthorizerARN
    , uarsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateAuthorizer' smart constructor.
data UpdateAuthorizer = UpdateAuthorizer'
  { _uaStatus                 :: !(Maybe AuthorizerStatus)
  , _uaAuthorizerFunctionARN  :: !(Maybe Text)
  , _uaTokenSigningPublicKeys :: !(Maybe (Map Text Text))
  , _uaTokenKeyName           :: !(Maybe Text)
  , _uaAuthorizerName         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateAuthorizer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uaStatus' - The status of the update authorizer request.
--
-- * 'uaAuthorizerFunctionARN' - The ARN of the authorizer's Lambda function.
--
-- * 'uaTokenSigningPublicKeys' - The public keys used to verify the token signature.
--
-- * 'uaTokenKeyName' - The key used to extract the token from the HTTP headers.
--
-- * 'uaAuthorizerName' - The authorizer name.
updateAuthorizer
    :: Text -- ^ 'uaAuthorizerName'
    -> UpdateAuthorizer
updateAuthorizer pAuthorizerName_ =
  UpdateAuthorizer'
    { _uaStatus = Nothing
    , _uaAuthorizerFunctionARN = Nothing
    , _uaTokenSigningPublicKeys = Nothing
    , _uaTokenKeyName = Nothing
    , _uaAuthorizerName = pAuthorizerName_
    }


-- | The status of the update authorizer request.
uaStatus :: Lens' UpdateAuthorizer (Maybe AuthorizerStatus)
uaStatus = lens _uaStatus (\ s a -> s{_uaStatus = a})

-- | The ARN of the authorizer's Lambda function.
uaAuthorizerFunctionARN :: Lens' UpdateAuthorizer (Maybe Text)
uaAuthorizerFunctionARN = lens _uaAuthorizerFunctionARN (\ s a -> s{_uaAuthorizerFunctionARN = a})

-- | The public keys used to verify the token signature.
uaTokenSigningPublicKeys :: Lens' UpdateAuthorizer (HashMap Text Text)
uaTokenSigningPublicKeys = lens _uaTokenSigningPublicKeys (\ s a -> s{_uaTokenSigningPublicKeys = a}) . _Default . _Map

-- | The key used to extract the token from the HTTP headers.
uaTokenKeyName :: Lens' UpdateAuthorizer (Maybe Text)
uaTokenKeyName = lens _uaTokenKeyName (\ s a -> s{_uaTokenKeyName = a})

-- | The authorizer name.
uaAuthorizerName :: Lens' UpdateAuthorizer Text
uaAuthorizerName = lens _uaAuthorizerName (\ s a -> s{_uaAuthorizerName = a})

instance AWSRequest UpdateAuthorizer where
        type Rs UpdateAuthorizer = UpdateAuthorizerResponse
        request = putJSON ioT
        response
          = receiveJSON
              (\ s h x ->
                 UpdateAuthorizerResponse' <$>
                   (x .?> "authorizerName") <*> (x .?> "authorizerArn")
                     <*> (pure (fromEnum s)))

instance Hashable UpdateAuthorizer where

instance NFData UpdateAuthorizer where

instance ToHeaders UpdateAuthorizer where
        toHeaders = const mempty

instance ToJSON UpdateAuthorizer where
        toJSON UpdateAuthorizer'{..}
          = object
              (catMaybes
                 [("status" .=) <$> _uaStatus,
                  ("authorizerFunctionArn" .=) <$>
                    _uaAuthorizerFunctionARN,
                  ("tokenSigningPublicKeys" .=) <$>
                    _uaTokenSigningPublicKeys,
                  ("tokenKeyName" .=) <$> _uaTokenKeyName])

instance ToPath UpdateAuthorizer where
        toPath UpdateAuthorizer'{..}
          = mconcat ["/authorizer/", toBS _uaAuthorizerName]

instance ToQuery UpdateAuthorizer where
        toQuery = const mempty

-- | /See:/ 'updateAuthorizerResponse' smart constructor.
data UpdateAuthorizerResponse = UpdateAuthorizerResponse'
  { _uarsAuthorizerName :: !(Maybe Text)
  , _uarsAuthorizerARN  :: !(Maybe Text)
  , _uarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateAuthorizerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uarsAuthorizerName' - The authorizer name.
--
-- * 'uarsAuthorizerARN' - The authorizer ARN.
--
-- * 'uarsResponseStatus' - -- | The response status code.
updateAuthorizerResponse
    :: Int -- ^ 'uarsResponseStatus'
    -> UpdateAuthorizerResponse
updateAuthorizerResponse pResponseStatus_ =
  UpdateAuthorizerResponse'
    { _uarsAuthorizerName = Nothing
    , _uarsAuthorizerARN = Nothing
    , _uarsResponseStatus = pResponseStatus_
    }


-- | The authorizer name.
uarsAuthorizerName :: Lens' UpdateAuthorizerResponse (Maybe Text)
uarsAuthorizerName = lens _uarsAuthorizerName (\ s a -> s{_uarsAuthorizerName = a})

-- | The authorizer ARN.
uarsAuthorizerARN :: Lens' UpdateAuthorizerResponse (Maybe Text)
uarsAuthorizerARN = lens _uarsAuthorizerARN (\ s a -> s{_uarsAuthorizerARN = a})

-- | -- | The response status code.
uarsResponseStatus :: Lens' UpdateAuthorizerResponse Int
uarsResponseStatus = lens _uarsResponseStatus (\ s a -> s{_uarsResponseStatus = a})

instance NFData UpdateAuthorizerResponse where
