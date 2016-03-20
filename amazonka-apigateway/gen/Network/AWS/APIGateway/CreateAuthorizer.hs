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
-- Module      : Network.AWS.APIGateway.CreateAuthorizer
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.APIGateway.CreateAuthorizer
    (
    -- * Creating a Request
      createAuthorizer
    , CreateAuthorizer
    -- * Request Lenses
    , caIdentityValidationExpression
    , caAuthorizerResultTtlInSeconds
    , caAuthorizerCredentials
    , caRestAPIId
    , caName
    , caType
    , caAuthorizerURI
    , caIdentitySource

    -- * Destructuring the Response
    , authorizer
    , Authorizer
    -- * Response Lenses
    , aAuthorizerURI
    , aIdentityValidationExpression
    , aName
    , aId
    , aAuthorizerResultTtlInSeconds
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

-- | /See:/ 'createAuthorizer' smart constructor.
data CreateAuthorizer = CreateAuthorizer'
    { _caIdentityValidationExpression :: !(Maybe Text)
    , _caAuthorizerResultTtlInSeconds :: !(Maybe Int)
    , _caAuthorizerCredentials        :: !(Maybe Text)
    , _caRestAPIId                    :: !Text
    , _caName                         :: !Text
    , _caType                         :: !AuthorizerType
    , _caAuthorizerURI                :: !Text
    , _caIdentitySource               :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateAuthorizer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caIdentityValidationExpression'
--
-- * 'caAuthorizerResultTtlInSeconds'
--
-- * 'caAuthorizerCredentials'
--
-- * 'caRestAPIId'
--
-- * 'caName'
--
-- * 'caType'
--
-- * 'caAuthorizerURI'
--
-- * 'caIdentitySource'
createAuthorizer
    :: Text -- ^ 'caRestAPIId'
    -> Text -- ^ 'caName'
    -> AuthorizerType -- ^ 'caType'
    -> Text -- ^ 'caAuthorizerURI'
    -> Text -- ^ 'caIdentitySource'
    -> CreateAuthorizer
createAuthorizer pRestAPIId_ pName_ pType_ pAuthorizerURI_ pIdentitySource_ =
    CreateAuthorizer'
    { _caIdentityValidationExpression = Nothing
    , _caAuthorizerResultTtlInSeconds = Nothing
    , _caAuthorizerCredentials = Nothing
    , _caRestAPIId = pRestAPIId_
    , _caName = pName_
    , _caType = pType_
    , _caAuthorizerURI = pAuthorizerURI_
    , _caIdentitySource = pIdentitySource_
    }

-- | A validation expression for the incoming identity.
caIdentityValidationExpression :: Lens' CreateAuthorizer (Maybe Text)
caIdentityValidationExpression = lens _caIdentityValidationExpression (\ s a -> s{_caIdentityValidationExpression = a});

-- | The TTL of cached authorizer results.
caAuthorizerResultTtlInSeconds :: Lens' CreateAuthorizer (Maybe Int)
caAuthorizerResultTtlInSeconds = lens _caAuthorizerResultTtlInSeconds (\ s a -> s{_caAuthorizerResultTtlInSeconds = a});

-- | Specifies the credentials required for the authorizer, if any.
caAuthorizerCredentials :: Lens' CreateAuthorizer (Maybe Text)
caAuthorizerCredentials = lens _caAuthorizerCredentials (\ s a -> s{_caAuthorizerCredentials = a});

-- | Undocumented member.
caRestAPIId :: Lens' CreateAuthorizer Text
caRestAPIId = lens _caRestAPIId (\ s a -> s{_caRestAPIId = a});

-- | [Required] The name of the authorizer.
caName :: Lens' CreateAuthorizer Text
caName = lens _caName (\ s a -> s{_caName = a});

-- | [Required] The type of the authorizer.
caType :: Lens' CreateAuthorizer AuthorizerType
caType = lens _caType (\ s a -> s{_caType = a});

-- | [Required] Specifies the authorizer\'s Uniform Resource Identifier
-- (URI).
caAuthorizerURI :: Lens' CreateAuthorizer Text
caAuthorizerURI = lens _caAuthorizerURI (\ s a -> s{_caAuthorizerURI = a});

-- | [Required] The source of the identity in an incoming request.
caIdentitySource :: Lens' CreateAuthorizer Text
caIdentitySource = lens _caIdentitySource (\ s a -> s{_caIdentitySource = a});

instance AWSRequest CreateAuthorizer where
        type Rs CreateAuthorizer = Authorizer
        request = postJSON aPIGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable CreateAuthorizer

instance ToHeaders CreateAuthorizer where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON CreateAuthorizer where
        toJSON CreateAuthorizer'{..}
          = object
              (catMaybes
                 [("identityValidationExpression" .=) <$>
                    _caIdentityValidationExpression,
                  ("authorizerResultTtlInSeconds" .=) <$>
                    _caAuthorizerResultTtlInSeconds,
                  ("authorizerCredentials" .=) <$>
                    _caAuthorizerCredentials,
                  Just ("name" .= _caName), Just ("type" .= _caType),
                  Just ("authorizerUri" .= _caAuthorizerURI),
                  Just ("identitySource" .= _caIdentitySource)])

instance ToPath CreateAuthorizer where
        toPath CreateAuthorizer'{..}
          = mconcat
              ["/restapis/", toBS _caRestAPIId, "/authorizers"]

instance ToQuery CreateAuthorizer where
        toQuery = const mempty
