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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a new 'Authorizer' resource to an existing 'RestApi' resource.
--
--
-- <http://docs.aws.amazon.com/cli/latest/reference/apigateway/create-authorizer.html AWS CLI>
module Network.AWS.APIGateway.CreateAuthorizer
    (
    -- * Creating a Request
      createAuthorizer
    , CreateAuthorizer
    -- * Request Lenses
    , caAuthorizerURI
    , caIdentityValidationExpression
    , caProviderARNs
    , caAuthorizerResultTtlInSeconds
    , caAuthType
    , caAuthorizerCredentials
    , caRestAPIId
    , caName
    , caType
    , caIdentitySource

    -- * Destructuring the Response
    , authorizer
    , Authorizer
    -- * Response Lenses
    , aAuthorizerURI
    , aIdentityValidationExpression
    , aProviderARNs
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

-- | Request to add a new 'Authorizer' to an existing 'RestApi' resource.
--
--
--
-- /See:/ 'createAuthorizer' smart constructor.
data CreateAuthorizer = CreateAuthorizer'
    { _caAuthorizerURI                :: !(Maybe Text)
    , _caIdentityValidationExpression :: !(Maybe Text)
    , _caProviderARNs                 :: !(Maybe [Text])
    , _caAuthorizerResultTtlInSeconds :: !(Maybe Int)
    , _caAuthType                     :: !(Maybe Text)
    , _caAuthorizerCredentials        :: !(Maybe Text)
    , _caRestAPIId                    :: !Text
    , _caName                         :: !Text
    , _caType                         :: !AuthorizerType
    , _caIdentitySource               :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateAuthorizer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caAuthorizerURI' - [Required] Specifies the authorizer's Uniform Resource Identifier (URI).
--
-- * 'caIdentityValidationExpression' - A validation expression for the incoming identity.
--
-- * 'caProviderARNs' - A list of the Cognito Your User Pool authorizer's provider ARNs.
--
-- * 'caAuthorizerResultTtlInSeconds' - The TTL of cached authorizer results.
--
-- * 'caAuthType' - Optional customer-defined field, used in Swagger imports/exports. Has no functional impact.
--
-- * 'caAuthorizerCredentials' - Specifies the credentials required for the authorizer, if any.
--
-- * 'caRestAPIId' - The string identifier of the associated 'RestApi' .
--
-- * 'caName' - [Required] The name of the authorizer.
--
-- * 'caType' - [Required] The type of the authorizer.
--
-- * 'caIdentitySource' - [Required] The source of the identity in an incoming request.
createAuthorizer
    :: Text -- ^ 'caRestAPIId'
    -> Text -- ^ 'caName'
    -> AuthorizerType -- ^ 'caType'
    -> Text -- ^ 'caIdentitySource'
    -> CreateAuthorizer
createAuthorizer pRestAPIId_ pName_ pType_ pIdentitySource_ =
    CreateAuthorizer'
    { _caAuthorizerURI = Nothing
    , _caIdentityValidationExpression = Nothing
    , _caProviderARNs = Nothing
    , _caAuthorizerResultTtlInSeconds = Nothing
    , _caAuthType = Nothing
    , _caAuthorizerCredentials = Nothing
    , _caRestAPIId = pRestAPIId_
    , _caName = pName_
    , _caType = pType_
    , _caIdentitySource = pIdentitySource_
    }

-- | [Required] Specifies the authorizer's Uniform Resource Identifier (URI).
caAuthorizerURI :: Lens' CreateAuthorizer (Maybe Text)
caAuthorizerURI = lens _caAuthorizerURI (\ s a -> s{_caAuthorizerURI = a});

-- | A validation expression for the incoming identity.
caIdentityValidationExpression :: Lens' CreateAuthorizer (Maybe Text)
caIdentityValidationExpression = lens _caIdentityValidationExpression (\ s a -> s{_caIdentityValidationExpression = a});

-- | A list of the Cognito Your User Pool authorizer's provider ARNs.
caProviderARNs :: Lens' CreateAuthorizer [Text]
caProviderARNs = lens _caProviderARNs (\ s a -> s{_caProviderARNs = a}) . _Default . _Coerce;

-- | The TTL of cached authorizer results.
caAuthorizerResultTtlInSeconds :: Lens' CreateAuthorizer (Maybe Int)
caAuthorizerResultTtlInSeconds = lens _caAuthorizerResultTtlInSeconds (\ s a -> s{_caAuthorizerResultTtlInSeconds = a});

-- | Optional customer-defined field, used in Swagger imports/exports. Has no functional impact.
caAuthType :: Lens' CreateAuthorizer (Maybe Text)
caAuthType = lens _caAuthType (\ s a -> s{_caAuthType = a});

-- | Specifies the credentials required for the authorizer, if any.
caAuthorizerCredentials :: Lens' CreateAuthorizer (Maybe Text)
caAuthorizerCredentials = lens _caAuthorizerCredentials (\ s a -> s{_caAuthorizerCredentials = a});

-- | The string identifier of the associated 'RestApi' .
caRestAPIId :: Lens' CreateAuthorizer Text
caRestAPIId = lens _caRestAPIId (\ s a -> s{_caRestAPIId = a});

-- | [Required] The name of the authorizer.
caName :: Lens' CreateAuthorizer Text
caName = lens _caName (\ s a -> s{_caName = a});

-- | [Required] The type of the authorizer.
caType :: Lens' CreateAuthorizer AuthorizerType
caType = lens _caType (\ s a -> s{_caType = a});

-- | [Required] The source of the identity in an incoming request.
caIdentitySource :: Lens' CreateAuthorizer Text
caIdentitySource = lens _caIdentitySource (\ s a -> s{_caIdentitySource = a});

instance AWSRequest CreateAuthorizer where
        type Rs CreateAuthorizer = Authorizer
        request = postJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable CreateAuthorizer

instance NFData CreateAuthorizer

instance ToHeaders CreateAuthorizer where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON CreateAuthorizer where
        toJSON CreateAuthorizer'{..}
          = object
              (catMaybes
                 [("authorizerUri" .=) <$> _caAuthorizerURI,
                  ("identityValidationExpression" .=) <$>
                    _caIdentityValidationExpression,
                  ("providerARNs" .=) <$> _caProviderARNs,
                  ("authorizerResultTtlInSeconds" .=) <$>
                    _caAuthorizerResultTtlInSeconds,
                  ("authType" .=) <$> _caAuthType,
                  ("authorizerCredentials" .=) <$>
                    _caAuthorizerCredentials,
                  Just ("name" .= _caName), Just ("type" .= _caType),
                  Just ("identitySource" .= _caIdentitySource)])

instance ToPath CreateAuthorizer where
        toPath CreateAuthorizer'{..}
          = mconcat
              ["/restapis/", toBS _caRestAPIId, "/authorizers"]

instance ToQuery CreateAuthorizer where
        toQuery = const mempty
