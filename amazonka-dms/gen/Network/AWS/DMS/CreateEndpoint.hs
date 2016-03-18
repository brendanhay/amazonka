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
-- Module      : Network.AWS.DMS.CreateEndpoint
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an endpoint using the provided settings.
--
--
-- /See:/ <http://docs.aws.amazon.com/apigateway/api-reference/resource/CreateEndpoint.html AWS API Reference> for CreateEndpoint.
module Network.AWS.DMS.CreateEndpoint
    (
    -- * Creating a Request
      createEndpoint
    , CreateEndpoint
    -- * Request Lenses
    , ceExtraConnectionAttributes
    , ceKMSKeyId
    , ceDatabaseName
    , ceTags
    , ceEndpointIdentifier
    , ceEndpointType
    , ceEngineName
    , ceUsername
    , cePassword
    , ceServerName
    , cePort

    -- * Destructuring the Response
    , createEndpointResponse
    , CreateEndpointResponse
    -- * Response Lenses
    , cersEndpoint
    , cersResponseStatus
    ) where

import           Network.AWS.DMS.Types
import           Network.AWS.DMS.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createEndpoint' smart constructor.
data CreateEndpoint = CreateEndpoint'
    { _ceExtraConnectionAttributes :: !(Maybe Text)
    , _ceKMSKeyId                  :: !(Maybe Text)
    , _ceDatabaseName              :: !(Maybe Text)
    , _ceTags                      :: !(Maybe [Tag])
    , _ceEndpointIdentifier        :: !Text
    , _ceEndpointType              :: !ReplicationEndpointTypeValue
    , _ceEngineName                :: !Text
    , _ceUsername                  :: !Text
    , _cePassword                  :: !(Sensitive Text)
    , _ceServerName                :: !Text
    , _cePort                      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ceExtraConnectionAttributes'
--
-- * 'ceKMSKeyId'
--
-- * 'ceDatabaseName'
--
-- * 'ceTags'
--
-- * 'ceEndpointIdentifier'
--
-- * 'ceEndpointType'
--
-- * 'ceEngineName'
--
-- * 'ceUsername'
--
-- * 'cePassword'
--
-- * 'ceServerName'
--
-- * 'cePort'
createEndpoint
    :: Text -- ^ 'ceEndpointIdentifier'
    -> ReplicationEndpointTypeValue -- ^ 'ceEndpointType'
    -> Text -- ^ 'ceEngineName'
    -> Text -- ^ 'ceUsername'
    -> Text -- ^ 'cePassword'
    -> Text -- ^ 'ceServerName'
    -> Int -- ^ 'cePort'
    -> CreateEndpoint
createEndpoint pEndpointIdentifier_ pEndpointType_ pEngineName_ pUsername_ pPassword_ pServerName_ pPort_ =
    CreateEndpoint'
    { _ceExtraConnectionAttributes = Nothing
    , _ceKMSKeyId = Nothing
    , _ceDatabaseName = Nothing
    , _ceTags = Nothing
    , _ceEndpointIdentifier = pEndpointIdentifier_
    , _ceEndpointType = pEndpointType_
    , _ceEngineName = pEngineName_
    , _ceUsername = pUsername_
    , _cePassword = _Sensitive # pPassword_
    , _ceServerName = pServerName_
    , _cePort = pPort_
    }

-- | Additional attributes associated with the connection.
ceExtraConnectionAttributes :: Lens' CreateEndpoint (Maybe Text)
ceExtraConnectionAttributes = lens _ceExtraConnectionAttributes (\ s a -> s{_ceExtraConnectionAttributes = a});

-- | The KMS key identifier that will be used to encrypt the connection
-- parameters. If you do not specify a value for the KmsKeyId parameter,
-- then AWS DMS will use your default encryption key. AWS KMS creates the
-- default encryption key for your AWS account. Your AWS account has a
-- different default encryption key for each AWS region.
ceKMSKeyId :: Lens' CreateEndpoint (Maybe Text)
ceKMSKeyId = lens _ceKMSKeyId (\ s a -> s{_ceKMSKeyId = a});

-- | The name of the endpoint database.
ceDatabaseName :: Lens' CreateEndpoint (Maybe Text)
ceDatabaseName = lens _ceDatabaseName (\ s a -> s{_ceDatabaseName = a});

-- | Tags to be added to the endpoint.
ceTags :: Lens' CreateEndpoint [Tag]
ceTags = lens _ceTags (\ s a -> s{_ceTags = a}) . _Default . _Coerce;

-- | The database endpoint identifier. Identifiers must begin with a letter;
-- must contain only ASCII letters, digits, and hyphens; and must not end
-- with a hyphen or contain two consecutive hyphens.
ceEndpointIdentifier :: Lens' CreateEndpoint Text
ceEndpointIdentifier = lens _ceEndpointIdentifier (\ s a -> s{_ceEndpointIdentifier = a});

-- | The type of endpoint.
ceEndpointType :: Lens' CreateEndpoint ReplicationEndpointTypeValue
ceEndpointType = lens _ceEndpointType (\ s a -> s{_ceEndpointType = a});

-- | The type of engine for the endpoint. Valid values include MYSQL, ORACLE,
-- POSTGRES, MARIADB, AURORA, SQLSERVER.
ceEngineName :: Lens' CreateEndpoint Text
ceEngineName = lens _ceEngineName (\ s a -> s{_ceEngineName = a});

-- | The user name to be used to login to the endpoint database.
ceUsername :: Lens' CreateEndpoint Text
ceUsername = lens _ceUsername (\ s a -> s{_ceUsername = a});

-- | The password to be used to login to the endpoint database.
cePassword :: Lens' CreateEndpoint Text
cePassword = lens _cePassword (\ s a -> s{_cePassword = a}) . _Sensitive;

-- | The name of the server where the endpoint database resides.
ceServerName :: Lens' CreateEndpoint Text
ceServerName = lens _ceServerName (\ s a -> s{_ceServerName = a});

-- | The port used by the endpoint database.
cePort :: Lens' CreateEndpoint Int
cePort = lens _cePort (\ s a -> s{_cePort = a});

instance AWSRequest CreateEndpoint where
        type Rs CreateEndpoint = CreateEndpointResponse
        request = postJSON dMS
        response
          = receiveJSON
              (\ s h x ->
                 CreateEndpointResponse' <$>
                   (x .?> "Endpoint") <*> (pure (fromEnum s)))

instance ToHeaders CreateEndpoint where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDMSv20160101.CreateEndpoint" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateEndpoint where
        toJSON CreateEndpoint'{..}
          = object
              (catMaybes
                 [("ExtraConnectionAttributes" .=) <$>
                    _ceExtraConnectionAttributes,
                  ("KmsKeyId" .=) <$> _ceKMSKeyId,
                  ("DatabaseName" .=) <$> _ceDatabaseName,
                  ("Tags" .=) <$> _ceTags,
                  Just ("EndpointIdentifier" .= _ceEndpointIdentifier),
                  Just ("EndpointType" .= _ceEndpointType),
                  Just ("EngineName" .= _ceEngineName),
                  Just ("Username" .= _ceUsername),
                  Just ("Password" .= _cePassword),
                  Just ("ServerName" .= _ceServerName),
                  Just ("Port" .= _cePort)])

instance ToPath CreateEndpoint where
        toPath = const "/"

instance ToQuery CreateEndpoint where
        toQuery = const mempty

-- | /See:/ 'createEndpointResponse' smart constructor.
data CreateEndpointResponse = CreateEndpointResponse'
    { _cersEndpoint       :: !(Maybe Endpoint)
    , _cersResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateEndpointResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cersEndpoint'
--
-- * 'cersResponseStatus'
createEndpointResponse
    :: Int -- ^ 'cersResponseStatus'
    -> CreateEndpointResponse
createEndpointResponse pResponseStatus_ =
    CreateEndpointResponse'
    { _cersEndpoint = Nothing
    , _cersResponseStatus = pResponseStatus_
    }

-- | The endpoint that was created.
cersEndpoint :: Lens' CreateEndpointResponse (Maybe Endpoint)
cersEndpoint = lens _cersEndpoint (\ s a -> s{_cersEndpoint = a});

-- | The response status code.
cersResponseStatus :: Lens' CreateEndpointResponse Int
cersResponseStatus = lens _cersResponseStatus (\ s a -> s{_cersResponseStatus = a});
