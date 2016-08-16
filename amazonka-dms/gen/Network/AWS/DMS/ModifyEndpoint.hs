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
-- Module      : Network.AWS.DMS.ModifyEndpoint
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified endpoint.
module Network.AWS.DMS.ModifyEndpoint
    (
    -- * Creating a Request
      modifyEndpoint
    , ModifyEndpoint
    -- * Request Lenses
    , meServerName
    , meCertificateARN
    , meExtraConnectionAttributes
    , meEndpointType
    , meUsername
    , meEngineName
    , meSSLMode
    , mePassword
    , meDatabaseName
    , meEndpointIdentifier
    , mePort
    , meEndpointARN

    -- * Destructuring the Response
    , modifyEndpointResponse
    , ModifyEndpointResponse
    -- * Response Lenses
    , mersEndpoint
    , mersResponseStatus
    ) where

import           Network.AWS.DMS.Types
import           Network.AWS.DMS.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'modifyEndpoint' smart constructor.
data ModifyEndpoint = ModifyEndpoint'
    { _meServerName                :: !(Maybe Text)
    , _meCertificateARN            :: !(Maybe Text)
    , _meExtraConnectionAttributes :: !(Maybe Text)
    , _meEndpointType              :: !(Maybe ReplicationEndpointTypeValue)
    , _meUsername                  :: !(Maybe Text)
    , _meEngineName                :: !(Maybe Text)
    , _meSSLMode                   :: !(Maybe DmsSSLModeValue)
    , _mePassword                  :: !(Maybe (Sensitive Text))
    , _meDatabaseName              :: !(Maybe Text)
    , _meEndpointIdentifier        :: !(Maybe Text)
    , _mePort                      :: !(Maybe Int)
    , _meEndpointARN               :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ModifyEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'meServerName'
--
-- * 'meCertificateARN'
--
-- * 'meExtraConnectionAttributes'
--
-- * 'meEndpointType'
--
-- * 'meUsername'
--
-- * 'meEngineName'
--
-- * 'meSSLMode'
--
-- * 'mePassword'
--
-- * 'meDatabaseName'
--
-- * 'meEndpointIdentifier'
--
-- * 'mePort'
--
-- * 'meEndpointARN'
modifyEndpoint
    :: Text -- ^ 'meEndpointARN'
    -> ModifyEndpoint
modifyEndpoint pEndpointARN_ =
    ModifyEndpoint'
    { _meServerName = Nothing
    , _meCertificateARN = Nothing
    , _meExtraConnectionAttributes = Nothing
    , _meEndpointType = Nothing
    , _meUsername = Nothing
    , _meEngineName = Nothing
    , _meSSLMode = Nothing
    , _mePassword = Nothing
    , _meDatabaseName = Nothing
    , _meEndpointIdentifier = Nothing
    , _mePort = Nothing
    , _meEndpointARN = pEndpointARN_
    }

-- | The name of the server where the endpoint database resides.
meServerName :: Lens' ModifyEndpoint (Maybe Text)
meServerName = lens _meServerName (\ s a -> s{_meServerName = a});

-- | The Amazon Resource Name (ARN) of the certificate used for SSL connection.
meCertificateARN :: Lens' ModifyEndpoint (Maybe Text)
meCertificateARN = lens _meCertificateARN (\ s a -> s{_meCertificateARN = a});

-- | Additional attributes associated with the connection.
meExtraConnectionAttributes :: Lens' ModifyEndpoint (Maybe Text)
meExtraConnectionAttributes = lens _meExtraConnectionAttributes (\ s a -> s{_meExtraConnectionAttributes = a});

-- | The type of endpoint.
meEndpointType :: Lens' ModifyEndpoint (Maybe ReplicationEndpointTypeValue)
meEndpointType = lens _meEndpointType (\ s a -> s{_meEndpointType = a});

-- | The user name to be used to login to the endpoint database.
meUsername :: Lens' ModifyEndpoint (Maybe Text)
meUsername = lens _meUsername (\ s a -> s{_meUsername = a});

-- | The type of engine for the endpoint. Valid values include MYSQL, ORACLE, POSTGRES, MARIADB, AURORA, REDSHIFT, and SQLSERVER.
meEngineName :: Lens' ModifyEndpoint (Maybe Text)
meEngineName = lens _meEngineName (\ s a -> s{_meEngineName = a});

-- | The SSL mode to be used.
--
-- SSL mode can be one of four values: none, require, verify-ca, verify-full.
--
-- The default value is none.
meSSLMode :: Lens' ModifyEndpoint (Maybe DmsSSLModeValue)
meSSLMode = lens _meSSLMode (\ s a -> s{_meSSLMode = a});

-- | The password to be used to login to the endpoint database.
mePassword :: Lens' ModifyEndpoint (Maybe Text)
mePassword = lens _mePassword (\ s a -> s{_mePassword = a}) . mapping _Sensitive;

-- | The name of the endpoint database.
meDatabaseName :: Lens' ModifyEndpoint (Maybe Text)
meDatabaseName = lens _meDatabaseName (\ s a -> s{_meDatabaseName = a});

-- | The database endpoint identifier. Identifiers must begin with a letter; must contain only ASCII letters, digits, and hyphens; and must not end with a hyphen or contain two consecutive hyphens.
meEndpointIdentifier :: Lens' ModifyEndpoint (Maybe Text)
meEndpointIdentifier = lens _meEndpointIdentifier (\ s a -> s{_meEndpointIdentifier = a});

-- | The port used by the endpoint database.
mePort :: Lens' ModifyEndpoint (Maybe Int)
mePort = lens _mePort (\ s a -> s{_mePort = a});

-- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
meEndpointARN :: Lens' ModifyEndpoint Text
meEndpointARN = lens _meEndpointARN (\ s a -> s{_meEndpointARN = a});

instance AWSRequest ModifyEndpoint where
        type Rs ModifyEndpoint = ModifyEndpointResponse
        request = postJSON dms
        response
          = receiveJSON
              (\ s h x ->
                 ModifyEndpointResponse' <$>
                   (x .?> "Endpoint") <*> (pure (fromEnum s)))

instance Hashable ModifyEndpoint

instance NFData ModifyEndpoint

instance ToHeaders ModifyEndpoint where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDMSv20160101.ModifyEndpoint" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ModifyEndpoint where
        toJSON ModifyEndpoint'{..}
          = object
              (catMaybes
                 [("ServerName" .=) <$> _meServerName,
                  ("CertificateArn" .=) <$> _meCertificateARN,
                  ("ExtraConnectionAttributes" .=) <$>
                    _meExtraConnectionAttributes,
                  ("EndpointType" .=) <$> _meEndpointType,
                  ("Username" .=) <$> _meUsername,
                  ("EngineName" .=) <$> _meEngineName,
                  ("SslMode" .=) <$> _meSSLMode,
                  ("Password" .=) <$> _mePassword,
                  ("DatabaseName" .=) <$> _meDatabaseName,
                  ("EndpointIdentifier" .=) <$> _meEndpointIdentifier,
                  ("Port" .=) <$> _mePort,
                  Just ("EndpointArn" .= _meEndpointARN)])

instance ToPath ModifyEndpoint where
        toPath = const "/"

instance ToQuery ModifyEndpoint where
        toQuery = const mempty

-- |
--
-- /See:/ 'modifyEndpointResponse' smart constructor.
data ModifyEndpointResponse = ModifyEndpointResponse'
    { _mersEndpoint       :: !(Maybe Endpoint)
    , _mersResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ModifyEndpointResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mersEndpoint'
--
-- * 'mersResponseStatus'
modifyEndpointResponse
    :: Int -- ^ 'mersResponseStatus'
    -> ModifyEndpointResponse
modifyEndpointResponse pResponseStatus_ =
    ModifyEndpointResponse'
    { _mersEndpoint = Nothing
    , _mersResponseStatus = pResponseStatus_
    }

-- | The modified endpoint.
mersEndpoint :: Lens' ModifyEndpointResponse (Maybe Endpoint)
mersEndpoint = lens _mersEndpoint (\ s a -> s{_mersEndpoint = a});

-- | The response status code.
mersResponseStatus :: Lens' ModifyEndpointResponse Int
mersResponseStatus = lens _mersResponseStatus (\ s a -> s{_mersResponseStatus = a});

instance NFData ModifyEndpointResponse
