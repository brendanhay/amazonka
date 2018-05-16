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
-- Module      : Network.AWS.EMR.CreateSecurityConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a security configuration, which is stored in the service and can be specified when a cluster is created.
--
--
module Network.AWS.EMR.CreateSecurityConfiguration
    (
    -- * Creating a Request
      createSecurityConfiguration
    , CreateSecurityConfiguration
    -- * Request Lenses
    , cscName
    , cscSecurityConfiguration

    -- * Destructuring the Response
    , createSecurityConfigurationResponse
    , CreateSecurityConfigurationResponse
    -- * Response Lenses
    , cscrsResponseStatus
    , cscrsName
    , cscrsCreationDateTime
    ) where

import Network.AWS.EMR.Types
import Network.AWS.EMR.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createSecurityConfiguration' smart constructor.
data CreateSecurityConfiguration = CreateSecurityConfiguration'
  { _cscName                  :: !Text
  , _cscSecurityConfiguration :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateSecurityConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cscName' - The name of the security configuration.
--
-- * 'cscSecurityConfiguration' - The security configuration details in JSON format. For JSON parameters and examples, see <http://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-security-configurations.html Use Security Configurations to Set Up Cluster Security> in the /Amazon EMR Management Guide/ .
createSecurityConfiguration
    :: Text -- ^ 'cscName'
    -> Text -- ^ 'cscSecurityConfiguration'
    -> CreateSecurityConfiguration
createSecurityConfiguration pName_ pSecurityConfiguration_ =
  CreateSecurityConfiguration'
    {_cscName = pName_, _cscSecurityConfiguration = pSecurityConfiguration_}


-- | The name of the security configuration.
cscName :: Lens' CreateSecurityConfiguration Text
cscName = lens _cscName (\ s a -> s{_cscName = a})

-- | The security configuration details in JSON format. For JSON parameters and examples, see <http://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-security-configurations.html Use Security Configurations to Set Up Cluster Security> in the /Amazon EMR Management Guide/ .
cscSecurityConfiguration :: Lens' CreateSecurityConfiguration Text
cscSecurityConfiguration = lens _cscSecurityConfiguration (\ s a -> s{_cscSecurityConfiguration = a})

instance AWSRequest CreateSecurityConfiguration where
        type Rs CreateSecurityConfiguration =
             CreateSecurityConfigurationResponse
        request = postJSON emr
        response
          = receiveJSON
              (\ s h x ->
                 CreateSecurityConfigurationResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "Name") <*>
                     (x .:> "CreationDateTime"))

instance Hashable CreateSecurityConfiguration where

instance NFData CreateSecurityConfiguration where

instance ToHeaders CreateSecurityConfiguration where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ElasticMapReduce.CreateSecurityConfiguration" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateSecurityConfiguration where
        toJSON CreateSecurityConfiguration'{..}
          = object
              (catMaybes
                 [Just ("Name" .= _cscName),
                  Just
                    ("SecurityConfiguration" .=
                       _cscSecurityConfiguration)])

instance ToPath CreateSecurityConfiguration where
        toPath = const "/"

instance ToQuery CreateSecurityConfiguration where
        toQuery = const mempty

-- | /See:/ 'createSecurityConfigurationResponse' smart constructor.
data CreateSecurityConfigurationResponse = CreateSecurityConfigurationResponse'
  { _cscrsResponseStatus   :: !Int
  , _cscrsName             :: !Text
  , _cscrsCreationDateTime :: !POSIX
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateSecurityConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cscrsResponseStatus' - -- | The response status code.
--
-- * 'cscrsName' - The name of the security configuration.
--
-- * 'cscrsCreationDateTime' - The date and time the security configuration was created.
createSecurityConfigurationResponse
    :: Int -- ^ 'cscrsResponseStatus'
    -> Text -- ^ 'cscrsName'
    -> UTCTime -- ^ 'cscrsCreationDateTime'
    -> CreateSecurityConfigurationResponse
createSecurityConfigurationResponse pResponseStatus_ pName_ pCreationDateTime_ =
  CreateSecurityConfigurationResponse'
    { _cscrsResponseStatus = pResponseStatus_
    , _cscrsName = pName_
    , _cscrsCreationDateTime = _Time # pCreationDateTime_
    }


-- | -- | The response status code.
cscrsResponseStatus :: Lens' CreateSecurityConfigurationResponse Int
cscrsResponseStatus = lens _cscrsResponseStatus (\ s a -> s{_cscrsResponseStatus = a})

-- | The name of the security configuration.
cscrsName :: Lens' CreateSecurityConfigurationResponse Text
cscrsName = lens _cscrsName (\ s a -> s{_cscrsName = a})

-- | The date and time the security configuration was created.
cscrsCreationDateTime :: Lens' CreateSecurityConfigurationResponse UTCTime
cscrsCreationDateTime = lens _cscrsCreationDateTime (\ s a -> s{_cscrsCreationDateTime = a}) . _Time

instance NFData CreateSecurityConfigurationResponse
         where
