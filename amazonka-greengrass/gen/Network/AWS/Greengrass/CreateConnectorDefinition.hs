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
-- Module      : Network.AWS.Greengrass.CreateConnectorDefinition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a connector definition. You may provide the initial version of the connector definition now or use ''CreateConnectorDefinitionVersion'' at a later time.
module Network.AWS.Greengrass.CreateConnectorDefinition
    (
    -- * Creating a Request
      createConnectorDefinition
    , CreateConnectorDefinition
    -- * Request Lenses
    , cAmznClientToken
    , cInitialVersion
    , cName

    -- * Destructuring the Response
    , createConnectorDefinitionResponse
    , CreateConnectorDefinitionResponse
    -- * Response Lenses
    , crersLatestVersionARN
    , crersARN
    , crersName
    , crersCreationTimestamp
    , crersId
    , crersLatestVersion
    , crersLastUpdatedTimestamp
    , crersResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createConnectorDefinition' smart constructor.
data CreateConnectorDefinition = CreateConnectorDefinition'
  { _cAmznClientToken :: !(Maybe Text)
  , _cInitialVersion  :: !(Maybe ConnectorDefinitionVersion)
  , _cName            :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateConnectorDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cAmznClientToken' - A client token used to correlate requests and responses.
--
-- * 'cInitialVersion' - Information about the initial version of the connector definition.
--
-- * 'cName' - The name of the connector definition.
createConnectorDefinition
    :: CreateConnectorDefinition
createConnectorDefinition =
  CreateConnectorDefinition'
    {_cAmznClientToken = Nothing, _cInitialVersion = Nothing, _cName = Nothing}


-- | A client token used to correlate requests and responses.
cAmznClientToken :: Lens' CreateConnectorDefinition (Maybe Text)
cAmznClientToken = lens _cAmznClientToken (\ s a -> s{_cAmznClientToken = a})

-- | Information about the initial version of the connector definition.
cInitialVersion :: Lens' CreateConnectorDefinition (Maybe ConnectorDefinitionVersion)
cInitialVersion = lens _cInitialVersion (\ s a -> s{_cInitialVersion = a})

-- | The name of the connector definition.
cName :: Lens' CreateConnectorDefinition (Maybe Text)
cName = lens _cName (\ s a -> s{_cName = a})

instance AWSRequest CreateConnectorDefinition where
        type Rs CreateConnectorDefinition =
             CreateConnectorDefinitionResponse
        request = postJSON greengrass
        response
          = receiveJSON
              (\ s h x ->
                 CreateConnectorDefinitionResponse' <$>
                   (x .?> "LatestVersionArn") <*> (x .?> "Arn") <*>
                     (x .?> "Name")
                     <*> (x .?> "CreationTimestamp")
                     <*> (x .?> "Id")
                     <*> (x .?> "LatestVersion")
                     <*> (x .?> "LastUpdatedTimestamp")
                     <*> (pure (fromEnum s)))

instance Hashable CreateConnectorDefinition where

instance NFData CreateConnectorDefinition where

instance ToHeaders CreateConnectorDefinition where
        toHeaders CreateConnectorDefinition'{..}
          = mconcat
              ["X-Amzn-Client-Token" =# _cAmznClientToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToJSON CreateConnectorDefinition where
        toJSON CreateConnectorDefinition'{..}
          = object
              (catMaybes
                 [("InitialVersion" .=) <$> _cInitialVersion,
                  ("Name" .=) <$> _cName])

instance ToPath CreateConnectorDefinition where
        toPath = const "/greengrass/definition/connectors"

instance ToQuery CreateConnectorDefinition where
        toQuery = const mempty

-- | /See:/ 'createConnectorDefinitionResponse' smart constructor.
data CreateConnectorDefinitionResponse = CreateConnectorDefinitionResponse'
  { _crersLatestVersionARN     :: !(Maybe Text)
  , _crersARN                  :: !(Maybe Text)
  , _crersName                 :: !(Maybe Text)
  , _crersCreationTimestamp    :: !(Maybe Text)
  , _crersId                   :: !(Maybe Text)
  , _crersLatestVersion        :: !(Maybe Text)
  , _crersLastUpdatedTimestamp :: !(Maybe Text)
  , _crersResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateConnectorDefinitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crersLatestVersionARN' - The ARN of the latest version of the definition.
--
-- * 'crersARN' - The ARN of the definition.
--
-- * 'crersName' - The name of the definition.
--
-- * 'crersCreationTimestamp' - The time, in milliseconds since the epoch, when the definition was created.
--
-- * 'crersId' - The ID of the definition.
--
-- * 'crersLatestVersion' - The latest version of the definition.
--
-- * 'crersLastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last updated.
--
-- * 'crersResponseStatus' - -- | The response status code.
createConnectorDefinitionResponse
    :: Int -- ^ 'crersResponseStatus'
    -> CreateConnectorDefinitionResponse
createConnectorDefinitionResponse pResponseStatus_ =
  CreateConnectorDefinitionResponse'
    { _crersLatestVersionARN = Nothing
    , _crersARN = Nothing
    , _crersName = Nothing
    , _crersCreationTimestamp = Nothing
    , _crersId = Nothing
    , _crersLatestVersion = Nothing
    , _crersLastUpdatedTimestamp = Nothing
    , _crersResponseStatus = pResponseStatus_
    }


-- | The ARN of the latest version of the definition.
crersLatestVersionARN :: Lens' CreateConnectorDefinitionResponse (Maybe Text)
crersLatestVersionARN = lens _crersLatestVersionARN (\ s a -> s{_crersLatestVersionARN = a})

-- | The ARN of the definition.
crersARN :: Lens' CreateConnectorDefinitionResponse (Maybe Text)
crersARN = lens _crersARN (\ s a -> s{_crersARN = a})

-- | The name of the definition.
crersName :: Lens' CreateConnectorDefinitionResponse (Maybe Text)
crersName = lens _crersName (\ s a -> s{_crersName = a})

-- | The time, in milliseconds since the epoch, when the definition was created.
crersCreationTimestamp :: Lens' CreateConnectorDefinitionResponse (Maybe Text)
crersCreationTimestamp = lens _crersCreationTimestamp (\ s a -> s{_crersCreationTimestamp = a})

-- | The ID of the definition.
crersId :: Lens' CreateConnectorDefinitionResponse (Maybe Text)
crersId = lens _crersId (\ s a -> s{_crersId = a})

-- | The latest version of the definition.
crersLatestVersion :: Lens' CreateConnectorDefinitionResponse (Maybe Text)
crersLatestVersion = lens _crersLatestVersion (\ s a -> s{_crersLatestVersion = a})

-- | The time, in milliseconds since the epoch, when the definition was last updated.
crersLastUpdatedTimestamp :: Lens' CreateConnectorDefinitionResponse (Maybe Text)
crersLastUpdatedTimestamp = lens _crersLastUpdatedTimestamp (\ s a -> s{_crersLastUpdatedTimestamp = a})

-- | -- | The response status code.
crersResponseStatus :: Lens' CreateConnectorDefinitionResponse Int
crersResponseStatus = lens _crersResponseStatus (\ s a -> s{_crersResponseStatus = a})

instance NFData CreateConnectorDefinitionResponse
         where
