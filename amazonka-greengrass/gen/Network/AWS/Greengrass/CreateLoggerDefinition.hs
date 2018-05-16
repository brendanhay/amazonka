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
-- Module      : Network.AWS.Greengrass.CreateLoggerDefinition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a logger definition. You may provide the initial version of the logger definition now or use ''CreateLoggerDefinitionVersion'' at a later time.
module Network.AWS.Greengrass.CreateLoggerDefinition
    (
    -- * Creating a Request
      createLoggerDefinition
    , CreateLoggerDefinition
    -- * Request Lenses
    , cldAmznClientToken
    , cldInitialVersion
    , cldName

    -- * Destructuring the Response
    , createLoggerDefinitionResponse
    , CreateLoggerDefinitionResponse
    -- * Response Lenses
    , cldrsLatestVersionARN
    , cldrsARN
    , cldrsName
    , cldrsCreationTimestamp
    , cldrsId
    , cldrsLatestVersion
    , cldrsLastUpdatedTimestamp
    , cldrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createLoggerDefinition' smart constructor.
data CreateLoggerDefinition = CreateLoggerDefinition'
  { _cldAmznClientToken :: !(Maybe Text)
  , _cldInitialVersion  :: !(Maybe LoggerDefinitionVersion)
  , _cldName            :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateLoggerDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cldAmznClientToken' - A client token used to correlate requests and responses.
--
-- * 'cldInitialVersion' - Information about the initial version of the logger definition.
--
-- * 'cldName' - The name of the logger definition.
createLoggerDefinition
    :: CreateLoggerDefinition
createLoggerDefinition =
  CreateLoggerDefinition'
    { _cldAmznClientToken = Nothing
    , _cldInitialVersion = Nothing
    , _cldName = Nothing
    }


-- | A client token used to correlate requests and responses.
cldAmznClientToken :: Lens' CreateLoggerDefinition (Maybe Text)
cldAmznClientToken = lens _cldAmznClientToken (\ s a -> s{_cldAmznClientToken = a})

-- | Information about the initial version of the logger definition.
cldInitialVersion :: Lens' CreateLoggerDefinition (Maybe LoggerDefinitionVersion)
cldInitialVersion = lens _cldInitialVersion (\ s a -> s{_cldInitialVersion = a})

-- | The name of the logger definition.
cldName :: Lens' CreateLoggerDefinition (Maybe Text)
cldName = lens _cldName (\ s a -> s{_cldName = a})

instance AWSRequest CreateLoggerDefinition where
        type Rs CreateLoggerDefinition =
             CreateLoggerDefinitionResponse
        request = postJSON greengrass
        response
          = receiveJSON
              (\ s h x ->
                 CreateLoggerDefinitionResponse' <$>
                   (x .?> "LatestVersionArn") <*> (x .?> "Arn") <*>
                     (x .?> "Name")
                     <*> (x .?> "CreationTimestamp")
                     <*> (x .?> "Id")
                     <*> (x .?> "LatestVersion")
                     <*> (x .?> "LastUpdatedTimestamp")
                     <*> (pure (fromEnum s)))

instance Hashable CreateLoggerDefinition where

instance NFData CreateLoggerDefinition where

instance ToHeaders CreateLoggerDefinition where
        toHeaders CreateLoggerDefinition'{..}
          = mconcat
              ["X-Amzn-Client-Token" =# _cldAmznClientToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToJSON CreateLoggerDefinition where
        toJSON CreateLoggerDefinition'{..}
          = object
              (catMaybes
                 [("InitialVersion" .=) <$> _cldInitialVersion,
                  ("Name" .=) <$> _cldName])

instance ToPath CreateLoggerDefinition where
        toPath = const "/greengrass/definition/loggers"

instance ToQuery CreateLoggerDefinition where
        toQuery = const mempty

-- | /See:/ 'createLoggerDefinitionResponse' smart constructor.
data CreateLoggerDefinitionResponse = CreateLoggerDefinitionResponse'
  { _cldrsLatestVersionARN     :: !(Maybe Text)
  , _cldrsARN                  :: !(Maybe Text)
  , _cldrsName                 :: !(Maybe Text)
  , _cldrsCreationTimestamp    :: !(Maybe Text)
  , _cldrsId                   :: !(Maybe Text)
  , _cldrsLatestVersion        :: !(Maybe Text)
  , _cldrsLastUpdatedTimestamp :: !(Maybe Text)
  , _cldrsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateLoggerDefinitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cldrsLatestVersionARN' - The ARN of the latest version of the definition.
--
-- * 'cldrsARN' - The ARN of the definition.
--
-- * 'cldrsName' - The name of the definition.
--
-- * 'cldrsCreationTimestamp' - The time, in milliseconds since the epoch, when the definition was created.
--
-- * 'cldrsId' - The ID of the definition.
--
-- * 'cldrsLatestVersion' - The latest version of the definition.
--
-- * 'cldrsLastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last updated.
--
-- * 'cldrsResponseStatus' - -- | The response status code.
createLoggerDefinitionResponse
    :: Int -- ^ 'cldrsResponseStatus'
    -> CreateLoggerDefinitionResponse
createLoggerDefinitionResponse pResponseStatus_ =
  CreateLoggerDefinitionResponse'
    { _cldrsLatestVersionARN = Nothing
    , _cldrsARN = Nothing
    , _cldrsName = Nothing
    , _cldrsCreationTimestamp = Nothing
    , _cldrsId = Nothing
    , _cldrsLatestVersion = Nothing
    , _cldrsLastUpdatedTimestamp = Nothing
    , _cldrsResponseStatus = pResponseStatus_
    }


-- | The ARN of the latest version of the definition.
cldrsLatestVersionARN :: Lens' CreateLoggerDefinitionResponse (Maybe Text)
cldrsLatestVersionARN = lens _cldrsLatestVersionARN (\ s a -> s{_cldrsLatestVersionARN = a})

-- | The ARN of the definition.
cldrsARN :: Lens' CreateLoggerDefinitionResponse (Maybe Text)
cldrsARN = lens _cldrsARN (\ s a -> s{_cldrsARN = a})

-- | The name of the definition.
cldrsName :: Lens' CreateLoggerDefinitionResponse (Maybe Text)
cldrsName = lens _cldrsName (\ s a -> s{_cldrsName = a})

-- | The time, in milliseconds since the epoch, when the definition was created.
cldrsCreationTimestamp :: Lens' CreateLoggerDefinitionResponse (Maybe Text)
cldrsCreationTimestamp = lens _cldrsCreationTimestamp (\ s a -> s{_cldrsCreationTimestamp = a})

-- | The ID of the definition.
cldrsId :: Lens' CreateLoggerDefinitionResponse (Maybe Text)
cldrsId = lens _cldrsId (\ s a -> s{_cldrsId = a})

-- | The latest version of the definition.
cldrsLatestVersion :: Lens' CreateLoggerDefinitionResponse (Maybe Text)
cldrsLatestVersion = lens _cldrsLatestVersion (\ s a -> s{_cldrsLatestVersion = a})

-- | The time, in milliseconds since the epoch, when the definition was last updated.
cldrsLastUpdatedTimestamp :: Lens' CreateLoggerDefinitionResponse (Maybe Text)
cldrsLastUpdatedTimestamp = lens _cldrsLastUpdatedTimestamp (\ s a -> s{_cldrsLastUpdatedTimestamp = a})

-- | -- | The response status code.
cldrsResponseStatus :: Lens' CreateLoggerDefinitionResponse Int
cldrsResponseStatus = lens _cldrsResponseStatus (\ s a -> s{_cldrsResponseStatus = a})

instance NFData CreateLoggerDefinitionResponse where
