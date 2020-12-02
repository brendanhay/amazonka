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
-- Module      : Network.AWS.Greengrass.CreateFunctionDefinition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Lambda function definition which contains a list of Lambda functions and their configurations to be used in a group. You can create an initial version of the definition by providing a list of Lambda functions and their configurations now, or use ''CreateFunctionDefinitionVersion'' later.
module Network.AWS.Greengrass.CreateFunctionDefinition
    (
    -- * Creating a Request
      createFunctionDefinition
    , CreateFunctionDefinition
    -- * Request Lenses
    , cfdAmznClientToken
    , cfdInitialVersion
    , cfdName

    -- * Destructuring the Response
    , createFunctionDefinitionResponse
    , CreateFunctionDefinitionResponse
    -- * Response Lenses
    , cfdrsLatestVersionARN
    , cfdrsARN
    , cfdrsName
    , cfdrsCreationTimestamp
    , cfdrsId
    , cfdrsLatestVersion
    , cfdrsLastUpdatedTimestamp
    , cfdrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createFunctionDefinition' smart constructor.
data CreateFunctionDefinition = CreateFunctionDefinition'
  { _cfdAmznClientToken :: !(Maybe Text)
  , _cfdInitialVersion  :: !(Maybe FunctionDefinitionVersion)
  , _cfdName            :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateFunctionDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfdAmznClientToken' - A client token used to correlate requests and responses.
--
-- * 'cfdInitialVersion' - Information about the initial version of the function definition.
--
-- * 'cfdName' - The name of the function definition.
createFunctionDefinition
    :: CreateFunctionDefinition
createFunctionDefinition =
  CreateFunctionDefinition'
    { _cfdAmznClientToken = Nothing
    , _cfdInitialVersion = Nothing
    , _cfdName = Nothing
    }


-- | A client token used to correlate requests and responses.
cfdAmznClientToken :: Lens' CreateFunctionDefinition (Maybe Text)
cfdAmznClientToken = lens _cfdAmznClientToken (\ s a -> s{_cfdAmznClientToken = a})

-- | Information about the initial version of the function definition.
cfdInitialVersion :: Lens' CreateFunctionDefinition (Maybe FunctionDefinitionVersion)
cfdInitialVersion = lens _cfdInitialVersion (\ s a -> s{_cfdInitialVersion = a})

-- | The name of the function definition.
cfdName :: Lens' CreateFunctionDefinition (Maybe Text)
cfdName = lens _cfdName (\ s a -> s{_cfdName = a})

instance AWSRequest CreateFunctionDefinition where
        type Rs CreateFunctionDefinition =
             CreateFunctionDefinitionResponse
        request = postJSON greengrass
        response
          = receiveJSON
              (\ s h x ->
                 CreateFunctionDefinitionResponse' <$>
                   (x .?> "LatestVersionArn") <*> (x .?> "Arn") <*>
                     (x .?> "Name")
                     <*> (x .?> "CreationTimestamp")
                     <*> (x .?> "Id")
                     <*> (x .?> "LatestVersion")
                     <*> (x .?> "LastUpdatedTimestamp")
                     <*> (pure (fromEnum s)))

instance Hashable CreateFunctionDefinition where

instance NFData CreateFunctionDefinition where

instance ToHeaders CreateFunctionDefinition where
        toHeaders CreateFunctionDefinition'{..}
          = mconcat
              ["X-Amzn-Client-Token" =# _cfdAmznClientToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToJSON CreateFunctionDefinition where
        toJSON CreateFunctionDefinition'{..}
          = object
              (catMaybes
                 [("InitialVersion" .=) <$> _cfdInitialVersion,
                  ("Name" .=) <$> _cfdName])

instance ToPath CreateFunctionDefinition where
        toPath = const "/greengrass/definition/functions"

instance ToQuery CreateFunctionDefinition where
        toQuery = const mempty

-- | /See:/ 'createFunctionDefinitionResponse' smart constructor.
data CreateFunctionDefinitionResponse = CreateFunctionDefinitionResponse'
  { _cfdrsLatestVersionARN     :: !(Maybe Text)
  , _cfdrsARN                  :: !(Maybe Text)
  , _cfdrsName                 :: !(Maybe Text)
  , _cfdrsCreationTimestamp    :: !(Maybe Text)
  , _cfdrsId                   :: !(Maybe Text)
  , _cfdrsLatestVersion        :: !(Maybe Text)
  , _cfdrsLastUpdatedTimestamp :: !(Maybe Text)
  , _cfdrsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateFunctionDefinitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfdrsLatestVersionARN' - The ARN of the latest version of the definition.
--
-- * 'cfdrsARN' - The ARN of the definition.
--
-- * 'cfdrsName' - The name of the definition.
--
-- * 'cfdrsCreationTimestamp' - The time, in milliseconds since the epoch, when the definition was created.
--
-- * 'cfdrsId' - The ID of the definition.
--
-- * 'cfdrsLatestVersion' - The latest version of the definition.
--
-- * 'cfdrsLastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last updated.
--
-- * 'cfdrsResponseStatus' - -- | The response status code.
createFunctionDefinitionResponse
    :: Int -- ^ 'cfdrsResponseStatus'
    -> CreateFunctionDefinitionResponse
createFunctionDefinitionResponse pResponseStatus_ =
  CreateFunctionDefinitionResponse'
    { _cfdrsLatestVersionARN = Nothing
    , _cfdrsARN = Nothing
    , _cfdrsName = Nothing
    , _cfdrsCreationTimestamp = Nothing
    , _cfdrsId = Nothing
    , _cfdrsLatestVersion = Nothing
    , _cfdrsLastUpdatedTimestamp = Nothing
    , _cfdrsResponseStatus = pResponseStatus_
    }


-- | The ARN of the latest version of the definition.
cfdrsLatestVersionARN :: Lens' CreateFunctionDefinitionResponse (Maybe Text)
cfdrsLatestVersionARN = lens _cfdrsLatestVersionARN (\ s a -> s{_cfdrsLatestVersionARN = a})

-- | The ARN of the definition.
cfdrsARN :: Lens' CreateFunctionDefinitionResponse (Maybe Text)
cfdrsARN = lens _cfdrsARN (\ s a -> s{_cfdrsARN = a})

-- | The name of the definition.
cfdrsName :: Lens' CreateFunctionDefinitionResponse (Maybe Text)
cfdrsName = lens _cfdrsName (\ s a -> s{_cfdrsName = a})

-- | The time, in milliseconds since the epoch, when the definition was created.
cfdrsCreationTimestamp :: Lens' CreateFunctionDefinitionResponse (Maybe Text)
cfdrsCreationTimestamp = lens _cfdrsCreationTimestamp (\ s a -> s{_cfdrsCreationTimestamp = a})

-- | The ID of the definition.
cfdrsId :: Lens' CreateFunctionDefinitionResponse (Maybe Text)
cfdrsId = lens _cfdrsId (\ s a -> s{_cfdrsId = a})

-- | The latest version of the definition.
cfdrsLatestVersion :: Lens' CreateFunctionDefinitionResponse (Maybe Text)
cfdrsLatestVersion = lens _cfdrsLatestVersion (\ s a -> s{_cfdrsLatestVersion = a})

-- | The time, in milliseconds since the epoch, when the definition was last updated.
cfdrsLastUpdatedTimestamp :: Lens' CreateFunctionDefinitionResponse (Maybe Text)
cfdrsLastUpdatedTimestamp = lens _cfdrsLastUpdatedTimestamp (\ s a -> s{_cfdrsLastUpdatedTimestamp = a})

-- | -- | The response status code.
cfdrsResponseStatus :: Lens' CreateFunctionDefinitionResponse Int
cfdrsResponseStatus = lens _cfdrsResponseStatus (\ s a -> s{_cfdrsResponseStatus = a})

instance NFData CreateFunctionDefinitionResponse
         where
