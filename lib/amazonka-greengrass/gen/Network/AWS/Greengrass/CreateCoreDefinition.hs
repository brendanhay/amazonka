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
-- Module      : Network.AWS.Greengrass.CreateCoreDefinition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a core definition. You may provide the initial version of the core definition now or use ''CreateCoreDefinitionVersion'' at a later time. AWS Greengrass groups must each contain exactly one AWS Greengrass core.
module Network.AWS.Greengrass.CreateCoreDefinition
    (
    -- * Creating a Request
      createCoreDefinition
    , CreateCoreDefinition
    -- * Request Lenses
    , ccdAmznClientToken
    , ccdInitialVersion
    , ccdName

    -- * Destructuring the Response
    , createCoreDefinitionResponse
    , CreateCoreDefinitionResponse
    -- * Response Lenses
    , ccdrsLatestVersionARN
    , ccdrsARN
    , ccdrsName
    , ccdrsCreationTimestamp
    , ccdrsId
    , ccdrsLatestVersion
    , ccdrsLastUpdatedTimestamp
    , ccdrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Information needed to create a core definition.
--
-- /See:/ 'createCoreDefinition' smart constructor.
data CreateCoreDefinition = CreateCoreDefinition'
  { _ccdAmznClientToken :: !(Maybe Text)
  , _ccdInitialVersion  :: !(Maybe CoreDefinitionVersion)
  , _ccdName            :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateCoreDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccdAmznClientToken' - A client token used to correlate requests and responses.
--
-- * 'ccdInitialVersion' - Information about the initial version of the core definition.
--
-- * 'ccdName' - The name of the core definition.
createCoreDefinition
    :: CreateCoreDefinition
createCoreDefinition =
  CreateCoreDefinition'
    { _ccdAmznClientToken = Nothing
    , _ccdInitialVersion = Nothing
    , _ccdName = Nothing
    }


-- | A client token used to correlate requests and responses.
ccdAmznClientToken :: Lens' CreateCoreDefinition (Maybe Text)
ccdAmznClientToken = lens _ccdAmznClientToken (\ s a -> s{_ccdAmznClientToken = a})

-- | Information about the initial version of the core definition.
ccdInitialVersion :: Lens' CreateCoreDefinition (Maybe CoreDefinitionVersion)
ccdInitialVersion = lens _ccdInitialVersion (\ s a -> s{_ccdInitialVersion = a})

-- | The name of the core definition.
ccdName :: Lens' CreateCoreDefinition (Maybe Text)
ccdName = lens _ccdName (\ s a -> s{_ccdName = a})

instance AWSRequest CreateCoreDefinition where
        type Rs CreateCoreDefinition =
             CreateCoreDefinitionResponse
        request = postJSON greengrass
        response
          = receiveJSON
              (\ s h x ->
                 CreateCoreDefinitionResponse' <$>
                   (x .?> "LatestVersionArn") <*> (x .?> "Arn") <*>
                     (x .?> "Name")
                     <*> (x .?> "CreationTimestamp")
                     <*> (x .?> "Id")
                     <*> (x .?> "LatestVersion")
                     <*> (x .?> "LastUpdatedTimestamp")
                     <*> (pure (fromEnum s)))

instance Hashable CreateCoreDefinition where

instance NFData CreateCoreDefinition where

instance ToHeaders CreateCoreDefinition where
        toHeaders CreateCoreDefinition'{..}
          = mconcat
              ["X-Amzn-Client-Token" =# _ccdAmznClientToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToJSON CreateCoreDefinition where
        toJSON CreateCoreDefinition'{..}
          = object
              (catMaybes
                 [("InitialVersion" .=) <$> _ccdInitialVersion,
                  ("Name" .=) <$> _ccdName])

instance ToPath CreateCoreDefinition where
        toPath = const "/greengrass/definition/cores"

instance ToQuery CreateCoreDefinition where
        toQuery = const mempty

-- | /See:/ 'createCoreDefinitionResponse' smart constructor.
data CreateCoreDefinitionResponse = CreateCoreDefinitionResponse'
  { _ccdrsLatestVersionARN     :: !(Maybe Text)
  , _ccdrsARN                  :: !(Maybe Text)
  , _ccdrsName                 :: !(Maybe Text)
  , _ccdrsCreationTimestamp    :: !(Maybe Text)
  , _ccdrsId                   :: !(Maybe Text)
  , _ccdrsLatestVersion        :: !(Maybe Text)
  , _ccdrsLastUpdatedTimestamp :: !(Maybe Text)
  , _ccdrsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateCoreDefinitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccdrsLatestVersionARN' - The ARN of the latest version of the definition.
--
-- * 'ccdrsARN' - The ARN of the definition.
--
-- * 'ccdrsName' - The name of the definition.
--
-- * 'ccdrsCreationTimestamp' - The time, in milliseconds since the epoch, when the definition was created.
--
-- * 'ccdrsId' - The ID of the definition.
--
-- * 'ccdrsLatestVersion' - The latest version of the definition.
--
-- * 'ccdrsLastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last updated.
--
-- * 'ccdrsResponseStatus' - -- | The response status code.
createCoreDefinitionResponse
    :: Int -- ^ 'ccdrsResponseStatus'
    -> CreateCoreDefinitionResponse
createCoreDefinitionResponse pResponseStatus_ =
  CreateCoreDefinitionResponse'
    { _ccdrsLatestVersionARN = Nothing
    , _ccdrsARN = Nothing
    , _ccdrsName = Nothing
    , _ccdrsCreationTimestamp = Nothing
    , _ccdrsId = Nothing
    , _ccdrsLatestVersion = Nothing
    , _ccdrsLastUpdatedTimestamp = Nothing
    , _ccdrsResponseStatus = pResponseStatus_
    }


-- | The ARN of the latest version of the definition.
ccdrsLatestVersionARN :: Lens' CreateCoreDefinitionResponse (Maybe Text)
ccdrsLatestVersionARN = lens _ccdrsLatestVersionARN (\ s a -> s{_ccdrsLatestVersionARN = a})

-- | The ARN of the definition.
ccdrsARN :: Lens' CreateCoreDefinitionResponse (Maybe Text)
ccdrsARN = lens _ccdrsARN (\ s a -> s{_ccdrsARN = a})

-- | The name of the definition.
ccdrsName :: Lens' CreateCoreDefinitionResponse (Maybe Text)
ccdrsName = lens _ccdrsName (\ s a -> s{_ccdrsName = a})

-- | The time, in milliseconds since the epoch, when the definition was created.
ccdrsCreationTimestamp :: Lens' CreateCoreDefinitionResponse (Maybe Text)
ccdrsCreationTimestamp = lens _ccdrsCreationTimestamp (\ s a -> s{_ccdrsCreationTimestamp = a})

-- | The ID of the definition.
ccdrsId :: Lens' CreateCoreDefinitionResponse (Maybe Text)
ccdrsId = lens _ccdrsId (\ s a -> s{_ccdrsId = a})

-- | The latest version of the definition.
ccdrsLatestVersion :: Lens' CreateCoreDefinitionResponse (Maybe Text)
ccdrsLatestVersion = lens _ccdrsLatestVersion (\ s a -> s{_ccdrsLatestVersion = a})

-- | The time, in milliseconds since the epoch, when the definition was last updated.
ccdrsLastUpdatedTimestamp :: Lens' CreateCoreDefinitionResponse (Maybe Text)
ccdrsLastUpdatedTimestamp = lens _ccdrsLastUpdatedTimestamp (\ s a -> s{_ccdrsLastUpdatedTimestamp = a})

-- | -- | The response status code.
ccdrsResponseStatus :: Lens' CreateCoreDefinitionResponse Int
ccdrsResponseStatus = lens _ccdrsResponseStatus (\ s a -> s{_ccdrsResponseStatus = a})

instance NFData CreateCoreDefinitionResponse where
