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
-- Module      : Network.AWS.Greengrass.CreateResourceDefinition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a resource definition which contains a list of resources to be used in a group. You can create an initial version of the definition by providing a list of resources now, or use ''CreateResourceDefinitionVersion'' later.
module Network.AWS.Greengrass.CreateResourceDefinition
    (
    -- * Creating a Request
      createResourceDefinition
    , CreateResourceDefinition
    -- * Request Lenses
    , crdAmznClientToken
    , crdInitialVersion
    , crdName

    -- * Destructuring the Response
    , createResourceDefinitionResponse
    , CreateResourceDefinitionResponse
    -- * Response Lenses
    , crdrsLatestVersionARN
    , crdrsARN
    , crdrsName
    , crdrsCreationTimestamp
    , crdrsId
    , crdrsLatestVersion
    , crdrsLastUpdatedTimestamp
    , crdrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createResourceDefinition' smart constructor.
data CreateResourceDefinition = CreateResourceDefinition'
  { _crdAmznClientToken :: !(Maybe Text)
  , _crdInitialVersion  :: !(Maybe ResourceDefinitionVersion)
  , _crdName            :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateResourceDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crdAmznClientToken' - A client token used to correlate requests and responses.
--
-- * 'crdInitialVersion' - Information about the initial version of the resource definition.
--
-- * 'crdName' - The name of the resource definition.
createResourceDefinition
    :: CreateResourceDefinition
createResourceDefinition =
  CreateResourceDefinition'
    { _crdAmznClientToken = Nothing
    , _crdInitialVersion = Nothing
    , _crdName = Nothing
    }


-- | A client token used to correlate requests and responses.
crdAmznClientToken :: Lens' CreateResourceDefinition (Maybe Text)
crdAmznClientToken = lens _crdAmznClientToken (\ s a -> s{_crdAmznClientToken = a})

-- | Information about the initial version of the resource definition.
crdInitialVersion :: Lens' CreateResourceDefinition (Maybe ResourceDefinitionVersion)
crdInitialVersion = lens _crdInitialVersion (\ s a -> s{_crdInitialVersion = a})

-- | The name of the resource definition.
crdName :: Lens' CreateResourceDefinition (Maybe Text)
crdName = lens _crdName (\ s a -> s{_crdName = a})

instance AWSRequest CreateResourceDefinition where
        type Rs CreateResourceDefinition =
             CreateResourceDefinitionResponse
        request = postJSON greengrass
        response
          = receiveJSON
              (\ s h x ->
                 CreateResourceDefinitionResponse' <$>
                   (x .?> "LatestVersionArn") <*> (x .?> "Arn") <*>
                     (x .?> "Name")
                     <*> (x .?> "CreationTimestamp")
                     <*> (x .?> "Id")
                     <*> (x .?> "LatestVersion")
                     <*> (x .?> "LastUpdatedTimestamp")
                     <*> (pure (fromEnum s)))

instance Hashable CreateResourceDefinition where

instance NFData CreateResourceDefinition where

instance ToHeaders CreateResourceDefinition where
        toHeaders CreateResourceDefinition'{..}
          = mconcat
              ["X-Amzn-Client-Token" =# _crdAmznClientToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToJSON CreateResourceDefinition where
        toJSON CreateResourceDefinition'{..}
          = object
              (catMaybes
                 [("InitialVersion" .=) <$> _crdInitialVersion,
                  ("Name" .=) <$> _crdName])

instance ToPath CreateResourceDefinition where
        toPath = const "/greengrass/definition/resources"

instance ToQuery CreateResourceDefinition where
        toQuery = const mempty

-- | /See:/ 'createResourceDefinitionResponse' smart constructor.
data CreateResourceDefinitionResponse = CreateResourceDefinitionResponse'
  { _crdrsLatestVersionARN     :: !(Maybe Text)
  , _crdrsARN                  :: !(Maybe Text)
  , _crdrsName                 :: !(Maybe Text)
  , _crdrsCreationTimestamp    :: !(Maybe Text)
  , _crdrsId                   :: !(Maybe Text)
  , _crdrsLatestVersion        :: !(Maybe Text)
  , _crdrsLastUpdatedTimestamp :: !(Maybe Text)
  , _crdrsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateResourceDefinitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crdrsLatestVersionARN' - The ARN of the latest version of the definition.
--
-- * 'crdrsARN' - The ARN of the definition.
--
-- * 'crdrsName' - The name of the definition.
--
-- * 'crdrsCreationTimestamp' - The time, in milliseconds since the epoch, when the definition was created.
--
-- * 'crdrsId' - The ID of the definition.
--
-- * 'crdrsLatestVersion' - The latest version of the definition.
--
-- * 'crdrsLastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last updated.
--
-- * 'crdrsResponseStatus' - -- | The response status code.
createResourceDefinitionResponse
    :: Int -- ^ 'crdrsResponseStatus'
    -> CreateResourceDefinitionResponse
createResourceDefinitionResponse pResponseStatus_ =
  CreateResourceDefinitionResponse'
    { _crdrsLatestVersionARN = Nothing
    , _crdrsARN = Nothing
    , _crdrsName = Nothing
    , _crdrsCreationTimestamp = Nothing
    , _crdrsId = Nothing
    , _crdrsLatestVersion = Nothing
    , _crdrsLastUpdatedTimestamp = Nothing
    , _crdrsResponseStatus = pResponseStatus_
    }


-- | The ARN of the latest version of the definition.
crdrsLatestVersionARN :: Lens' CreateResourceDefinitionResponse (Maybe Text)
crdrsLatestVersionARN = lens _crdrsLatestVersionARN (\ s a -> s{_crdrsLatestVersionARN = a})

-- | The ARN of the definition.
crdrsARN :: Lens' CreateResourceDefinitionResponse (Maybe Text)
crdrsARN = lens _crdrsARN (\ s a -> s{_crdrsARN = a})

-- | The name of the definition.
crdrsName :: Lens' CreateResourceDefinitionResponse (Maybe Text)
crdrsName = lens _crdrsName (\ s a -> s{_crdrsName = a})

-- | The time, in milliseconds since the epoch, when the definition was created.
crdrsCreationTimestamp :: Lens' CreateResourceDefinitionResponse (Maybe Text)
crdrsCreationTimestamp = lens _crdrsCreationTimestamp (\ s a -> s{_crdrsCreationTimestamp = a})

-- | The ID of the definition.
crdrsId :: Lens' CreateResourceDefinitionResponse (Maybe Text)
crdrsId = lens _crdrsId (\ s a -> s{_crdrsId = a})

-- | The latest version of the definition.
crdrsLatestVersion :: Lens' CreateResourceDefinitionResponse (Maybe Text)
crdrsLatestVersion = lens _crdrsLatestVersion (\ s a -> s{_crdrsLatestVersion = a})

-- | The time, in milliseconds since the epoch, when the definition was last updated.
crdrsLastUpdatedTimestamp :: Lens' CreateResourceDefinitionResponse (Maybe Text)
crdrsLastUpdatedTimestamp = lens _crdrsLastUpdatedTimestamp (\ s a -> s{_crdrsLastUpdatedTimestamp = a})

-- | -- | The response status code.
crdrsResponseStatus :: Lens' CreateResourceDefinitionResponse Int
crdrsResponseStatus = lens _crdrsResponseStatus (\ s a -> s{_crdrsResponseStatus = a})

instance NFData CreateResourceDefinitionResponse
         where
