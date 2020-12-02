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
-- Module      : Network.AWS.Greengrass.CreateResourceDefinitionVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a version of a resource definition that has already been defined.
module Network.AWS.Greengrass.CreateResourceDefinitionVersion
    (
    -- * Creating a Request
      createResourceDefinitionVersion
    , CreateResourceDefinitionVersion
    -- * Request Lenses
    , crdvAmznClientToken
    , crdvResources
    , crdvResourceDefinitionId

    -- * Destructuring the Response
    , createResourceDefinitionVersionResponse
    , CreateResourceDefinitionVersionResponse
    -- * Response Lenses
    , crdvrsARN
    , crdvrsCreationTimestamp
    , crdvrsVersion
    , crdvrsId
    , crdvrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createResourceDefinitionVersion' smart constructor.
data CreateResourceDefinitionVersion = CreateResourceDefinitionVersion'
  { _crdvAmznClientToken      :: !(Maybe Text)
  , _crdvResources            :: !(Maybe [Resource])
  , _crdvResourceDefinitionId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateResourceDefinitionVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crdvAmznClientToken' - A client token used to correlate requests and responses.
--
-- * 'crdvResources' - A list of resources.
--
-- * 'crdvResourceDefinitionId' - The ID of the resource definition.
createResourceDefinitionVersion
    :: Text -- ^ 'crdvResourceDefinitionId'
    -> CreateResourceDefinitionVersion
createResourceDefinitionVersion pResourceDefinitionId_ =
  CreateResourceDefinitionVersion'
    { _crdvAmznClientToken = Nothing
    , _crdvResources = Nothing
    , _crdvResourceDefinitionId = pResourceDefinitionId_
    }


-- | A client token used to correlate requests and responses.
crdvAmznClientToken :: Lens' CreateResourceDefinitionVersion (Maybe Text)
crdvAmznClientToken = lens _crdvAmznClientToken (\ s a -> s{_crdvAmznClientToken = a})

-- | A list of resources.
crdvResources :: Lens' CreateResourceDefinitionVersion [Resource]
crdvResources = lens _crdvResources (\ s a -> s{_crdvResources = a}) . _Default . _Coerce

-- | The ID of the resource definition.
crdvResourceDefinitionId :: Lens' CreateResourceDefinitionVersion Text
crdvResourceDefinitionId = lens _crdvResourceDefinitionId (\ s a -> s{_crdvResourceDefinitionId = a})

instance AWSRequest CreateResourceDefinitionVersion
         where
        type Rs CreateResourceDefinitionVersion =
             CreateResourceDefinitionVersionResponse
        request = postJSON greengrass
        response
          = receiveJSON
              (\ s h x ->
                 CreateResourceDefinitionVersionResponse' <$>
                   (x .?> "Arn") <*> (x .?> "CreationTimestamp") <*>
                     (x .?> "Version")
                     <*> (x .?> "Id")
                     <*> (pure (fromEnum s)))

instance Hashable CreateResourceDefinitionVersion
         where

instance NFData CreateResourceDefinitionVersion where

instance ToHeaders CreateResourceDefinitionVersion
         where
        toHeaders CreateResourceDefinitionVersion'{..}
          = mconcat
              ["X-Amzn-Client-Token" =# _crdvAmznClientToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToJSON CreateResourceDefinitionVersion where
        toJSON CreateResourceDefinitionVersion'{..}
          = object
              (catMaybes [("Resources" .=) <$> _crdvResources])

instance ToPath CreateResourceDefinitionVersion where
        toPath CreateResourceDefinitionVersion'{..}
          = mconcat
              ["/greengrass/definition/resources/",
               toBS _crdvResourceDefinitionId, "/versions"]

instance ToQuery CreateResourceDefinitionVersion
         where
        toQuery = const mempty

-- | /See:/ 'createResourceDefinitionVersionResponse' smart constructor.
data CreateResourceDefinitionVersionResponse = CreateResourceDefinitionVersionResponse'
  { _crdvrsARN               :: !(Maybe Text)
  , _crdvrsCreationTimestamp :: !(Maybe Text)
  , _crdvrsVersion           :: !(Maybe Text)
  , _crdvrsId                :: !(Maybe Text)
  , _crdvrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateResourceDefinitionVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crdvrsARN' - The ARN of the version.
--
-- * 'crdvrsCreationTimestamp' - The time, in milliseconds since the epoch, when the version was created.
--
-- * 'crdvrsVersion' - The unique ID of the version.
--
-- * 'crdvrsId' - The ID of the version.
--
-- * 'crdvrsResponseStatus' - -- | The response status code.
createResourceDefinitionVersionResponse
    :: Int -- ^ 'crdvrsResponseStatus'
    -> CreateResourceDefinitionVersionResponse
createResourceDefinitionVersionResponse pResponseStatus_ =
  CreateResourceDefinitionVersionResponse'
    { _crdvrsARN = Nothing
    , _crdvrsCreationTimestamp = Nothing
    , _crdvrsVersion = Nothing
    , _crdvrsId = Nothing
    , _crdvrsResponseStatus = pResponseStatus_
    }


-- | The ARN of the version.
crdvrsARN :: Lens' CreateResourceDefinitionVersionResponse (Maybe Text)
crdvrsARN = lens _crdvrsARN (\ s a -> s{_crdvrsARN = a})

-- | The time, in milliseconds since the epoch, when the version was created.
crdvrsCreationTimestamp :: Lens' CreateResourceDefinitionVersionResponse (Maybe Text)
crdvrsCreationTimestamp = lens _crdvrsCreationTimestamp (\ s a -> s{_crdvrsCreationTimestamp = a})

-- | The unique ID of the version.
crdvrsVersion :: Lens' CreateResourceDefinitionVersionResponse (Maybe Text)
crdvrsVersion = lens _crdvrsVersion (\ s a -> s{_crdvrsVersion = a})

-- | The ID of the version.
crdvrsId :: Lens' CreateResourceDefinitionVersionResponse (Maybe Text)
crdvrsId = lens _crdvrsId (\ s a -> s{_crdvrsId = a})

-- | -- | The response status code.
crdvrsResponseStatus :: Lens' CreateResourceDefinitionVersionResponse Int
crdvrsResponseStatus = lens _crdvrsResponseStatus (\ s a -> s{_crdvrsResponseStatus = a})

instance NFData
           CreateResourceDefinitionVersionResponse
         where
