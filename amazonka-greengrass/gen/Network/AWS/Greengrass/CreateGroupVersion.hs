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
-- Module      : Network.AWS.Greengrass.CreateGroupVersion
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a version of a group which has already been defined.
module Network.AWS.Greengrass.CreateGroupVersion
    (
    -- * Creating a Request
      createGroupVersion
    , CreateGroupVersion
    -- * Request Lenses
    , cgvAmznClientToken
    , cgvSubscriptionDefinitionVersionARN
    , cgvCoreDefinitionVersionARN
    , cgvDeviceDefinitionVersionARN
    , cgvFunctionDefinitionVersionARN
    , cgvLoggerDefinitionVersionARN
    , cgvGroupId

    -- * Destructuring the Response
    , createGroupVersionResponse
    , CreateGroupVersionResponse
    -- * Response Lenses
    , cgvrsARN
    , cgvrsCreationTimestamp
    , cgvrsVersion
    , cgvrsId
    , cgvrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createGroupVersion' smart constructor.
data CreateGroupVersion = CreateGroupVersion'
  { _cgvAmznClientToken                  :: !(Maybe Text)
  , _cgvSubscriptionDefinitionVersionARN :: !(Maybe Text)
  , _cgvCoreDefinitionVersionARN         :: !(Maybe Text)
  , _cgvDeviceDefinitionVersionARN       :: !(Maybe Text)
  , _cgvFunctionDefinitionVersionARN     :: !(Maybe Text)
  , _cgvLoggerDefinitionVersionARN       :: !(Maybe Text)
  , _cgvGroupId                          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateGroupVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgvAmznClientToken' - The client token used to request idempotent operations.
--
-- * 'cgvSubscriptionDefinitionVersionARN' - Subscription definition version arn for this group.
--
-- * 'cgvCoreDefinitionVersionARN' - Core definition version arn for this group.
--
-- * 'cgvDeviceDefinitionVersionARN' - Device definition version arn for this group.
--
-- * 'cgvFunctionDefinitionVersionARN' - Function definition version arn for this group.
--
-- * 'cgvLoggerDefinitionVersionARN' - Logger definitionv ersion arn for this group.
--
-- * 'cgvGroupId' - The unique Id of the AWS Greengrass Group
createGroupVersion
    :: Text -- ^ 'cgvGroupId'
    -> CreateGroupVersion
createGroupVersion pGroupId_ =
  CreateGroupVersion'
  { _cgvAmznClientToken = Nothing
  , _cgvSubscriptionDefinitionVersionARN = Nothing
  , _cgvCoreDefinitionVersionARN = Nothing
  , _cgvDeviceDefinitionVersionARN = Nothing
  , _cgvFunctionDefinitionVersionARN = Nothing
  , _cgvLoggerDefinitionVersionARN = Nothing
  , _cgvGroupId = pGroupId_
  }


-- | The client token used to request idempotent operations.
cgvAmznClientToken :: Lens' CreateGroupVersion (Maybe Text)
cgvAmznClientToken = lens _cgvAmznClientToken (\ s a -> s{_cgvAmznClientToken = a});

-- | Subscription definition version arn for this group.
cgvSubscriptionDefinitionVersionARN :: Lens' CreateGroupVersion (Maybe Text)
cgvSubscriptionDefinitionVersionARN = lens _cgvSubscriptionDefinitionVersionARN (\ s a -> s{_cgvSubscriptionDefinitionVersionARN = a});

-- | Core definition version arn for this group.
cgvCoreDefinitionVersionARN :: Lens' CreateGroupVersion (Maybe Text)
cgvCoreDefinitionVersionARN = lens _cgvCoreDefinitionVersionARN (\ s a -> s{_cgvCoreDefinitionVersionARN = a});

-- | Device definition version arn for this group.
cgvDeviceDefinitionVersionARN :: Lens' CreateGroupVersion (Maybe Text)
cgvDeviceDefinitionVersionARN = lens _cgvDeviceDefinitionVersionARN (\ s a -> s{_cgvDeviceDefinitionVersionARN = a});

-- | Function definition version arn for this group.
cgvFunctionDefinitionVersionARN :: Lens' CreateGroupVersion (Maybe Text)
cgvFunctionDefinitionVersionARN = lens _cgvFunctionDefinitionVersionARN (\ s a -> s{_cgvFunctionDefinitionVersionARN = a});

-- | Logger definitionv ersion arn for this group.
cgvLoggerDefinitionVersionARN :: Lens' CreateGroupVersion (Maybe Text)
cgvLoggerDefinitionVersionARN = lens _cgvLoggerDefinitionVersionARN (\ s a -> s{_cgvLoggerDefinitionVersionARN = a});

-- | The unique Id of the AWS Greengrass Group
cgvGroupId :: Lens' CreateGroupVersion Text
cgvGroupId = lens _cgvGroupId (\ s a -> s{_cgvGroupId = a});

instance AWSRequest CreateGroupVersion where
        type Rs CreateGroupVersion =
             CreateGroupVersionResponse
        request = postJSON greengrass
        response
          = receiveJSON
              (\ s h x ->
                 CreateGroupVersionResponse' <$>
                   (x .?> "Arn") <*> (x .?> "CreationTimestamp") <*>
                     (x .?> "Version")
                     <*> (x .?> "Id")
                     <*> (pure (fromEnum s)))

instance Hashable CreateGroupVersion where

instance NFData CreateGroupVersion where

instance ToHeaders CreateGroupVersion where
        toHeaders CreateGroupVersion'{..}
          = mconcat
              ["X-Amzn-Client-Token" =# _cgvAmznClientToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToJSON CreateGroupVersion where
        toJSON CreateGroupVersion'{..}
          = object
              (catMaybes
                 [("SubscriptionDefinitionVersionArn" .=) <$>
                    _cgvSubscriptionDefinitionVersionARN,
                  ("CoreDefinitionVersionArn" .=) <$>
                    _cgvCoreDefinitionVersionARN,
                  ("DeviceDefinitionVersionArn" .=) <$>
                    _cgvDeviceDefinitionVersionARN,
                  ("FunctionDefinitionVersionArn" .=) <$>
                    _cgvFunctionDefinitionVersionARN,
                  ("LoggerDefinitionVersionArn" .=) <$>
                    _cgvLoggerDefinitionVersionARN])

instance ToPath CreateGroupVersion where
        toPath CreateGroupVersion'{..}
          = mconcat
              ["/greengrass/groups/", toBS _cgvGroupId,
               "/versions"]

instance ToQuery CreateGroupVersion where
        toQuery = const mempty

-- | /See:/ 'createGroupVersionResponse' smart constructor.
data CreateGroupVersionResponse = CreateGroupVersionResponse'
  { _cgvrsARN               :: !(Maybe Text)
  , _cgvrsCreationTimestamp :: !(Maybe Text)
  , _cgvrsVersion           :: !(Maybe Text)
  , _cgvrsId                :: !(Maybe Text)
  , _cgvrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateGroupVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgvrsARN' - Arn of the version.
--
-- * 'cgvrsCreationTimestamp' - Timestamp of when the version was created.
--
-- * 'cgvrsVersion' - Unique Id of a version.
--
-- * 'cgvrsId' - Id of the resource container.
--
-- * 'cgvrsResponseStatus' - -- | The response status code.
createGroupVersionResponse
    :: Int -- ^ 'cgvrsResponseStatus'
    -> CreateGroupVersionResponse
createGroupVersionResponse pResponseStatus_ =
  CreateGroupVersionResponse'
  { _cgvrsARN = Nothing
  , _cgvrsCreationTimestamp = Nothing
  , _cgvrsVersion = Nothing
  , _cgvrsId = Nothing
  , _cgvrsResponseStatus = pResponseStatus_
  }


-- | Arn of the version.
cgvrsARN :: Lens' CreateGroupVersionResponse (Maybe Text)
cgvrsARN = lens _cgvrsARN (\ s a -> s{_cgvrsARN = a});

-- | Timestamp of when the version was created.
cgvrsCreationTimestamp :: Lens' CreateGroupVersionResponse (Maybe Text)
cgvrsCreationTimestamp = lens _cgvrsCreationTimestamp (\ s a -> s{_cgvrsCreationTimestamp = a});

-- | Unique Id of a version.
cgvrsVersion :: Lens' CreateGroupVersionResponse (Maybe Text)
cgvrsVersion = lens _cgvrsVersion (\ s a -> s{_cgvrsVersion = a});

-- | Id of the resource container.
cgvrsId :: Lens' CreateGroupVersionResponse (Maybe Text)
cgvrsId = lens _cgvrsId (\ s a -> s{_cgvrsId = a});

-- | -- | The response status code.
cgvrsResponseStatus :: Lens' CreateGroupVersionResponse Int
cgvrsResponseStatus = lens _cgvrsResponseStatus (\ s a -> s{_cgvrsResponseStatus = a});

instance NFData CreateGroupVersionResponse where
