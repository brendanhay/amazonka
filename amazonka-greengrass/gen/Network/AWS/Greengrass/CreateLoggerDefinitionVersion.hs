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
-- Module      : Network.AWS.Greengrass.CreateLoggerDefinitionVersion
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a version of a logger definition that has already been defined.
module Network.AWS.Greengrass.CreateLoggerDefinitionVersion
    (
    -- * Creating a Request
      createLoggerDefinitionVersion
    , CreateLoggerDefinitionVersion
    -- * Request Lenses
    , cldvLoggers
    , cldvAmznClientToken
    , cldvLoggerDefinitionId

    -- * Destructuring the Response
    , createLoggerDefinitionVersionResponse
    , CreateLoggerDefinitionVersionResponse
    -- * Response Lenses
    , cldvrsARN
    , cldvrsCreationTimestamp
    , cldvrsVersion
    , cldvrsId
    , cldvrsResponseStatus
    ) where

import           Network.AWS.Greengrass.Types
import           Network.AWS.Greengrass.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createLoggerDefinitionVersion' smart constructor.
data CreateLoggerDefinitionVersion = CreateLoggerDefinitionVersion'
    { _cldvLoggers            :: !(Maybe [GreengrassLogger])
    , _cldvAmznClientToken    :: !(Maybe Text)
    , _cldvLoggerDefinitionId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateLoggerDefinitionVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cldvLoggers' - List of loggers.
--
-- * 'cldvAmznClientToken' - The client token used to request idempotent operations.
--
-- * 'cldvLoggerDefinitionId' - logger definition Id
createLoggerDefinitionVersion
    :: Text -- ^ 'cldvLoggerDefinitionId'
    -> CreateLoggerDefinitionVersion
createLoggerDefinitionVersion pLoggerDefinitionId_ =
    CreateLoggerDefinitionVersion'
    { _cldvLoggers = Nothing
    , _cldvAmznClientToken = Nothing
    , _cldvLoggerDefinitionId = pLoggerDefinitionId_
    }

-- | List of loggers.
cldvLoggers :: Lens' CreateLoggerDefinitionVersion [GreengrassLogger]
cldvLoggers = lens _cldvLoggers (\ s a -> s{_cldvLoggers = a}) . _Default . _Coerce;

-- | The client token used to request idempotent operations.
cldvAmznClientToken :: Lens' CreateLoggerDefinitionVersion (Maybe Text)
cldvAmznClientToken = lens _cldvAmznClientToken (\ s a -> s{_cldvAmznClientToken = a});

-- | logger definition Id
cldvLoggerDefinitionId :: Lens' CreateLoggerDefinitionVersion Text
cldvLoggerDefinitionId = lens _cldvLoggerDefinitionId (\ s a -> s{_cldvLoggerDefinitionId = a});

instance AWSRequest CreateLoggerDefinitionVersion
         where
        type Rs CreateLoggerDefinitionVersion =
             CreateLoggerDefinitionVersionResponse
        request = postJSON greengrass
        response
          = receiveJSON
              (\ s h x ->
                 CreateLoggerDefinitionVersionResponse' <$>
                   (x .?> "Arn") <*> (x .?> "CreationTimestamp") <*>
                     (x .?> "Version")
                     <*> (x .?> "Id")
                     <*> (pure (fromEnum s)))

instance Hashable CreateLoggerDefinitionVersion

instance NFData CreateLoggerDefinitionVersion

instance ToHeaders CreateLoggerDefinitionVersion
         where
        toHeaders CreateLoggerDefinitionVersion'{..}
          = mconcat
              ["X-Amzn-Client-Token" =# _cldvAmznClientToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToJSON CreateLoggerDefinitionVersion where
        toJSON CreateLoggerDefinitionVersion'{..}
          = object
              (catMaybes [("Loggers" .=) <$> _cldvLoggers])

instance ToPath CreateLoggerDefinitionVersion where
        toPath CreateLoggerDefinitionVersion'{..}
          = mconcat
              ["/greengrass/definition/loggers/",
               toBS _cldvLoggerDefinitionId, "/versions"]

instance ToQuery CreateLoggerDefinitionVersion where
        toQuery = const mempty

-- | /See:/ 'createLoggerDefinitionVersionResponse' smart constructor.
data CreateLoggerDefinitionVersionResponse = CreateLoggerDefinitionVersionResponse'
    { _cldvrsARN               :: !(Maybe Text)
    , _cldvrsCreationTimestamp :: !(Maybe Text)
    , _cldvrsVersion           :: !(Maybe Text)
    , _cldvrsId                :: !(Maybe Text)
    , _cldvrsResponseStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateLoggerDefinitionVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cldvrsARN' - Arn of the version.
--
-- * 'cldvrsCreationTimestamp' - Timestamp of when the version was created.
--
-- * 'cldvrsVersion' - Unique Id of a version.
--
-- * 'cldvrsId' - Id of the resource container.
--
-- * 'cldvrsResponseStatus' - -- | The response status code.
createLoggerDefinitionVersionResponse
    :: Int -- ^ 'cldvrsResponseStatus'
    -> CreateLoggerDefinitionVersionResponse
createLoggerDefinitionVersionResponse pResponseStatus_ =
    CreateLoggerDefinitionVersionResponse'
    { _cldvrsARN = Nothing
    , _cldvrsCreationTimestamp = Nothing
    , _cldvrsVersion = Nothing
    , _cldvrsId = Nothing
    , _cldvrsResponseStatus = pResponseStatus_
    }

-- | Arn of the version.
cldvrsARN :: Lens' CreateLoggerDefinitionVersionResponse (Maybe Text)
cldvrsARN = lens _cldvrsARN (\ s a -> s{_cldvrsARN = a});

-- | Timestamp of when the version was created.
cldvrsCreationTimestamp :: Lens' CreateLoggerDefinitionVersionResponse (Maybe Text)
cldvrsCreationTimestamp = lens _cldvrsCreationTimestamp (\ s a -> s{_cldvrsCreationTimestamp = a});

-- | Unique Id of a version.
cldvrsVersion :: Lens' CreateLoggerDefinitionVersionResponse (Maybe Text)
cldvrsVersion = lens _cldvrsVersion (\ s a -> s{_cldvrsVersion = a});

-- | Id of the resource container.
cldvrsId :: Lens' CreateLoggerDefinitionVersionResponse (Maybe Text)
cldvrsId = lens _cldvrsId (\ s a -> s{_cldvrsId = a});

-- | -- | The response status code.
cldvrsResponseStatus :: Lens' CreateLoggerDefinitionVersionResponse Int
cldvrsResponseStatus = lens _cldvrsResponseStatus (\ s a -> s{_cldvrsResponseStatus = a});

instance NFData CreateLoggerDefinitionVersionResponse
