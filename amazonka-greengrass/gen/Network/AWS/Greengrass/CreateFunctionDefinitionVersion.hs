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
-- Module      : Network.AWS.Greengrass.CreateFunctionDefinitionVersion
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a version of a Lambda function definition that has already been defined.
module Network.AWS.Greengrass.CreateFunctionDefinitionVersion
    (
    -- * Creating a Request
      createFunctionDefinitionVersion
    , CreateFunctionDefinitionVersion
    -- * Request Lenses
    , cfdvAmznClientToken
    , cfdvFunctions
    , cfdvFunctionDefinitionId

    -- * Destructuring the Response
    , createFunctionDefinitionVersionResponse
    , CreateFunctionDefinitionVersionResponse
    -- * Response Lenses
    , cfdvrsARN
    , cfdvrsCreationTimestamp
    , cfdvrsVersion
    , cfdvrsId
    , cfdvrsResponseStatus
    ) where

import           Network.AWS.Greengrass.Types
import           Network.AWS.Greengrass.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Function definition version
--
-- /See:/ 'createFunctionDefinitionVersion' smart constructor.
data CreateFunctionDefinitionVersion = CreateFunctionDefinitionVersion'
    { _cfdvAmznClientToken      :: !(Maybe Text)
    , _cfdvFunctions            :: !(Maybe [Function])
    , _cfdvFunctionDefinitionId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateFunctionDefinitionVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfdvAmznClientToken' - The client token used to request idempotent operations.
--
-- * 'cfdvFunctions' - Lambda functions in this function definition version.
--
-- * 'cfdvFunctionDefinitionId' - the unique Id of the lambda definition
createFunctionDefinitionVersion
    :: Text -- ^ 'cfdvFunctionDefinitionId'
    -> CreateFunctionDefinitionVersion
createFunctionDefinitionVersion pFunctionDefinitionId_ =
    CreateFunctionDefinitionVersion'
    { _cfdvAmznClientToken = Nothing
    , _cfdvFunctions = Nothing
    , _cfdvFunctionDefinitionId = pFunctionDefinitionId_
    }

-- | The client token used to request idempotent operations.
cfdvAmznClientToken :: Lens' CreateFunctionDefinitionVersion (Maybe Text)
cfdvAmznClientToken = lens _cfdvAmznClientToken (\ s a -> s{_cfdvAmznClientToken = a});

-- | Lambda functions in this function definition version.
cfdvFunctions :: Lens' CreateFunctionDefinitionVersion [Function]
cfdvFunctions = lens _cfdvFunctions (\ s a -> s{_cfdvFunctions = a}) . _Default . _Coerce;

-- | the unique Id of the lambda definition
cfdvFunctionDefinitionId :: Lens' CreateFunctionDefinitionVersion Text
cfdvFunctionDefinitionId = lens _cfdvFunctionDefinitionId (\ s a -> s{_cfdvFunctionDefinitionId = a});

instance AWSRequest CreateFunctionDefinitionVersion
         where
        type Rs CreateFunctionDefinitionVersion =
             CreateFunctionDefinitionVersionResponse
        request = postJSON greengrass
        response
          = receiveJSON
              (\ s h x ->
                 CreateFunctionDefinitionVersionResponse' <$>
                   (x .?> "Arn") <*> (x .?> "CreationTimestamp") <*>
                     (x .?> "Version")
                     <*> (x .?> "Id")
                     <*> (pure (fromEnum s)))

instance Hashable CreateFunctionDefinitionVersion

instance NFData CreateFunctionDefinitionVersion

instance ToHeaders CreateFunctionDefinitionVersion
         where
        toHeaders CreateFunctionDefinitionVersion'{..}
          = mconcat
              ["X-Amzn-Client-Token" =# _cfdvAmznClientToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToJSON CreateFunctionDefinitionVersion where
        toJSON CreateFunctionDefinitionVersion'{..}
          = object
              (catMaybes [("Functions" .=) <$> _cfdvFunctions])

instance ToPath CreateFunctionDefinitionVersion where
        toPath CreateFunctionDefinitionVersion'{..}
          = mconcat
              ["/greengrass/definition/functions/",
               toBS _cfdvFunctionDefinitionId, "/versions"]

instance ToQuery CreateFunctionDefinitionVersion
         where
        toQuery = const mempty

-- | /See:/ 'createFunctionDefinitionVersionResponse' smart constructor.
data CreateFunctionDefinitionVersionResponse = CreateFunctionDefinitionVersionResponse'
    { _cfdvrsARN               :: !(Maybe Text)
    , _cfdvrsCreationTimestamp :: !(Maybe Text)
    , _cfdvrsVersion           :: !(Maybe Text)
    , _cfdvrsId                :: !(Maybe Text)
    , _cfdvrsResponseStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateFunctionDefinitionVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfdvrsARN' - Arn of the version.
--
-- * 'cfdvrsCreationTimestamp' - Timestamp of when the version was created.
--
-- * 'cfdvrsVersion' - Unique Id of a version.
--
-- * 'cfdvrsId' - Id of the resource container.
--
-- * 'cfdvrsResponseStatus' - -- | The response status code.
createFunctionDefinitionVersionResponse
    :: Int -- ^ 'cfdvrsResponseStatus'
    -> CreateFunctionDefinitionVersionResponse
createFunctionDefinitionVersionResponse pResponseStatus_ =
    CreateFunctionDefinitionVersionResponse'
    { _cfdvrsARN = Nothing
    , _cfdvrsCreationTimestamp = Nothing
    , _cfdvrsVersion = Nothing
    , _cfdvrsId = Nothing
    , _cfdvrsResponseStatus = pResponseStatus_
    }

-- | Arn of the version.
cfdvrsARN :: Lens' CreateFunctionDefinitionVersionResponse (Maybe Text)
cfdvrsARN = lens _cfdvrsARN (\ s a -> s{_cfdvrsARN = a});

-- | Timestamp of when the version was created.
cfdvrsCreationTimestamp :: Lens' CreateFunctionDefinitionVersionResponse (Maybe Text)
cfdvrsCreationTimestamp = lens _cfdvrsCreationTimestamp (\ s a -> s{_cfdvrsCreationTimestamp = a});

-- | Unique Id of a version.
cfdvrsVersion :: Lens' CreateFunctionDefinitionVersionResponse (Maybe Text)
cfdvrsVersion = lens _cfdvrsVersion (\ s a -> s{_cfdvrsVersion = a});

-- | Id of the resource container.
cfdvrsId :: Lens' CreateFunctionDefinitionVersionResponse (Maybe Text)
cfdvrsId = lens _cfdvrsId (\ s a -> s{_cfdvrsId = a});

-- | -- | The response status code.
cfdvrsResponseStatus :: Lens' CreateFunctionDefinitionVersionResponse Int
cfdvrsResponseStatus = lens _cfdvrsResponseStatus (\ s a -> s{_cfdvrsResponseStatus = a});

instance NFData
         CreateFunctionDefinitionVersionResponse
