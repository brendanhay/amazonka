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
-- Module      : Network.AWS.Greengrass.GetFunctionDefinitionVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a Lambda function definition version, including which Lambda functions are included in the version and their configurations.
module Network.AWS.Greengrass.GetFunctionDefinitionVersion
    (
    -- * Creating a Request
      getFunctionDefinitionVersion
    , GetFunctionDefinitionVersion
    -- * Request Lenses
    , gfdvFunctionDefinitionId
    , gfdvFunctionDefinitionVersionId

    -- * Destructuring the Response
    , getFunctionDefinitionVersionResponse
    , GetFunctionDefinitionVersionResponse
    -- * Response Lenses
    , gfdvrsDefinition
    , gfdvrsARN
    , gfdvrsCreationTimestamp
    , gfdvrsVersion
    , gfdvrsId
    , gfdvrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getFunctionDefinitionVersion' smart constructor.
data GetFunctionDefinitionVersion = GetFunctionDefinitionVersion'
  { _gfdvFunctionDefinitionId        :: !Text
  , _gfdvFunctionDefinitionVersionId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetFunctionDefinitionVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gfdvFunctionDefinitionId' - The ID of the Lambda function definition.
--
-- * 'gfdvFunctionDefinitionVersionId' - The ID of the function definition version.
getFunctionDefinitionVersion
    :: Text -- ^ 'gfdvFunctionDefinitionId'
    -> Text -- ^ 'gfdvFunctionDefinitionVersionId'
    -> GetFunctionDefinitionVersion
getFunctionDefinitionVersion pFunctionDefinitionId_ pFunctionDefinitionVersionId_ =
  GetFunctionDefinitionVersion'
    { _gfdvFunctionDefinitionId = pFunctionDefinitionId_
    , _gfdvFunctionDefinitionVersionId = pFunctionDefinitionVersionId_
    }


-- | The ID of the Lambda function definition.
gfdvFunctionDefinitionId :: Lens' GetFunctionDefinitionVersion Text
gfdvFunctionDefinitionId = lens _gfdvFunctionDefinitionId (\ s a -> s{_gfdvFunctionDefinitionId = a})

-- | The ID of the function definition version.
gfdvFunctionDefinitionVersionId :: Lens' GetFunctionDefinitionVersion Text
gfdvFunctionDefinitionVersionId = lens _gfdvFunctionDefinitionVersionId (\ s a -> s{_gfdvFunctionDefinitionVersionId = a})

instance AWSRequest GetFunctionDefinitionVersion
         where
        type Rs GetFunctionDefinitionVersion =
             GetFunctionDefinitionVersionResponse
        request = get greengrass
        response
          = receiveJSON
              (\ s h x ->
                 GetFunctionDefinitionVersionResponse' <$>
                   (x .?> "Definition") <*> (x .?> "Arn") <*>
                     (x .?> "CreationTimestamp")
                     <*> (x .?> "Version")
                     <*> (x .?> "Id")
                     <*> (pure (fromEnum s)))

instance Hashable GetFunctionDefinitionVersion where

instance NFData GetFunctionDefinitionVersion where

instance ToHeaders GetFunctionDefinitionVersion where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetFunctionDefinitionVersion where
        toPath GetFunctionDefinitionVersion'{..}
          = mconcat
              ["/greengrass/definition/functions/",
               toBS _gfdvFunctionDefinitionId, "/versions/",
               toBS _gfdvFunctionDefinitionVersionId]

instance ToQuery GetFunctionDefinitionVersion where
        toQuery = const mempty

-- | /See:/ 'getFunctionDefinitionVersionResponse' smart constructor.
data GetFunctionDefinitionVersionResponse = GetFunctionDefinitionVersionResponse'
  { _gfdvrsDefinition        :: !(Maybe FunctionDefinitionVersion)
  , _gfdvrsARN               :: !(Maybe Text)
  , _gfdvrsCreationTimestamp :: !(Maybe Text)
  , _gfdvrsVersion           :: !(Maybe Text)
  , _gfdvrsId                :: !(Maybe Text)
  , _gfdvrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetFunctionDefinitionVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gfdvrsDefinition' - Information on the definition.
--
-- * 'gfdvrsARN' - The ARN of the function definition version.
--
-- * 'gfdvrsCreationTimestamp' - The time, in milliseconds since the epoch, when the function definition version was created.
--
-- * 'gfdvrsVersion' - The version of the function definition version.
--
-- * 'gfdvrsId' - The ID of the function definition version.
--
-- * 'gfdvrsResponseStatus' - -- | The response status code.
getFunctionDefinitionVersionResponse
    :: Int -- ^ 'gfdvrsResponseStatus'
    -> GetFunctionDefinitionVersionResponse
getFunctionDefinitionVersionResponse pResponseStatus_ =
  GetFunctionDefinitionVersionResponse'
    { _gfdvrsDefinition = Nothing
    , _gfdvrsARN = Nothing
    , _gfdvrsCreationTimestamp = Nothing
    , _gfdvrsVersion = Nothing
    , _gfdvrsId = Nothing
    , _gfdvrsResponseStatus = pResponseStatus_
    }


-- | Information on the definition.
gfdvrsDefinition :: Lens' GetFunctionDefinitionVersionResponse (Maybe FunctionDefinitionVersion)
gfdvrsDefinition = lens _gfdvrsDefinition (\ s a -> s{_gfdvrsDefinition = a})

-- | The ARN of the function definition version.
gfdvrsARN :: Lens' GetFunctionDefinitionVersionResponse (Maybe Text)
gfdvrsARN = lens _gfdvrsARN (\ s a -> s{_gfdvrsARN = a})

-- | The time, in milliseconds since the epoch, when the function definition version was created.
gfdvrsCreationTimestamp :: Lens' GetFunctionDefinitionVersionResponse (Maybe Text)
gfdvrsCreationTimestamp = lens _gfdvrsCreationTimestamp (\ s a -> s{_gfdvrsCreationTimestamp = a})

-- | The version of the function definition version.
gfdvrsVersion :: Lens' GetFunctionDefinitionVersionResponse (Maybe Text)
gfdvrsVersion = lens _gfdvrsVersion (\ s a -> s{_gfdvrsVersion = a})

-- | The ID of the function definition version.
gfdvrsId :: Lens' GetFunctionDefinitionVersionResponse (Maybe Text)
gfdvrsId = lens _gfdvrsId (\ s a -> s{_gfdvrsId = a})

-- | -- | The response status code.
gfdvrsResponseStatus :: Lens' GetFunctionDefinitionVersionResponse Int
gfdvrsResponseStatus = lens _gfdvrsResponseStatus (\ s a -> s{_gfdvrsResponseStatus = a})

instance NFData GetFunctionDefinitionVersionResponse
         where
