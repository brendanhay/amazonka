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
-- Module      : Network.AWS.AppSync.CreateFunction
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @Function@ object.
--
--
-- A function is a reusable entity. Multiple functions can be used to compose the resolver logic.
--
module Network.AWS.AppSync.CreateFunction
    (
    -- * Creating a Request
      createFunction
    , CreateFunction
    -- * Request Lenses
    , cfResponseMappingTemplate
    , cfDescription
    , cfApiId
    , cfName
    , cfDataSourceName
    , cfRequestMappingTemplate
    , cfFunctionVersion

    -- * Destructuring the Response
    , createFunctionResponse
    , CreateFunctionResponse
    -- * Response Lenses
    , cfrsFunctionConfiguration
    , cfrsResponseStatus
    ) where

import Network.AWS.AppSync.Types
import Network.AWS.AppSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createFunction' smart constructor.
data CreateFunction = CreateFunction'
  { _cfResponseMappingTemplate :: !(Maybe Text)
  , _cfDescription             :: !(Maybe Text)
  , _cfApiId                   :: !Text
  , _cfName                    :: !Text
  , _cfDataSourceName          :: !Text
  , _cfRequestMappingTemplate  :: !Text
  , _cfFunctionVersion         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateFunction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfResponseMappingTemplate' - The @Function@ response mapping template.
--
-- * 'cfDescription' - The @Function@ description.
--
-- * 'cfApiId' - The GraphQL API ID.
--
-- * 'cfName' - The @Function@ name. The function name does not have to be unique.
--
-- * 'cfDataSourceName' - The @Function@ @DataSource@ name.
--
-- * 'cfRequestMappingTemplate' - The @Function@ request mapping template. Functions support only the 2018-05-29 version of the request mapping template.
--
-- * 'cfFunctionVersion' - The @version@ of the request mapping template. Currently the supported value is 2018-05-29.
createFunction
    :: Text -- ^ 'cfApiId'
    -> Text -- ^ 'cfName'
    -> Text -- ^ 'cfDataSourceName'
    -> Text -- ^ 'cfRequestMappingTemplate'
    -> Text -- ^ 'cfFunctionVersion'
    -> CreateFunction
createFunction pApiId_ pName_ pDataSourceName_ pRequestMappingTemplate_ pFunctionVersion_ =
  CreateFunction'
    { _cfResponseMappingTemplate = Nothing
    , _cfDescription = Nothing
    , _cfApiId = pApiId_
    , _cfName = pName_
    , _cfDataSourceName = pDataSourceName_
    , _cfRequestMappingTemplate = pRequestMappingTemplate_
    , _cfFunctionVersion = pFunctionVersion_
    }


-- | The @Function@ response mapping template.
cfResponseMappingTemplate :: Lens' CreateFunction (Maybe Text)
cfResponseMappingTemplate = lens _cfResponseMappingTemplate (\ s a -> s{_cfResponseMappingTemplate = a})

-- | The @Function@ description.
cfDescription :: Lens' CreateFunction (Maybe Text)
cfDescription = lens _cfDescription (\ s a -> s{_cfDescription = a})

-- | The GraphQL API ID.
cfApiId :: Lens' CreateFunction Text
cfApiId = lens _cfApiId (\ s a -> s{_cfApiId = a})

-- | The @Function@ name. The function name does not have to be unique.
cfName :: Lens' CreateFunction Text
cfName = lens _cfName (\ s a -> s{_cfName = a})

-- | The @Function@ @DataSource@ name.
cfDataSourceName :: Lens' CreateFunction Text
cfDataSourceName = lens _cfDataSourceName (\ s a -> s{_cfDataSourceName = a})

-- | The @Function@ request mapping template. Functions support only the 2018-05-29 version of the request mapping template.
cfRequestMappingTemplate :: Lens' CreateFunction Text
cfRequestMappingTemplate = lens _cfRequestMappingTemplate (\ s a -> s{_cfRequestMappingTemplate = a})

-- | The @version@ of the request mapping template. Currently the supported value is 2018-05-29.
cfFunctionVersion :: Lens' CreateFunction Text
cfFunctionVersion = lens _cfFunctionVersion (\ s a -> s{_cfFunctionVersion = a})

instance AWSRequest CreateFunction where
        type Rs CreateFunction = CreateFunctionResponse
        request = postJSON appSync
        response
          = receiveJSON
              (\ s h x ->
                 CreateFunctionResponse' <$>
                   (x .?> "functionConfiguration") <*>
                     (pure (fromEnum s)))

instance Hashable CreateFunction where

instance NFData CreateFunction where

instance ToHeaders CreateFunction where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateFunction where
        toJSON CreateFunction'{..}
          = object
              (catMaybes
                 [("responseMappingTemplate" .=) <$>
                    _cfResponseMappingTemplate,
                  ("description" .=) <$> _cfDescription,
                  Just ("name" .= _cfName),
                  Just ("dataSourceName" .= _cfDataSourceName),
                  Just
                    ("requestMappingTemplate" .=
                       _cfRequestMappingTemplate),
                  Just ("functionVersion" .= _cfFunctionVersion)])

instance ToPath CreateFunction where
        toPath CreateFunction'{..}
          = mconcat ["/v1/apis/", toBS _cfApiId, "/functions"]

instance ToQuery CreateFunction where
        toQuery = const mempty

-- | /See:/ 'createFunctionResponse' smart constructor.
data CreateFunctionResponse = CreateFunctionResponse'
  { _cfrsFunctionConfiguration :: !(Maybe FunctionConfiguration)
  , _cfrsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateFunctionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfrsFunctionConfiguration' - The @Function@ object.
--
-- * 'cfrsResponseStatus' - -- | The response status code.
createFunctionResponse
    :: Int -- ^ 'cfrsResponseStatus'
    -> CreateFunctionResponse
createFunctionResponse pResponseStatus_ =
  CreateFunctionResponse'
    { _cfrsFunctionConfiguration = Nothing
    , _cfrsResponseStatus = pResponseStatus_
    }


-- | The @Function@ object.
cfrsFunctionConfiguration :: Lens' CreateFunctionResponse (Maybe FunctionConfiguration)
cfrsFunctionConfiguration = lens _cfrsFunctionConfiguration (\ s a -> s{_cfrsFunctionConfiguration = a})

-- | -- | The response status code.
cfrsResponseStatus :: Lens' CreateFunctionResponse Int
cfrsResponseStatus = lens _cfrsResponseStatus (\ s a -> s{_cfrsResponseStatus = a})

instance NFData CreateFunctionResponse where
