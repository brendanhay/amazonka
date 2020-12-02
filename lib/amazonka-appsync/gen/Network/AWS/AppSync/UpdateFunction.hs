{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.UpdateFunction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a @Function@ object.
module Network.AWS.AppSync.UpdateFunction
  ( -- * Creating a Request
    updateFunction,
    UpdateFunction,

    -- * Request Lenses
    ufRequestMappingTemplate,
    ufResponseMappingTemplate,
    ufDescription,
    ufApiId,
    ufName,
    ufFunctionId,
    ufDataSourceName,
    ufFunctionVersion,

    -- * Destructuring the Response
    updateFunctionResponse,
    UpdateFunctionResponse,

    -- * Response Lenses
    ufrsFunctionConfiguration,
    ufrsResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateFunction' smart constructor.
data UpdateFunction = UpdateFunction'
  { _ufRequestMappingTemplate ::
      !(Maybe Text),
    _ufResponseMappingTemplate :: !(Maybe Text),
    _ufDescription :: !(Maybe Text),
    _ufApiId :: !Text,
    _ufName :: !Text,
    _ufFunctionId :: !Text,
    _ufDataSourceName :: !Text,
    _ufFunctionVersion :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateFunction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ufRequestMappingTemplate' - The @Function@ request mapping template. Functions support only the 2018-05-29 version of the request mapping template.
--
-- * 'ufResponseMappingTemplate' - The @Function@ request mapping template.
--
-- * 'ufDescription' - The @Function@ description.
--
-- * 'ufApiId' - The GraphQL API ID.
--
-- * 'ufName' - The @Function@ name.
--
-- * 'ufFunctionId' - The function ID.
--
-- * 'ufDataSourceName' - The @Function@ @DataSource@ name.
--
-- * 'ufFunctionVersion' - The @version@ of the request mapping template. Currently the supported value is 2018-05-29.
updateFunction ::
  -- | 'ufApiId'
  Text ->
  -- | 'ufName'
  Text ->
  -- | 'ufFunctionId'
  Text ->
  -- | 'ufDataSourceName'
  Text ->
  -- | 'ufFunctionVersion'
  Text ->
  UpdateFunction
updateFunction
  pApiId_
  pName_
  pFunctionId_
  pDataSourceName_
  pFunctionVersion_ =
    UpdateFunction'
      { _ufRequestMappingTemplate = Nothing,
        _ufResponseMappingTemplate = Nothing,
        _ufDescription = Nothing,
        _ufApiId = pApiId_,
        _ufName = pName_,
        _ufFunctionId = pFunctionId_,
        _ufDataSourceName = pDataSourceName_,
        _ufFunctionVersion = pFunctionVersion_
      }

-- | The @Function@ request mapping template. Functions support only the 2018-05-29 version of the request mapping template.
ufRequestMappingTemplate :: Lens' UpdateFunction (Maybe Text)
ufRequestMappingTemplate = lens _ufRequestMappingTemplate (\s a -> s {_ufRequestMappingTemplate = a})

-- | The @Function@ request mapping template.
ufResponseMappingTemplate :: Lens' UpdateFunction (Maybe Text)
ufResponseMappingTemplate = lens _ufResponseMappingTemplate (\s a -> s {_ufResponseMappingTemplate = a})

-- | The @Function@ description.
ufDescription :: Lens' UpdateFunction (Maybe Text)
ufDescription = lens _ufDescription (\s a -> s {_ufDescription = a})

-- | The GraphQL API ID.
ufApiId :: Lens' UpdateFunction Text
ufApiId = lens _ufApiId (\s a -> s {_ufApiId = a})

-- | The @Function@ name.
ufName :: Lens' UpdateFunction Text
ufName = lens _ufName (\s a -> s {_ufName = a})

-- | The function ID.
ufFunctionId :: Lens' UpdateFunction Text
ufFunctionId = lens _ufFunctionId (\s a -> s {_ufFunctionId = a})

-- | The @Function@ @DataSource@ name.
ufDataSourceName :: Lens' UpdateFunction Text
ufDataSourceName = lens _ufDataSourceName (\s a -> s {_ufDataSourceName = a})

-- | The @version@ of the request mapping template. Currently the supported value is 2018-05-29.
ufFunctionVersion :: Lens' UpdateFunction Text
ufFunctionVersion = lens _ufFunctionVersion (\s a -> s {_ufFunctionVersion = a})

instance AWSRequest UpdateFunction where
  type Rs UpdateFunction = UpdateFunctionResponse
  request = postJSON appSync
  response =
    receiveJSON
      ( \s h x ->
          UpdateFunctionResponse'
            <$> (x .?> "functionConfiguration") <*> (pure (fromEnum s))
      )

instance Hashable UpdateFunction

instance NFData UpdateFunction

instance ToHeaders UpdateFunction where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON UpdateFunction where
  toJSON UpdateFunction' {..} =
    object
      ( catMaybes
          [ ("requestMappingTemplate" .=) <$> _ufRequestMappingTemplate,
            ("responseMappingTemplate" .=) <$> _ufResponseMappingTemplate,
            ("description" .=) <$> _ufDescription,
            Just ("name" .= _ufName),
            Just ("dataSourceName" .= _ufDataSourceName),
            Just ("functionVersion" .= _ufFunctionVersion)
          ]
      )

instance ToPath UpdateFunction where
  toPath UpdateFunction' {..} =
    mconcat
      ["/v1/apis/", toBS _ufApiId, "/functions/", toBS _ufFunctionId]

instance ToQuery UpdateFunction where
  toQuery = const mempty

-- | /See:/ 'updateFunctionResponse' smart constructor.
data UpdateFunctionResponse = UpdateFunctionResponse'
  { _ufrsFunctionConfiguration ::
      !(Maybe FunctionConfiguration),
    _ufrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateFunctionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ufrsFunctionConfiguration' - The @Function@ object.
--
-- * 'ufrsResponseStatus' - -- | The response status code.
updateFunctionResponse ::
  -- | 'ufrsResponseStatus'
  Int ->
  UpdateFunctionResponse
updateFunctionResponse pResponseStatus_ =
  UpdateFunctionResponse'
    { _ufrsFunctionConfiguration = Nothing,
      _ufrsResponseStatus = pResponseStatus_
    }

-- | The @Function@ object.
ufrsFunctionConfiguration :: Lens' UpdateFunctionResponse (Maybe FunctionConfiguration)
ufrsFunctionConfiguration = lens _ufrsFunctionConfiguration (\s a -> s {_ufrsFunctionConfiguration = a})

-- | -- | The response status code.
ufrsResponseStatus :: Lens' UpdateFunctionResponse Int
ufrsResponseStatus = lens _ufrsResponseStatus (\s a -> s {_ufrsResponseStatus = a})

instance NFData UpdateFunctionResponse
