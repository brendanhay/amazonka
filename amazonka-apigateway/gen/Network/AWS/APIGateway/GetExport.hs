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
-- Module      : Network.AWS.APIGateway.GetExport
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports a deployed version of a 'RestApi' in a specified format.
--
--
module Network.AWS.APIGateway.GetExport
    (
    -- * Creating a Request
      getExport
    , GetExport
    -- * Request Lenses
    , geParameters
    , geAccepts
    , geRestAPIId
    , geStageName
    , geExportType

    -- * Destructuring the Response
    , getExportResponse
    , GetExportResponse
    -- * Response Lenses
    , gersBody
    , gersContentDisposition
    , gersContentType
    , gersResponseStatus
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request a new export of a 'RestApi' for a particular 'Stage' .
--
--
--
-- /See:/ 'getExport' smart constructor.
data GetExport = GetExport'
  { _geParameters :: !(Maybe (Map Text Text))
  , _geAccepts    :: !(Maybe Text)
  , _geRestAPIId  :: !Text
  , _geStageName  :: !Text
  , _geExportType :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetExport' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'geParameters' - A key-value map of query string parameters that specify properties of the export, depending on the requested @exportType@ . For @exportType@ @swagger@ , any combination of the following parameters are supported: @integrations@ will export the API with x-amazon-apigateway-integration extensions. @authorizers@ will export the API with x-amazon-apigateway-authorizer extensions. @postman@ will export the API with Postman extensions, allowing for import to the Postman tool
--
-- * 'geAccepts' - The content-type of the export, for example @application/json@ . Currently @application/json@ and @application/yaml@ are supported for @exportType@ of @swagger@ . This should be specified in the @Accept@ header for direct API requests.
--
-- * 'geRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'geStageName' - [Required] The name of the 'Stage' that will be exported.
--
-- * 'geExportType' - [Required] The type of export. Currently only 'swagger' is supported.
getExport
    :: Text -- ^ 'geRestAPIId'
    -> Text -- ^ 'geStageName'
    -> Text -- ^ 'geExportType'
    -> GetExport
getExport pRestAPIId_ pStageName_ pExportType_ =
  GetExport'
    { _geParameters = Nothing
    , _geAccepts = Nothing
    , _geRestAPIId = pRestAPIId_
    , _geStageName = pStageName_
    , _geExportType = pExportType_
    }


-- | A key-value map of query string parameters that specify properties of the export, depending on the requested @exportType@ . For @exportType@ @swagger@ , any combination of the following parameters are supported: @integrations@ will export the API with x-amazon-apigateway-integration extensions. @authorizers@ will export the API with x-amazon-apigateway-authorizer extensions. @postman@ will export the API with Postman extensions, allowing for import to the Postman tool
geParameters :: Lens' GetExport (HashMap Text Text)
geParameters = lens _geParameters (\ s a -> s{_geParameters = a}) . _Default . _Map

-- | The content-type of the export, for example @application/json@ . Currently @application/json@ and @application/yaml@ are supported for @exportType@ of @swagger@ . This should be specified in the @Accept@ header for direct API requests.
geAccepts :: Lens' GetExport (Maybe Text)
geAccepts = lens _geAccepts (\ s a -> s{_geAccepts = a})

-- | [Required] The string identifier of the associated 'RestApi' .
geRestAPIId :: Lens' GetExport Text
geRestAPIId = lens _geRestAPIId (\ s a -> s{_geRestAPIId = a})

-- | [Required] The name of the 'Stage' that will be exported.
geStageName :: Lens' GetExport Text
geStageName = lens _geStageName (\ s a -> s{_geStageName = a})

-- | [Required] The type of export. Currently only 'swagger' is supported.
geExportType :: Lens' GetExport Text
geExportType = lens _geExportType (\ s a -> s{_geExportType = a})

instance AWSRequest GetExport where
        type Rs GetExport = GetExportResponse
        request = get apiGateway
        response
          = receiveBytes
              (\ s h x ->
                 GetExportResponse' <$>
                   (pure (Just x)) <*> (h .#? "Content-Disposition") <*>
                     (h .#? "Content-Type")
                     <*> (pure (fromEnum s)))

instance Hashable GetExport where

instance NFData GetExport where

instance ToHeaders GetExport where
        toHeaders GetExport'{..}
          = mconcat
              ["Accept" =# _geAccepts,
               "Accept" =# ("application/json" :: ByteString)]

instance ToPath GetExport where
        toPath GetExport'{..}
          = mconcat
              ["/restapis/", toBS _geRestAPIId, "/stages/",
               toBS _geStageName, "/exports/", toBS _geExportType]

instance ToQuery GetExport where
        toQuery GetExport'{..}
          = mconcat
              ["parameters" =:
                 toQuery
                   (toQueryMap "entry" "key" "value" <$> _geParameters)]

-- | The binary blob response to 'GetExport' , which contains the generated SDK.
--
--
--
-- /See:/ 'getExportResponse' smart constructor.
data GetExportResponse = GetExportResponse'
  { _gersBody               :: !(Maybe ByteString)
  , _gersContentDisposition :: !(Maybe Text)
  , _gersContentType        :: !(Maybe Text)
  , _gersResponseStatus     :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetExportResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gersBody' - The binary blob response to 'GetExport' , which contains the export.
--
-- * 'gersContentDisposition' - The content-disposition header value in the HTTP response.
--
-- * 'gersContentType' - The content-type header value in the HTTP response. This will correspond to a valid 'accept' type in the request.
--
-- * 'gersResponseStatus' - -- | The response status code.
getExportResponse
    :: Int -- ^ 'gersResponseStatus'
    -> GetExportResponse
getExportResponse pResponseStatus_ =
  GetExportResponse'
    { _gersBody = Nothing
    , _gersContentDisposition = Nothing
    , _gersContentType = Nothing
    , _gersResponseStatus = pResponseStatus_
    }


-- | The binary blob response to 'GetExport' , which contains the export.
gersBody :: Lens' GetExportResponse (Maybe ByteString)
gersBody = lens _gersBody (\ s a -> s{_gersBody = a})

-- | The content-disposition header value in the HTTP response.
gersContentDisposition :: Lens' GetExportResponse (Maybe Text)
gersContentDisposition = lens _gersContentDisposition (\ s a -> s{_gersContentDisposition = a})

-- | The content-type header value in the HTTP response. This will correspond to a valid 'accept' type in the request.
gersContentType :: Lens' GetExportResponse (Maybe Text)
gersContentType = lens _gersContentType (\ s a -> s{_gersContentType = a})

-- | -- | The response status code.
gersResponseStatus :: Lens' GetExportResponse Int
gersResponseStatus = lens _gersResponseStatus (\ s a -> s{_gersResponseStatus = a})

instance NFData GetExportResponse where
