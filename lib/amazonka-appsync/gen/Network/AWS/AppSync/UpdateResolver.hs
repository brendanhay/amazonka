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
-- Module      : Network.AWS.AppSync.UpdateResolver
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a @Resolver@ object.
--
--
module Network.AWS.AppSync.UpdateResolver
    (
    -- * Creating a Request
      updateResolver
    , UpdateResolver
    -- * Request Lenses
    , urResponseMappingTemplate
    , urApiId
    , urTypeName
    , urFieldName
    , urDataSourceName
    , urRequestMappingTemplate

    -- * Destructuring the Response
    , updateResolverResponse
    , UpdateResolverResponse
    -- * Response Lenses
    , urrsResolver
    , urrsResponseStatus
    ) where

import Network.AWS.AppSync.Types
import Network.AWS.AppSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateResolver' smart constructor.
data UpdateResolver = UpdateResolver'
  { _urResponseMappingTemplate :: !(Maybe Text)
  , _urApiId                   :: !Text
  , _urTypeName                :: !Text
  , _urFieldName               :: !Text
  , _urDataSourceName          :: !Text
  , _urRequestMappingTemplate  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateResolver' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urResponseMappingTemplate' - The new response mapping template.
--
-- * 'urApiId' - The API ID.
--
-- * 'urTypeName' - The new type name.
--
-- * 'urFieldName' - The new field name.
--
-- * 'urDataSourceName' - The new data source name.
--
-- * 'urRequestMappingTemplate' - The new request mapping template.
updateResolver
    :: Text -- ^ 'urApiId'
    -> Text -- ^ 'urTypeName'
    -> Text -- ^ 'urFieldName'
    -> Text -- ^ 'urDataSourceName'
    -> Text -- ^ 'urRequestMappingTemplate'
    -> UpdateResolver
updateResolver pApiId_ pTypeName_ pFieldName_ pDataSourceName_ pRequestMappingTemplate_ =
  UpdateResolver'
    { _urResponseMappingTemplate = Nothing
    , _urApiId = pApiId_
    , _urTypeName = pTypeName_
    , _urFieldName = pFieldName_
    , _urDataSourceName = pDataSourceName_
    , _urRequestMappingTemplate = pRequestMappingTemplate_
    }


-- | The new response mapping template.
urResponseMappingTemplate :: Lens' UpdateResolver (Maybe Text)
urResponseMappingTemplate = lens _urResponseMappingTemplate (\ s a -> s{_urResponseMappingTemplate = a})

-- | The API ID.
urApiId :: Lens' UpdateResolver Text
urApiId = lens _urApiId (\ s a -> s{_urApiId = a})

-- | The new type name.
urTypeName :: Lens' UpdateResolver Text
urTypeName = lens _urTypeName (\ s a -> s{_urTypeName = a})

-- | The new field name.
urFieldName :: Lens' UpdateResolver Text
urFieldName = lens _urFieldName (\ s a -> s{_urFieldName = a})

-- | The new data source name.
urDataSourceName :: Lens' UpdateResolver Text
urDataSourceName = lens _urDataSourceName (\ s a -> s{_urDataSourceName = a})

-- | The new request mapping template.
urRequestMappingTemplate :: Lens' UpdateResolver Text
urRequestMappingTemplate = lens _urRequestMappingTemplate (\ s a -> s{_urRequestMappingTemplate = a})

instance AWSRequest UpdateResolver where
        type Rs UpdateResolver = UpdateResolverResponse
        request = postJSON appSync
        response
          = receiveJSON
              (\ s h x ->
                 UpdateResolverResponse' <$>
                   (x .?> "resolver") <*> (pure (fromEnum s)))

instance Hashable UpdateResolver where

instance NFData UpdateResolver where

instance ToHeaders UpdateResolver where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateResolver where
        toJSON UpdateResolver'{..}
          = object
              (catMaybes
                 [("responseMappingTemplate" .=) <$>
                    _urResponseMappingTemplate,
                  Just ("dataSourceName" .= _urDataSourceName),
                  Just
                    ("requestMappingTemplate" .=
                       _urRequestMappingTemplate)])

instance ToPath UpdateResolver where
        toPath UpdateResolver'{..}
          = mconcat
              ["/v1/apis/", toBS _urApiId, "/types/",
               toBS _urTypeName, "/resolvers/", toBS _urFieldName]

instance ToQuery UpdateResolver where
        toQuery = const mempty

-- | /See:/ 'updateResolverResponse' smart constructor.
data UpdateResolverResponse = UpdateResolverResponse'
  { _urrsResolver       :: !(Maybe Resolver)
  , _urrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateResolverResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urrsResolver' - The updated @Resolver@ object.
--
-- * 'urrsResponseStatus' - -- | The response status code.
updateResolverResponse
    :: Int -- ^ 'urrsResponseStatus'
    -> UpdateResolverResponse
updateResolverResponse pResponseStatus_ =
  UpdateResolverResponse'
    {_urrsResolver = Nothing, _urrsResponseStatus = pResponseStatus_}


-- | The updated @Resolver@ object.
urrsResolver :: Lens' UpdateResolverResponse (Maybe Resolver)
urrsResolver = lens _urrsResolver (\ s a -> s{_urrsResolver = a})

-- | -- | The response status code.
urrsResponseStatus :: Lens' UpdateResolverResponse Int
urrsResponseStatus = lens _urrsResponseStatus (\ s a -> s{_urrsResponseStatus = a})

instance NFData UpdateResolverResponse where
