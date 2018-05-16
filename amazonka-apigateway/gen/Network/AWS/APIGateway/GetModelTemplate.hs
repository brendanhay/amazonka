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
-- Module      : Network.AWS.APIGateway.GetModelTemplate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a sample mapping template that can be used to transform a payload into the structure of a model.
--
--
module Network.AWS.APIGateway.GetModelTemplate
    (
    -- * Creating a Request
      getModelTemplate
    , GetModelTemplate
    -- * Request Lenses
    , gmtRestAPIId
    , gmtModelName

    -- * Destructuring the Response
    , getModelTemplateResponse
    , GetModelTemplateResponse
    -- * Response Lenses
    , gmtrsValue
    , gmtrsResponseStatus
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request to generate a sample mapping template used to transform the payload.
--
--
--
-- /See:/ 'getModelTemplate' smart constructor.
data GetModelTemplate = GetModelTemplate'
  { _gmtRestAPIId :: !Text
  , _gmtModelName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetModelTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmtRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'gmtModelName' - [Required] The name of the model for which to generate a template.
getModelTemplate
    :: Text -- ^ 'gmtRestAPIId'
    -> Text -- ^ 'gmtModelName'
    -> GetModelTemplate
getModelTemplate pRestAPIId_ pModelName_ =
  GetModelTemplate' {_gmtRestAPIId = pRestAPIId_, _gmtModelName = pModelName_}


-- | [Required] The string identifier of the associated 'RestApi' .
gmtRestAPIId :: Lens' GetModelTemplate Text
gmtRestAPIId = lens _gmtRestAPIId (\ s a -> s{_gmtRestAPIId = a})

-- | [Required] The name of the model for which to generate a template.
gmtModelName :: Lens' GetModelTemplate Text
gmtModelName = lens _gmtModelName (\ s a -> s{_gmtModelName = a})

instance AWSRequest GetModelTemplate where
        type Rs GetModelTemplate = GetModelTemplateResponse
        request = get apiGateway
        response
          = receiveJSON
              (\ s h x ->
                 GetModelTemplateResponse' <$>
                   (x .?> "value") <*> (pure (fromEnum s)))

instance Hashable GetModelTemplate where

instance NFData GetModelTemplate where

instance ToHeaders GetModelTemplate where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath GetModelTemplate where
        toPath GetModelTemplate'{..}
          = mconcat
              ["/restapis/", toBS _gmtRestAPIId, "/models/",
               toBS _gmtModelName, "/default_template"]

instance ToQuery GetModelTemplate where
        toQuery = const mempty

-- | Represents a mapping template used to transform a payload.
--
--
-- <http://docs.aws.amazon.com/apigateway/latest/developerguide/models-mappings.html#models-mappings-mappings Mapping Templates>
--
-- /See:/ 'getModelTemplateResponse' smart constructor.
data GetModelTemplateResponse = GetModelTemplateResponse'
  { _gmtrsValue          :: !(Maybe Text)
  , _gmtrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetModelTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmtrsValue' - The Apache <http://velocity.apache.org/engine/devel/vtl-reference-guide.html Velocity Template Language (VTL)> template content used for the template resource.
--
-- * 'gmtrsResponseStatus' - -- | The response status code.
getModelTemplateResponse
    :: Int -- ^ 'gmtrsResponseStatus'
    -> GetModelTemplateResponse
getModelTemplateResponse pResponseStatus_ =
  GetModelTemplateResponse'
    {_gmtrsValue = Nothing, _gmtrsResponseStatus = pResponseStatus_}


-- | The Apache <http://velocity.apache.org/engine/devel/vtl-reference-guide.html Velocity Template Language (VTL)> template content used for the template resource.
gmtrsValue :: Lens' GetModelTemplateResponse (Maybe Text)
gmtrsValue = lens _gmtrsValue (\ s a -> s{_gmtrsValue = a})

-- | -- | The response status code.
gmtrsResponseStatus :: Lens' GetModelTemplateResponse Int
gmtrsResponseStatus = lens _gmtrsResponseStatus (\ s a -> s{_gmtrsResponseStatus = a})

instance NFData GetModelTemplateResponse where
