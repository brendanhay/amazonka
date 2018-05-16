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
-- Module      : Network.AWS.CloudDirectory.GetSchemaAsJSON
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a JSON representation of the schema. See <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_schemas.html#jsonformat JSON Schema Format> for more information.
--
--
module Network.AWS.CloudDirectory.GetSchemaAsJSON
    (
    -- * Creating a Request
      getSchemaAsJSON
    , GetSchemaAsJSON
    -- * Request Lenses
    , gsajSchemaARN

    -- * Destructuring the Response
    , getSchemaAsJSONResponse
    , GetSchemaAsJSONResponse
    -- * Response Lenses
    , gsajrsDocument
    , gsajrsName
    , gsajrsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getSchemaAsJSON' smart constructor.
newtype GetSchemaAsJSON = GetSchemaAsJSON'
  { _gsajSchemaARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSchemaAsJSON' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsajSchemaARN' - The ARN of the schema to retrieve.
getSchemaAsJSON
    :: Text -- ^ 'gsajSchemaARN'
    -> GetSchemaAsJSON
getSchemaAsJSON pSchemaARN_ = GetSchemaAsJSON' {_gsajSchemaARN = pSchemaARN_}


-- | The ARN of the schema to retrieve.
gsajSchemaARN :: Lens' GetSchemaAsJSON Text
gsajSchemaARN = lens _gsajSchemaARN (\ s a -> s{_gsajSchemaARN = a})

instance AWSRequest GetSchemaAsJSON where
        type Rs GetSchemaAsJSON = GetSchemaAsJSONResponse
        request = postJSON cloudDirectory
        response
          = receiveJSON
              (\ s h x ->
                 GetSchemaAsJSONResponse' <$>
                   (x .?> "Document") <*> (x .?> "Name") <*>
                     (pure (fromEnum s)))

instance Hashable GetSchemaAsJSON where

instance NFData GetSchemaAsJSON where

instance ToHeaders GetSchemaAsJSON where
        toHeaders GetSchemaAsJSON'{..}
          = mconcat ["x-amz-data-partition" =# _gsajSchemaARN]

instance ToJSON GetSchemaAsJSON where
        toJSON = const (Object mempty)

instance ToPath GetSchemaAsJSON where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/schema/json"

instance ToQuery GetSchemaAsJSON where
        toQuery = const mempty

-- | /See:/ 'getSchemaAsJSONResponse' smart constructor.
data GetSchemaAsJSONResponse = GetSchemaAsJSONResponse'
  { _gsajrsDocument       :: !(Maybe Text)
  , _gsajrsName           :: !(Maybe Text)
  , _gsajrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSchemaAsJSONResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsajrsDocument' - The JSON representation of the schema document.
--
-- * 'gsajrsName' - The name of the retrieved schema.
--
-- * 'gsajrsResponseStatus' - -- | The response status code.
getSchemaAsJSONResponse
    :: Int -- ^ 'gsajrsResponseStatus'
    -> GetSchemaAsJSONResponse
getSchemaAsJSONResponse pResponseStatus_ =
  GetSchemaAsJSONResponse'
    { _gsajrsDocument = Nothing
    , _gsajrsName = Nothing
    , _gsajrsResponseStatus = pResponseStatus_
    }


-- | The JSON representation of the schema document.
gsajrsDocument :: Lens' GetSchemaAsJSONResponse (Maybe Text)
gsajrsDocument = lens _gsajrsDocument (\ s a -> s{_gsajrsDocument = a})

-- | The name of the retrieved schema.
gsajrsName :: Lens' GetSchemaAsJSONResponse (Maybe Text)
gsajrsName = lens _gsajrsName (\ s a -> s{_gsajrsName = a})

-- | -- | The response status code.
gsajrsResponseStatus :: Lens' GetSchemaAsJSONResponse Int
gsajrsResponseStatus = lens _gsajrsResponseStatus (\ s a -> s{_gsajrsResponseStatus = a})

instance NFData GetSchemaAsJSONResponse where
