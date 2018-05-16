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
-- Module      : Network.AWS.CloudDirectory.PutSchemaFromJSON
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows a schema to be updated using JSON upload. Only available for development schemas. See <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_schemas.html#jsonformat JSON Schema Format> for more information.
--
--
module Network.AWS.CloudDirectory.PutSchemaFromJSON
    (
    -- * Creating a Request
      putSchemaFromJSON
    , PutSchemaFromJSON
    -- * Request Lenses
    , psfjSchemaARN
    , psfjDocument

    -- * Destructuring the Response
    , putSchemaFromJSONResponse
    , PutSchemaFromJSONResponse
    -- * Response Lenses
    , psfjrsARN
    , psfjrsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putSchemaFromJSON' smart constructor.
data PutSchemaFromJSON = PutSchemaFromJSON'
  { _psfjSchemaARN :: !Text
  , _psfjDocument  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutSchemaFromJSON' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psfjSchemaARN' - The ARN of the schema to update.
--
-- * 'psfjDocument' - The replacement JSON schema.
putSchemaFromJSON
    :: Text -- ^ 'psfjSchemaARN'
    -> Text -- ^ 'psfjDocument'
    -> PutSchemaFromJSON
putSchemaFromJSON pSchemaARN_ pDocument_ =
  PutSchemaFromJSON' {_psfjSchemaARN = pSchemaARN_, _psfjDocument = pDocument_}


-- | The ARN of the schema to update.
psfjSchemaARN :: Lens' PutSchemaFromJSON Text
psfjSchemaARN = lens _psfjSchemaARN (\ s a -> s{_psfjSchemaARN = a})

-- | The replacement JSON schema.
psfjDocument :: Lens' PutSchemaFromJSON Text
psfjDocument = lens _psfjDocument (\ s a -> s{_psfjDocument = a})

instance AWSRequest PutSchemaFromJSON where
        type Rs PutSchemaFromJSON = PutSchemaFromJSONResponse
        request = putJSON cloudDirectory
        response
          = receiveJSON
              (\ s h x ->
                 PutSchemaFromJSONResponse' <$>
                   (x .?> "Arn") <*> (pure (fromEnum s)))

instance Hashable PutSchemaFromJSON where

instance NFData PutSchemaFromJSON where

instance ToHeaders PutSchemaFromJSON where
        toHeaders PutSchemaFromJSON'{..}
          = mconcat ["x-amz-data-partition" =# _psfjSchemaARN]

instance ToJSON PutSchemaFromJSON where
        toJSON PutSchemaFromJSON'{..}
          = object
              (catMaybes [Just ("Document" .= _psfjDocument)])

instance ToPath PutSchemaFromJSON where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/schema/json"

instance ToQuery PutSchemaFromJSON where
        toQuery = const mempty

-- | /See:/ 'putSchemaFromJSONResponse' smart constructor.
data PutSchemaFromJSONResponse = PutSchemaFromJSONResponse'
  { _psfjrsARN            :: !(Maybe Text)
  , _psfjrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutSchemaFromJSONResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psfjrsARN' - The ARN of the schema to update.
--
-- * 'psfjrsResponseStatus' - -- | The response status code.
putSchemaFromJSONResponse
    :: Int -- ^ 'psfjrsResponseStatus'
    -> PutSchemaFromJSONResponse
putSchemaFromJSONResponse pResponseStatus_ =
  PutSchemaFromJSONResponse'
    {_psfjrsARN = Nothing, _psfjrsResponseStatus = pResponseStatus_}


-- | The ARN of the schema to update.
psfjrsARN :: Lens' PutSchemaFromJSONResponse (Maybe Text)
psfjrsARN = lens _psfjrsARN (\ s a -> s{_psfjrsARN = a})

-- | -- | The response status code.
psfjrsResponseStatus :: Lens' PutSchemaFromJSONResponse Int
psfjrsResponseStatus = lens _psfjrsResponseStatus (\ s a -> s{_psfjrsResponseStatus = a})

instance NFData PutSchemaFromJSONResponse where
