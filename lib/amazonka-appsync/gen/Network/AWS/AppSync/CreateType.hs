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
-- Module      : Network.AWS.AppSync.CreateType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @Type@ object.
--
--
module Network.AWS.AppSync.CreateType
    (
    -- * Creating a Request
      createType
    , CreateType
    -- * Request Lenses
    , ctApiId
    , ctDefinition
    , ctFormat

    -- * Destructuring the Response
    , createTypeResponse
    , CreateTypeResponse
    -- * Response Lenses
    , ctrsType
    , ctrsResponseStatus
    ) where

import Network.AWS.AppSync.Types
import Network.AWS.AppSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createType' smart constructor.
data CreateType = CreateType'
  { _ctApiId      :: !Text
  , _ctDefinition :: !Text
  , _ctFormat     :: !TypeDefinitionFormat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctApiId' - The API ID.
--
-- * 'ctDefinition' - The type definition, in GraphQL Schema Definition Language (SDL) format. For more information, see the <http://graphql.org/learn/schema/ GraphQL SDL documentation> .
--
-- * 'ctFormat' - The type format: SDL or JSON.
createType
    :: Text -- ^ 'ctApiId'
    -> Text -- ^ 'ctDefinition'
    -> TypeDefinitionFormat -- ^ 'ctFormat'
    -> CreateType
createType pApiId_ pDefinition_ pFormat_ =
  CreateType'
    {_ctApiId = pApiId_, _ctDefinition = pDefinition_, _ctFormat = pFormat_}


-- | The API ID.
ctApiId :: Lens' CreateType Text
ctApiId = lens _ctApiId (\ s a -> s{_ctApiId = a})

-- | The type definition, in GraphQL Schema Definition Language (SDL) format. For more information, see the <http://graphql.org/learn/schema/ GraphQL SDL documentation> .
ctDefinition :: Lens' CreateType Text
ctDefinition = lens _ctDefinition (\ s a -> s{_ctDefinition = a})

-- | The type format: SDL or JSON.
ctFormat :: Lens' CreateType TypeDefinitionFormat
ctFormat = lens _ctFormat (\ s a -> s{_ctFormat = a})

instance AWSRequest CreateType where
        type Rs CreateType = CreateTypeResponse
        request = postJSON appSync
        response
          = receiveJSON
              (\ s h x ->
                 CreateTypeResponse' <$>
                   (x .?> "type") <*> (pure (fromEnum s)))

instance Hashable CreateType where

instance NFData CreateType where

instance ToHeaders CreateType where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateType where
        toJSON CreateType'{..}
          = object
              (catMaybes
                 [Just ("definition" .= _ctDefinition),
                  Just ("format" .= _ctFormat)])

instance ToPath CreateType where
        toPath CreateType'{..}
          = mconcat ["/v1/apis/", toBS _ctApiId, "/types"]

instance ToQuery CreateType where
        toQuery = const mempty

-- | /See:/ 'createTypeResponse' smart constructor.
data CreateTypeResponse = CreateTypeResponse'
  { _ctrsType           :: !(Maybe Type)
  , _ctrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTypeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctrsType' - The @Type@ object.
--
-- * 'ctrsResponseStatus' - -- | The response status code.
createTypeResponse
    :: Int -- ^ 'ctrsResponseStatus'
    -> CreateTypeResponse
createTypeResponse pResponseStatus_ =
  CreateTypeResponse'
    {_ctrsType = Nothing, _ctrsResponseStatus = pResponseStatus_}


-- | The @Type@ object.
ctrsType :: Lens' CreateTypeResponse (Maybe Type)
ctrsType = lens _ctrsType (\ s a -> s{_ctrsType = a})

-- | -- | The response status code.
ctrsResponseStatus :: Lens' CreateTypeResponse Int
ctrsResponseStatus = lens _ctrsResponseStatus (\ s a -> s{_ctrsResponseStatus = a})

instance NFData CreateTypeResponse where
