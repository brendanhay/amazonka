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
-- Module      : Network.AWS.AppSync.GetResolver
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a @Resolver@ object.
--
--
module Network.AWS.AppSync.GetResolver
    (
    -- * Creating a Request
      getResolver
    , GetResolver
    -- * Request Lenses
    , grApiId
    , grTypeName
    , grFieldName

    -- * Destructuring the Response
    , getResolverResponse
    , GetResolverResponse
    -- * Response Lenses
    , grrsResolver
    , grrsResponseStatus
    ) where

import Network.AWS.AppSync.Types
import Network.AWS.AppSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getResolver' smart constructor.
data GetResolver = GetResolver'
  { _grApiId     :: !Text
  , _grTypeName  :: !Text
  , _grFieldName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetResolver' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grApiId' - The API ID.
--
-- * 'grTypeName' - The resolver type name.
--
-- * 'grFieldName' - The resolver field name.
getResolver
    :: Text -- ^ 'grApiId'
    -> Text -- ^ 'grTypeName'
    -> Text -- ^ 'grFieldName'
    -> GetResolver
getResolver pApiId_ pTypeName_ pFieldName_ =
  GetResolver'
    {_grApiId = pApiId_, _grTypeName = pTypeName_, _grFieldName = pFieldName_}


-- | The API ID.
grApiId :: Lens' GetResolver Text
grApiId = lens _grApiId (\ s a -> s{_grApiId = a})

-- | The resolver type name.
grTypeName :: Lens' GetResolver Text
grTypeName = lens _grTypeName (\ s a -> s{_grTypeName = a})

-- | The resolver field name.
grFieldName :: Lens' GetResolver Text
grFieldName = lens _grFieldName (\ s a -> s{_grFieldName = a})

instance AWSRequest GetResolver where
        type Rs GetResolver = GetResolverResponse
        request = get appSync
        response
          = receiveJSON
              (\ s h x ->
                 GetResolverResponse' <$>
                   (x .?> "resolver") <*> (pure (fromEnum s)))

instance Hashable GetResolver where

instance NFData GetResolver where

instance ToHeaders GetResolver where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetResolver where
        toPath GetResolver'{..}
          = mconcat
              ["/v1/apis/", toBS _grApiId, "/types/",
               toBS _grTypeName, "/resolvers/", toBS _grFieldName]

instance ToQuery GetResolver where
        toQuery = const mempty

-- | /See:/ 'getResolverResponse' smart constructor.
data GetResolverResponse = GetResolverResponse'
  { _grrsResolver       :: !(Maybe Resolver)
  , _grrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetResolverResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grrsResolver' - The @Resolver@ object.
--
-- * 'grrsResponseStatus' - -- | The response status code.
getResolverResponse
    :: Int -- ^ 'grrsResponseStatus'
    -> GetResolverResponse
getResolverResponse pResponseStatus_ =
  GetResolverResponse'
    {_grrsResolver = Nothing, _grrsResponseStatus = pResponseStatus_}


-- | The @Resolver@ object.
grrsResolver :: Lens' GetResolverResponse (Maybe Resolver)
grrsResolver = lens _grrsResolver (\ s a -> s{_grrsResolver = a})

-- | -- | The response status code.
grrsResponseStatus :: Lens' GetResolverResponse Int
grrsResponseStatus = lens _grrsResponseStatus (\ s a -> s{_grrsResponseStatus = a})

instance NFData GetResolverResponse where
