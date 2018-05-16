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
-- Module      : Network.AWS.Glue.GetMapping
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates mappings.
--
--
module Network.AWS.Glue.GetMapping
    (
    -- * Creating a Request
      getMapping
    , GetMapping
    -- * Request Lenses
    , gmSinks
    , gmLocation
    , gmSource

    -- * Destructuring the Response
    , getMappingResponse
    , GetMappingResponse
    -- * Response Lenses
    , gmrsResponseStatus
    , gmrsMapping
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getMapping' smart constructor.
data GetMapping = GetMapping'
  { _gmSinks    :: !(Maybe [CatalogEntry])
  , _gmLocation :: !(Maybe Location)
  , _gmSource   :: !CatalogEntry
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmSinks' - A list of target tables.
--
-- * 'gmLocation' - Parameters for the mapping.
--
-- * 'gmSource' - Specifies the source table.
getMapping
    :: CatalogEntry -- ^ 'gmSource'
    -> GetMapping
getMapping pSource_ =
  GetMapping' {_gmSinks = Nothing, _gmLocation = Nothing, _gmSource = pSource_}


-- | A list of target tables.
gmSinks :: Lens' GetMapping [CatalogEntry]
gmSinks = lens _gmSinks (\ s a -> s{_gmSinks = a}) . _Default . _Coerce

-- | Parameters for the mapping.
gmLocation :: Lens' GetMapping (Maybe Location)
gmLocation = lens _gmLocation (\ s a -> s{_gmLocation = a})

-- | Specifies the source table.
gmSource :: Lens' GetMapping CatalogEntry
gmSource = lens _gmSource (\ s a -> s{_gmSource = a})

instance AWSRequest GetMapping where
        type Rs GetMapping = GetMappingResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 GetMappingResponse' <$>
                   (pure (fromEnum s)) <*> (x .?> "Mapping" .!@ mempty))

instance Hashable GetMapping where

instance NFData GetMapping where

instance ToHeaders GetMapping where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.GetMapping" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetMapping where
        toJSON GetMapping'{..}
          = object
              (catMaybes
                 [("Sinks" .=) <$> _gmSinks,
                  ("Location" .=) <$> _gmLocation,
                  Just ("Source" .= _gmSource)])

instance ToPath GetMapping where
        toPath = const "/"

instance ToQuery GetMapping where
        toQuery = const mempty

-- | /See:/ 'getMappingResponse' smart constructor.
data GetMappingResponse = GetMappingResponse'
  { _gmrsResponseStatus :: !Int
  , _gmrsMapping        :: ![MappingEntry]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetMappingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmrsResponseStatus' - -- | The response status code.
--
-- * 'gmrsMapping' - A list of mappings to the specified targets.
getMappingResponse
    :: Int -- ^ 'gmrsResponseStatus'
    -> GetMappingResponse
getMappingResponse pResponseStatus_ =
  GetMappingResponse'
    {_gmrsResponseStatus = pResponseStatus_, _gmrsMapping = mempty}


-- | -- | The response status code.
gmrsResponseStatus :: Lens' GetMappingResponse Int
gmrsResponseStatus = lens _gmrsResponseStatus (\ s a -> s{_gmrsResponseStatus = a})

-- | A list of mappings to the specified targets.
gmrsMapping :: Lens' GetMappingResponse [MappingEntry]
gmrsMapping = lens _gmrsMapping (\ s a -> s{_gmrsMapping = a}) . _Coerce

instance NFData GetMappingResponse where
