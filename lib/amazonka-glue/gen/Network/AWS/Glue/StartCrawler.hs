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
-- Module      : Network.AWS.Glue.StartCrawler
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a crawl using the specified crawler, regardless of what is scheduled. If the crawler is already running, does nothing.
--
--
module Network.AWS.Glue.StartCrawler
    (
    -- * Creating a Request
      startCrawler
    , StartCrawler
    -- * Request Lenses
    , scName

    -- * Destructuring the Response
    , startCrawlerResponse
    , StartCrawlerResponse
    -- * Response Lenses
    , scrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startCrawler' smart constructor.
newtype StartCrawler = StartCrawler'
  { _scName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartCrawler' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scName' - Name of the crawler to start.
startCrawler
    :: Text -- ^ 'scName'
    -> StartCrawler
startCrawler pName_ = StartCrawler' {_scName = pName_}


-- | Name of the crawler to start.
scName :: Lens' StartCrawler Text
scName = lens _scName (\ s a -> s{_scName = a})

instance AWSRequest StartCrawler where
        type Rs StartCrawler = StartCrawlerResponse
        request = postJSON glue
        response
          = receiveEmpty
              (\ s h x ->
                 StartCrawlerResponse' <$> (pure (fromEnum s)))

instance Hashable StartCrawler where

instance NFData StartCrawler where

instance ToHeaders StartCrawler where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.StartCrawler" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartCrawler where
        toJSON StartCrawler'{..}
          = object (catMaybes [Just ("Name" .= _scName)])

instance ToPath StartCrawler where
        toPath = const "/"

instance ToQuery StartCrawler where
        toQuery = const mempty

-- | /See:/ 'startCrawlerResponse' smart constructor.
newtype StartCrawlerResponse = StartCrawlerResponse'
  { _scrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartCrawlerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scrsResponseStatus' - -- | The response status code.
startCrawlerResponse
    :: Int -- ^ 'scrsResponseStatus'
    -> StartCrawlerResponse
startCrawlerResponse pResponseStatus_ =
  StartCrawlerResponse' {_scrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
scrsResponseStatus :: Lens' StartCrawlerResponse Int
scrsResponseStatus = lens _scrsResponseStatus (\ s a -> s{_scrsResponseStatus = a})

instance NFData StartCrawlerResponse where
