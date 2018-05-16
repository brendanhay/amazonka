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
-- Module      : Network.AWS.Glue.StopCrawler
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- If the specified crawler is running, stops the crawl.
--
--
module Network.AWS.Glue.StopCrawler
    (
    -- * Creating a Request
      stopCrawler
    , StopCrawler
    -- * Request Lenses
    , sName

    -- * Destructuring the Response
    , stopCrawlerResponse
    , StopCrawlerResponse
    -- * Response Lenses
    , srsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'stopCrawler' smart constructor.
newtype StopCrawler = StopCrawler'
  { _sName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopCrawler' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sName' - Name of the crawler to stop.
stopCrawler
    :: Text -- ^ 'sName'
    -> StopCrawler
stopCrawler pName_ = StopCrawler' {_sName = pName_}


-- | Name of the crawler to stop.
sName :: Lens' StopCrawler Text
sName = lens _sName (\ s a -> s{_sName = a})

instance AWSRequest StopCrawler where
        type Rs StopCrawler = StopCrawlerResponse
        request = postJSON glue
        response
          = receiveEmpty
              (\ s h x ->
                 StopCrawlerResponse' <$> (pure (fromEnum s)))

instance Hashable StopCrawler where

instance NFData StopCrawler where

instance ToHeaders StopCrawler where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.StopCrawler" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopCrawler where
        toJSON StopCrawler'{..}
          = object (catMaybes [Just ("Name" .= _sName)])

instance ToPath StopCrawler where
        toPath = const "/"

instance ToQuery StopCrawler where
        toQuery = const mempty

-- | /See:/ 'stopCrawlerResponse' smart constructor.
newtype StopCrawlerResponse = StopCrawlerResponse'
  { _srsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopCrawlerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsResponseStatus' - -- | The response status code.
stopCrawlerResponse
    :: Int -- ^ 'srsResponseStatus'
    -> StopCrawlerResponse
stopCrawlerResponse pResponseStatus_ =
  StopCrawlerResponse' {_srsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
srsResponseStatus :: Lens' StopCrawlerResponse Int
srsResponseStatus = lens _srsResponseStatus (\ s a -> s{_srsResponseStatus = a})

instance NFData StopCrawlerResponse where
