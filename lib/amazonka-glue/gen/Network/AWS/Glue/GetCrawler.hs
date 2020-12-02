{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetCrawler
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves metadata for a specified crawler.
module Network.AWS.Glue.GetCrawler
  ( -- * Creating a Request
    getCrawler,
    GetCrawler,

    -- * Request Lenses
    gccName,

    -- * Destructuring the Response
    getCrawlerResponse,
    GetCrawlerResponse,

    -- * Response Lenses
    getcrawlerersCrawler,
    getcrawlerersResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getCrawler' smart constructor.
newtype GetCrawler = GetCrawler' {_gccName :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetCrawler' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gccName' - The name of the crawler to retrieve metadata for.
getCrawler ::
  -- | 'gccName'
  Text ->
  GetCrawler
getCrawler pName_ = GetCrawler' {_gccName = pName_}

-- | The name of the crawler to retrieve metadata for.
gccName :: Lens' GetCrawler Text
gccName = lens _gccName (\s a -> s {_gccName = a})

instance AWSRequest GetCrawler where
  type Rs GetCrawler = GetCrawlerResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          GetCrawlerResponse' <$> (x .?> "Crawler") <*> (pure (fromEnum s))
      )

instance Hashable GetCrawler

instance NFData GetCrawler

instance ToHeaders GetCrawler where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.GetCrawler" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetCrawler where
  toJSON GetCrawler' {..} =
    object (catMaybes [Just ("Name" .= _gccName)])

instance ToPath GetCrawler where
  toPath = const "/"

instance ToQuery GetCrawler where
  toQuery = const mempty

-- | /See:/ 'getCrawlerResponse' smart constructor.
data GetCrawlerResponse = GetCrawlerResponse'
  { _getcrawlerersCrawler ::
      !(Maybe Crawler),
    _getcrawlerersResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetCrawlerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'getcrawlerersCrawler' - The metadata for the specified crawler.
--
-- * 'getcrawlerersResponseStatus' - -- | The response status code.
getCrawlerResponse ::
  -- | 'getcrawlerersResponseStatus'
  Int ->
  GetCrawlerResponse
getCrawlerResponse pResponseStatus_ =
  GetCrawlerResponse'
    { _getcrawlerersCrawler = Nothing,
      _getcrawlerersResponseStatus = pResponseStatus_
    }

-- | The metadata for the specified crawler.
getcrawlerersCrawler :: Lens' GetCrawlerResponse (Maybe Crawler)
getcrawlerersCrawler = lens _getcrawlerersCrawler (\s a -> s {_getcrawlerersCrawler = a})

-- | -- | The response status code.
getcrawlerersResponseStatus :: Lens' GetCrawlerResponse Int
getcrawlerersResponseStatus = lens _getcrawlerersResponseStatus (\s a -> s {_getcrawlerersResponseStatus = a})

instance NFData GetCrawlerResponse
