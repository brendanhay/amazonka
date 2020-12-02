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
-- Module      : Network.AWS.CloudWatchEvents.ListArchives
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your archives. You can either list all the archives or you can provide a prefix to match to the archive names. Filter parameters are exclusive.
module Network.AWS.CloudWatchEvents.ListArchives
  ( -- * Creating a Request
    listArchives,
    ListArchives,

    -- * Request Lenses
    laEventSourceARN,
    laState,
    laNextToken,
    laNamePrefix,
    laLimit,

    -- * Destructuring the Response
    listArchivesResponse,
    ListArchivesResponse,

    -- * Response Lenses
    larsArchives,
    larsNextToken,
    larsResponseStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listArchives' smart constructor.
data ListArchives = ListArchives'
  { _laEventSourceARN ::
      !(Maybe Text),
    _laState :: !(Maybe ArchiveState),
    _laNextToken :: !(Maybe Text),
    _laNamePrefix :: !(Maybe Text),
    _laLimit :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListArchives' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laEventSourceARN' - The ARN of the event source associated with the archive.
--
-- * 'laState' - The state of the archive.
--
-- * 'laNextToken' - The token returned by a previous call to retrieve the next set of results.
--
-- * 'laNamePrefix' - A name prefix to filter the archives returned. Only archives with name that match the prefix are returned.
--
-- * 'laLimit' - The maximum number of results to return.
listArchives ::
  ListArchives
listArchives =
  ListArchives'
    { _laEventSourceARN = Nothing,
      _laState = Nothing,
      _laNextToken = Nothing,
      _laNamePrefix = Nothing,
      _laLimit = Nothing
    }

-- | The ARN of the event source associated with the archive.
laEventSourceARN :: Lens' ListArchives (Maybe Text)
laEventSourceARN = lens _laEventSourceARN (\s a -> s {_laEventSourceARN = a})

-- | The state of the archive.
laState :: Lens' ListArchives (Maybe ArchiveState)
laState = lens _laState (\s a -> s {_laState = a})

-- | The token returned by a previous call to retrieve the next set of results.
laNextToken :: Lens' ListArchives (Maybe Text)
laNextToken = lens _laNextToken (\s a -> s {_laNextToken = a})

-- | A name prefix to filter the archives returned. Only archives with name that match the prefix are returned.
laNamePrefix :: Lens' ListArchives (Maybe Text)
laNamePrefix = lens _laNamePrefix (\s a -> s {_laNamePrefix = a})

-- | The maximum number of results to return.
laLimit :: Lens' ListArchives (Maybe Natural)
laLimit = lens _laLimit (\s a -> s {_laLimit = a}) . mapping _Nat

instance AWSRequest ListArchives where
  type Rs ListArchives = ListArchivesResponse
  request = postJSON cloudWatchEvents
  response =
    receiveJSON
      ( \s h x ->
          ListArchivesResponse'
            <$> (x .?> "Archives" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListArchives

instance NFData ListArchives

instance ToHeaders ListArchives where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSEvents.ListArchives" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListArchives where
  toJSON ListArchives' {..} =
    object
      ( catMaybes
          [ ("EventSourceArn" .=) <$> _laEventSourceARN,
            ("State" .=) <$> _laState,
            ("NextToken" .=) <$> _laNextToken,
            ("NamePrefix" .=) <$> _laNamePrefix,
            ("Limit" .=) <$> _laLimit
          ]
      )

instance ToPath ListArchives where
  toPath = const "/"

instance ToQuery ListArchives where
  toQuery = const mempty

-- | /See:/ 'listArchivesResponse' smart constructor.
data ListArchivesResponse = ListArchivesResponse'
  { _larsArchives ::
      !(Maybe [Archive]),
    _larsNextToken :: !(Maybe Text),
    _larsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListArchivesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'larsArchives' - An array of @Archive@ objects that include details about an archive.
--
-- * 'larsNextToken' - The token returned by a previous call to retrieve the next set of results.
--
-- * 'larsResponseStatus' - -- | The response status code.
listArchivesResponse ::
  -- | 'larsResponseStatus'
  Int ->
  ListArchivesResponse
listArchivesResponse pResponseStatus_ =
  ListArchivesResponse'
    { _larsArchives = Nothing,
      _larsNextToken = Nothing,
      _larsResponseStatus = pResponseStatus_
    }

-- | An array of @Archive@ objects that include details about an archive.
larsArchives :: Lens' ListArchivesResponse [Archive]
larsArchives = lens _larsArchives (\s a -> s {_larsArchives = a}) . _Default . _Coerce

-- | The token returned by a previous call to retrieve the next set of results.
larsNextToken :: Lens' ListArchivesResponse (Maybe Text)
larsNextToken = lens _larsNextToken (\s a -> s {_larsNextToken = a})

-- | -- | The response status code.
larsResponseStatus :: Lens' ListArchivesResponse Int
larsResponseStatus = lens _larsResponseStatus (\s a -> s {_larsResponseStatus = a})

instance NFData ListArchivesResponse
