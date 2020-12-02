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
-- Module      : Network.AWS.Glue.ResetJobBookmark
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets a bookmark entry.
--
--
module Network.AWS.Glue.ResetJobBookmark
    (
    -- * Creating a Request
      resetJobBookmark
    , ResetJobBookmark
    -- * Request Lenses
    , rjbJobName

    -- * Destructuring the Response
    , resetJobBookmarkResponse
    , ResetJobBookmarkResponse
    -- * Response Lenses
    , rjbrsJobBookmarkEntry
    , rjbrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'resetJobBookmark' smart constructor.
newtype ResetJobBookmark = ResetJobBookmark'
  { _rjbJobName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResetJobBookmark' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rjbJobName' - The name of the job in question.
resetJobBookmark
    :: Text -- ^ 'rjbJobName'
    -> ResetJobBookmark
resetJobBookmark pJobName_ = ResetJobBookmark' {_rjbJobName = pJobName_}


-- | The name of the job in question.
rjbJobName :: Lens' ResetJobBookmark Text
rjbJobName = lens _rjbJobName (\ s a -> s{_rjbJobName = a})

instance AWSRequest ResetJobBookmark where
        type Rs ResetJobBookmark = ResetJobBookmarkResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 ResetJobBookmarkResponse' <$>
                   (x .?> "JobBookmarkEntry") <*> (pure (fromEnum s)))

instance Hashable ResetJobBookmark where

instance NFData ResetJobBookmark where

instance ToHeaders ResetJobBookmark where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.ResetJobBookmark" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ResetJobBookmark where
        toJSON ResetJobBookmark'{..}
          = object
              (catMaybes [Just ("JobName" .= _rjbJobName)])

instance ToPath ResetJobBookmark where
        toPath = const "/"

instance ToQuery ResetJobBookmark where
        toQuery = const mempty

-- | /See:/ 'resetJobBookmarkResponse' smart constructor.
data ResetJobBookmarkResponse = ResetJobBookmarkResponse'
  { _rjbrsJobBookmarkEntry :: !(Maybe JobBookmarkEntry)
  , _rjbrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResetJobBookmarkResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rjbrsJobBookmarkEntry' - The reset bookmark entry.
--
-- * 'rjbrsResponseStatus' - -- | The response status code.
resetJobBookmarkResponse
    :: Int -- ^ 'rjbrsResponseStatus'
    -> ResetJobBookmarkResponse
resetJobBookmarkResponse pResponseStatus_ =
  ResetJobBookmarkResponse'
    {_rjbrsJobBookmarkEntry = Nothing, _rjbrsResponseStatus = pResponseStatus_}


-- | The reset bookmark entry.
rjbrsJobBookmarkEntry :: Lens' ResetJobBookmarkResponse (Maybe JobBookmarkEntry)
rjbrsJobBookmarkEntry = lens _rjbrsJobBookmarkEntry (\ s a -> s{_rjbrsJobBookmarkEntry = a})

-- | -- | The response status code.
rjbrsResponseStatus :: Lens' ResetJobBookmarkResponse Int
rjbrsResponseStatus = lens _rjbrsResponseStatus (\ s a -> s{_rjbrsResponseStatus = a})

instance NFData ResetJobBookmarkResponse where
