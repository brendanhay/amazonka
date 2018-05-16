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
-- Module      : Network.AWS.CloudWatchLogs.UntagLogGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified tags from the specified log group.
--
--
-- To list the tags for a log group, use 'ListTagsLogGroup' . To add tags, use 'UntagLogGroup' .
--
module Network.AWS.CloudWatchLogs.UntagLogGroup
    (
    -- * Creating a Request
      untagLogGroup
    , UntagLogGroup
    -- * Request Lenses
    , ulgLogGroupName
    , ulgTags

    -- * Destructuring the Response
    , untagLogGroupResponse
    , UntagLogGroupResponse
    ) where

import Network.AWS.CloudWatchLogs.Types
import Network.AWS.CloudWatchLogs.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'untagLogGroup' smart constructor.
data UntagLogGroup = UntagLogGroup'
  { _ulgLogGroupName :: !Text
  , _ulgTags         :: !(List1 Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UntagLogGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ulgLogGroupName' - The name of the log group.
--
-- * 'ulgTags' - The tag keys. The corresponding tags are removed from the log group.
untagLogGroup
    :: Text -- ^ 'ulgLogGroupName'
    -> NonEmpty Text -- ^ 'ulgTags'
    -> UntagLogGroup
untagLogGroup pLogGroupName_ pTags_ =
  UntagLogGroup' {_ulgLogGroupName = pLogGroupName_, _ulgTags = _List1 # pTags_}


-- | The name of the log group.
ulgLogGroupName :: Lens' UntagLogGroup Text
ulgLogGroupName = lens _ulgLogGroupName (\ s a -> s{_ulgLogGroupName = a})

-- | The tag keys. The corresponding tags are removed from the log group.
ulgTags :: Lens' UntagLogGroup (NonEmpty Text)
ulgTags = lens _ulgTags (\ s a -> s{_ulgTags = a}) . _List1

instance AWSRequest UntagLogGroup where
        type Rs UntagLogGroup = UntagLogGroupResponse
        request = postJSON cloudWatchLogs
        response = receiveNull UntagLogGroupResponse'

instance Hashable UntagLogGroup where

instance NFData UntagLogGroup where

instance ToHeaders UntagLogGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.UntagLogGroup" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UntagLogGroup where
        toJSON UntagLogGroup'{..}
          = object
              (catMaybes
                 [Just ("logGroupName" .= _ulgLogGroupName),
                  Just ("tags" .= _ulgTags)])

instance ToPath UntagLogGroup where
        toPath = const "/"

instance ToQuery UntagLogGroup where
        toQuery = const mempty

-- | /See:/ 'untagLogGroupResponse' smart constructor.
data UntagLogGroupResponse =
  UntagLogGroupResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UntagLogGroupResponse' with the minimum fields required to make a request.
--
untagLogGroupResponse
    :: UntagLogGroupResponse
untagLogGroupResponse = UntagLogGroupResponse'


instance NFData UntagLogGroupResponse where
