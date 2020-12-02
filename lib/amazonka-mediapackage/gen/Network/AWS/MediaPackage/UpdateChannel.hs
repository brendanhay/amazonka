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
-- Module      : Network.AWS.MediaPackage.UpdateChannel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing Channel.
module Network.AWS.MediaPackage.UpdateChannel
    (
    -- * Creating a Request
      updateChannel
    , UpdateChannel
    -- * Request Lenses
    , ucDescription
    , ucId

    -- * Destructuring the Response
    , updateChannelResponse
    , UpdateChannelResponse
    -- * Response Lenses
    , ucrsHlsIngest
    , ucrsARN
    , ucrsId
    , ucrsDescription
    , ucrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaPackage.Types
import Network.AWS.MediaPackage.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Configuration parameters used to update the Channel.
--
-- /See:/ 'updateChannel' smart constructor.
data UpdateChannel = UpdateChannel'
  { _ucDescription :: !(Maybe Text)
  , _ucId          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucDescription' - A short text description of the Channel.
--
-- * 'ucId' - The ID of the Channel to update.
updateChannel
    :: Text -- ^ 'ucId'
    -> UpdateChannel
updateChannel pId_ = UpdateChannel' {_ucDescription = Nothing, _ucId = pId_}


-- | A short text description of the Channel.
ucDescription :: Lens' UpdateChannel (Maybe Text)
ucDescription = lens _ucDescription (\ s a -> s{_ucDescription = a})

-- | The ID of the Channel to update.
ucId :: Lens' UpdateChannel Text
ucId = lens _ucId (\ s a -> s{_ucId = a})

instance AWSRequest UpdateChannel where
        type Rs UpdateChannel = UpdateChannelResponse
        request = putJSON mediaPackage
        response
          = receiveJSON
              (\ s h x ->
                 UpdateChannelResponse' <$>
                   (x .?> "hlsIngest") <*> (x .?> "arn") <*>
                     (x .?> "id")
                     <*> (x .?> "description")
                     <*> (pure (fromEnum s)))

instance Hashable UpdateChannel where

instance NFData UpdateChannel where

instance ToHeaders UpdateChannel where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateChannel where
        toJSON UpdateChannel'{..}
          = object
              (catMaybes [("description" .=) <$> _ucDescription])

instance ToPath UpdateChannel where
        toPath UpdateChannel'{..}
          = mconcat ["/channels/", toBS _ucId]

instance ToQuery UpdateChannel where
        toQuery = const mempty

-- | /See:/ 'updateChannelResponse' smart constructor.
data UpdateChannelResponse = UpdateChannelResponse'
  { _ucrsHlsIngest      :: !(Maybe HlsIngest)
  , _ucrsARN            :: !(Maybe Text)
  , _ucrsId             :: !(Maybe Text)
  , _ucrsDescription    :: !(Maybe Text)
  , _ucrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucrsHlsIngest' - Undocumented member.
--
-- * 'ucrsARN' - The Amazon Resource Name (ARN) assigned to the Channel.
--
-- * 'ucrsId' - The ID of the Channel.
--
-- * 'ucrsDescription' - A short text description of the Channel.
--
-- * 'ucrsResponseStatus' - -- | The response status code.
updateChannelResponse
    :: Int -- ^ 'ucrsResponseStatus'
    -> UpdateChannelResponse
updateChannelResponse pResponseStatus_ =
  UpdateChannelResponse'
    { _ucrsHlsIngest = Nothing
    , _ucrsARN = Nothing
    , _ucrsId = Nothing
    , _ucrsDescription = Nothing
    , _ucrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
ucrsHlsIngest :: Lens' UpdateChannelResponse (Maybe HlsIngest)
ucrsHlsIngest = lens _ucrsHlsIngest (\ s a -> s{_ucrsHlsIngest = a})

-- | The Amazon Resource Name (ARN) assigned to the Channel.
ucrsARN :: Lens' UpdateChannelResponse (Maybe Text)
ucrsARN = lens _ucrsARN (\ s a -> s{_ucrsARN = a})

-- | The ID of the Channel.
ucrsId :: Lens' UpdateChannelResponse (Maybe Text)
ucrsId = lens _ucrsId (\ s a -> s{_ucrsId = a})

-- | A short text description of the Channel.
ucrsDescription :: Lens' UpdateChannelResponse (Maybe Text)
ucrsDescription = lens _ucrsDescription (\ s a -> s{_ucrsDescription = a})

-- | -- | The response status code.
ucrsResponseStatus :: Lens' UpdateChannelResponse Int
ucrsResponseStatus = lens _ucrsResponseStatus (\ s a -> s{_ucrsResponseStatus = a})

instance NFData UpdateChannelResponse where
