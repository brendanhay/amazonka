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
-- Module      : Network.AWS.MediaPackage.RotateChannelCredentials
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the Channel ingest username and password.
module Network.AWS.MediaPackage.RotateChannelCredentials
    (
    -- * Creating a Request
      rotateChannelCredentials
    , RotateChannelCredentials
    -- * Request Lenses
    , rccId

    -- * Destructuring the Response
    , rotateChannelCredentialsResponse
    , RotateChannelCredentialsResponse
    -- * Response Lenses
    , rccrsHlsIngest
    , rccrsARN
    , rccrsId
    , rccrsDescription
    , rccrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaPackage.Types
import Network.AWS.MediaPackage.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'rotateChannelCredentials' smart constructor.
newtype RotateChannelCredentials = RotateChannelCredentials'
  { _rccId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RotateChannelCredentials' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rccId' - The ID of the channel to update.
rotateChannelCredentials
    :: Text -- ^ 'rccId'
    -> RotateChannelCredentials
rotateChannelCredentials pId_ = RotateChannelCredentials' {_rccId = pId_}


-- | The ID of the channel to update.
rccId :: Lens' RotateChannelCredentials Text
rccId = lens _rccId (\ s a -> s{_rccId = a})

instance AWSRequest RotateChannelCredentials where
        type Rs RotateChannelCredentials =
             RotateChannelCredentialsResponse
        request = putJSON mediaPackage
        response
          = receiveJSON
              (\ s h x ->
                 RotateChannelCredentialsResponse' <$>
                   (x .?> "hlsIngest") <*> (x .?> "arn") <*>
                     (x .?> "id")
                     <*> (x .?> "description")
                     <*> (pure (fromEnum s)))

instance Hashable RotateChannelCredentials where

instance NFData RotateChannelCredentials where

instance ToHeaders RotateChannelCredentials where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RotateChannelCredentials where
        toJSON = const (Object mempty)

instance ToPath RotateChannelCredentials where
        toPath RotateChannelCredentials'{..}
          = mconcat ["/channels/", toBS _rccId, "/credentials"]

instance ToQuery RotateChannelCredentials where
        toQuery = const mempty

-- | /See:/ 'rotateChannelCredentialsResponse' smart constructor.
data RotateChannelCredentialsResponse = RotateChannelCredentialsResponse'
  { _rccrsHlsIngest      :: !(Maybe HlsIngest)
  , _rccrsARN            :: !(Maybe Text)
  , _rccrsId             :: !(Maybe Text)
  , _rccrsDescription    :: !(Maybe Text)
  , _rccrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RotateChannelCredentialsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rccrsHlsIngest' - Undocumented member.
--
-- * 'rccrsARN' - The Amazon Resource Name (ARN) assigned to the Channel.
--
-- * 'rccrsId' - The ID of the Channel.
--
-- * 'rccrsDescription' - A short text description of the Channel.
--
-- * 'rccrsResponseStatus' - -- | The response status code.
rotateChannelCredentialsResponse
    :: Int -- ^ 'rccrsResponseStatus'
    -> RotateChannelCredentialsResponse
rotateChannelCredentialsResponse pResponseStatus_ =
  RotateChannelCredentialsResponse'
    { _rccrsHlsIngest = Nothing
    , _rccrsARN = Nothing
    , _rccrsId = Nothing
    , _rccrsDescription = Nothing
    , _rccrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
rccrsHlsIngest :: Lens' RotateChannelCredentialsResponse (Maybe HlsIngest)
rccrsHlsIngest = lens _rccrsHlsIngest (\ s a -> s{_rccrsHlsIngest = a})

-- | The Amazon Resource Name (ARN) assigned to the Channel.
rccrsARN :: Lens' RotateChannelCredentialsResponse (Maybe Text)
rccrsARN = lens _rccrsARN (\ s a -> s{_rccrsARN = a})

-- | The ID of the Channel.
rccrsId :: Lens' RotateChannelCredentialsResponse (Maybe Text)
rccrsId = lens _rccrsId (\ s a -> s{_rccrsId = a})

-- | A short text description of the Channel.
rccrsDescription :: Lens' RotateChannelCredentialsResponse (Maybe Text)
rccrsDescription = lens _rccrsDescription (\ s a -> s{_rccrsDescription = a})

-- | -- | The response status code.
rccrsResponseStatus :: Lens' RotateChannelCredentialsResponse Int
rccrsResponseStatus = lens _rccrsResponseStatus (\ s a -> s{_rccrsResponseStatus = a})

instance NFData RotateChannelCredentialsResponse
         where
