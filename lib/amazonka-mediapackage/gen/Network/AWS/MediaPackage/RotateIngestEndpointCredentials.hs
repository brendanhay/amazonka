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
-- Module      : Network.AWS.MediaPackage.RotateIngestEndpointCredentials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rotate the IngestEndpoint's username and password, as specified by the IngestEndpoint's id.
module Network.AWS.MediaPackage.RotateIngestEndpointCredentials
  ( -- * Creating a Request
    rotateIngestEndpointCredentials,
    RotateIngestEndpointCredentials,

    -- * Request Lenses
    riecIngestEndpointId,
    riecId,

    -- * Destructuring the Response
    rotateIngestEndpointCredentialsResponse,
    RotateIngestEndpointCredentialsResponse,

    -- * Response Lenses
    riecrsIngressAccessLogs,
    riecrsHlsIngest,
    riecrsARN,
    riecrsId,
    riecrsDescription,
    riecrsEgressAccessLogs,
    riecrsTags,
    riecrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaPackage.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'rotateIngestEndpointCredentials' smart constructor.
data RotateIngestEndpointCredentials = RotateIngestEndpointCredentials'
  { _riecIngestEndpointId ::
      !Text,
    _riecId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RotateIngestEndpointCredentials' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'riecIngestEndpointId' - The id of the IngestEndpoint whose credentials should be rotated
--
-- * 'riecId' - The ID of the channel the IngestEndpoint is on.
rotateIngestEndpointCredentials ::
  -- | 'riecIngestEndpointId'
  Text ->
  -- | 'riecId'
  Text ->
  RotateIngestEndpointCredentials
rotateIngestEndpointCredentials pIngestEndpointId_ pId_ =
  RotateIngestEndpointCredentials'
    { _riecIngestEndpointId =
        pIngestEndpointId_,
      _riecId = pId_
    }

-- | The id of the IngestEndpoint whose credentials should be rotated
riecIngestEndpointId :: Lens' RotateIngestEndpointCredentials Text
riecIngestEndpointId = lens _riecIngestEndpointId (\s a -> s {_riecIngestEndpointId = a})

-- | The ID of the channel the IngestEndpoint is on.
riecId :: Lens' RotateIngestEndpointCredentials Text
riecId = lens _riecId (\s a -> s {_riecId = a})

instance AWSRequest RotateIngestEndpointCredentials where
  type
    Rs RotateIngestEndpointCredentials =
      RotateIngestEndpointCredentialsResponse
  request = putJSON mediaPackage
  response =
    receiveJSON
      ( \s h x ->
          RotateIngestEndpointCredentialsResponse'
            <$> (x .?> "ingressAccessLogs")
            <*> (x .?> "hlsIngest")
            <*> (x .?> "arn")
            <*> (x .?> "id")
            <*> (x .?> "description")
            <*> (x .?> "egressAccessLogs")
            <*> (x .?> "tags" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable RotateIngestEndpointCredentials

instance NFData RotateIngestEndpointCredentials

instance ToHeaders RotateIngestEndpointCredentials where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON RotateIngestEndpointCredentials where
  toJSON = const (Object mempty)

instance ToPath RotateIngestEndpointCredentials where
  toPath RotateIngestEndpointCredentials' {..} =
    mconcat
      [ "/channels/",
        toBS _riecId,
        "/ingest_endpoints/",
        toBS _riecIngestEndpointId,
        "/credentials"
      ]

instance ToQuery RotateIngestEndpointCredentials where
  toQuery = const mempty

-- | /See:/ 'rotateIngestEndpointCredentialsResponse' smart constructor.
data RotateIngestEndpointCredentialsResponse = RotateIngestEndpointCredentialsResponse'
  { _riecrsIngressAccessLogs ::
      !( Maybe
           IngressAccessLogs
       ),
    _riecrsHlsIngest ::
      !( Maybe
           HlsIngest
       ),
    _riecrsARN ::
      !( Maybe
           Text
       ),
    _riecrsId ::
      !( Maybe
           Text
       ),
    _riecrsDescription ::
      !( Maybe
           Text
       ),
    _riecrsEgressAccessLogs ::
      !( Maybe
           EgressAccessLogs
       ),
    _riecrsTags ::
      !( Maybe
           ( Map
               Text
               (Text)
           )
       ),
    _riecrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RotateIngestEndpointCredentialsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'riecrsIngressAccessLogs' - Undocumented member.
--
-- * 'riecrsHlsIngest' - Undocumented member.
--
-- * 'riecrsARN' - The Amazon Resource Name (ARN) assigned to the Channel.
--
-- * 'riecrsId' - The ID of the Channel.
--
-- * 'riecrsDescription' - A short text description of the Channel.
--
-- * 'riecrsEgressAccessLogs' - Undocumented member.
--
-- * 'riecrsTags' - Undocumented member.
--
-- * 'riecrsResponseStatus' - -- | The response status code.
rotateIngestEndpointCredentialsResponse ::
  -- | 'riecrsResponseStatus'
  Int ->
  RotateIngestEndpointCredentialsResponse
rotateIngestEndpointCredentialsResponse pResponseStatus_ =
  RotateIngestEndpointCredentialsResponse'
    { _riecrsIngressAccessLogs =
        Nothing,
      _riecrsHlsIngest = Nothing,
      _riecrsARN = Nothing,
      _riecrsId = Nothing,
      _riecrsDescription = Nothing,
      _riecrsEgressAccessLogs = Nothing,
      _riecrsTags = Nothing,
      _riecrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
riecrsIngressAccessLogs :: Lens' RotateIngestEndpointCredentialsResponse (Maybe IngressAccessLogs)
riecrsIngressAccessLogs = lens _riecrsIngressAccessLogs (\s a -> s {_riecrsIngressAccessLogs = a})

-- | Undocumented member.
riecrsHlsIngest :: Lens' RotateIngestEndpointCredentialsResponse (Maybe HlsIngest)
riecrsHlsIngest = lens _riecrsHlsIngest (\s a -> s {_riecrsHlsIngest = a})

-- | The Amazon Resource Name (ARN) assigned to the Channel.
riecrsARN :: Lens' RotateIngestEndpointCredentialsResponse (Maybe Text)
riecrsARN = lens _riecrsARN (\s a -> s {_riecrsARN = a})

-- | The ID of the Channel.
riecrsId :: Lens' RotateIngestEndpointCredentialsResponse (Maybe Text)
riecrsId = lens _riecrsId (\s a -> s {_riecrsId = a})

-- | A short text description of the Channel.
riecrsDescription :: Lens' RotateIngestEndpointCredentialsResponse (Maybe Text)
riecrsDescription = lens _riecrsDescription (\s a -> s {_riecrsDescription = a})

-- | Undocumented member.
riecrsEgressAccessLogs :: Lens' RotateIngestEndpointCredentialsResponse (Maybe EgressAccessLogs)
riecrsEgressAccessLogs = lens _riecrsEgressAccessLogs (\s a -> s {_riecrsEgressAccessLogs = a})

-- | Undocumented member.
riecrsTags :: Lens' RotateIngestEndpointCredentialsResponse (HashMap Text (Text))
riecrsTags = lens _riecrsTags (\s a -> s {_riecrsTags = a}) . _Default . _Map

-- | -- | The response status code.
riecrsResponseStatus :: Lens' RotateIngestEndpointCredentialsResponse Int
riecrsResponseStatus = lens _riecrsResponseStatus (\s a -> s {_riecrsResponseStatus = a})

instance NFData RotateIngestEndpointCredentialsResponse
