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
-- Module      : Network.AWS.MediaPackage.ConfigureLogs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the Channel's properities to configure log subscription
module Network.AWS.MediaPackage.ConfigureLogs
  ( -- * Creating a Request
    configureLogs,
    ConfigureLogs,

    -- * Request Lenses
    clIngressAccessLogs,
    clEgressAccessLogs,
    clId,

    -- * Destructuring the Response
    configureLogsResponse,
    ConfigureLogsResponse,

    -- * Response Lenses
    clrsIngressAccessLogs,
    clrsHlsIngest,
    clrsARN,
    clrsId,
    clrsDescription,
    clrsEgressAccessLogs,
    clrsTags,
    clrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaPackage.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | the option to configure log subscription.
--
-- /See:/ 'configureLogs' smart constructor.
data ConfigureLogs = ConfigureLogs'
  { _clIngressAccessLogs ::
      !(Maybe IngressAccessLogs),
    _clEgressAccessLogs :: !(Maybe EgressAccessLogs),
    _clId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConfigureLogs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clIngressAccessLogs' - Undocumented member.
--
-- * 'clEgressAccessLogs' - Undocumented member.
--
-- * 'clId' - The ID of the channel to log subscription.
configureLogs ::
  -- | 'clId'
  Text ->
  ConfigureLogs
configureLogs pId_ =
  ConfigureLogs'
    { _clIngressAccessLogs = Nothing,
      _clEgressAccessLogs = Nothing,
      _clId = pId_
    }

-- | Undocumented member.
clIngressAccessLogs :: Lens' ConfigureLogs (Maybe IngressAccessLogs)
clIngressAccessLogs = lens _clIngressAccessLogs (\s a -> s {_clIngressAccessLogs = a})

-- | Undocumented member.
clEgressAccessLogs :: Lens' ConfigureLogs (Maybe EgressAccessLogs)
clEgressAccessLogs = lens _clEgressAccessLogs (\s a -> s {_clEgressAccessLogs = a})

-- | The ID of the channel to log subscription.
clId :: Lens' ConfigureLogs Text
clId = lens _clId (\s a -> s {_clId = a})

instance AWSRequest ConfigureLogs where
  type Rs ConfigureLogs = ConfigureLogsResponse
  request = putJSON mediaPackage
  response =
    receiveJSON
      ( \s h x ->
          ConfigureLogsResponse'
            <$> (x .?> "ingressAccessLogs")
            <*> (x .?> "hlsIngest")
            <*> (x .?> "arn")
            <*> (x .?> "id")
            <*> (x .?> "description")
            <*> (x .?> "egressAccessLogs")
            <*> (x .?> "tags" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ConfigureLogs

instance NFData ConfigureLogs

instance ToHeaders ConfigureLogs where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON ConfigureLogs where
  toJSON ConfigureLogs' {..} =
    object
      ( catMaybes
          [ ("ingressAccessLogs" .=) <$> _clIngressAccessLogs,
            ("egressAccessLogs" .=) <$> _clEgressAccessLogs
          ]
      )

instance ToPath ConfigureLogs where
  toPath ConfigureLogs' {..} =
    mconcat ["/channels/", toBS _clId, "/configure_logs"]

instance ToQuery ConfigureLogs where
  toQuery = const mempty

-- | /See:/ 'configureLogsResponse' smart constructor.
data ConfigureLogsResponse = ConfigureLogsResponse'
  { _clrsIngressAccessLogs ::
      !(Maybe IngressAccessLogs),
    _clrsHlsIngest :: !(Maybe HlsIngest),
    _clrsARN :: !(Maybe Text),
    _clrsId :: !(Maybe Text),
    _clrsDescription :: !(Maybe Text),
    _clrsEgressAccessLogs ::
      !(Maybe EgressAccessLogs),
    _clrsTags :: !(Maybe (Map Text (Text))),
    _clrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConfigureLogsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clrsIngressAccessLogs' - Undocumented member.
--
-- * 'clrsHlsIngest' - Undocumented member.
--
-- * 'clrsARN' - The Amazon Resource Name (ARN) assigned to the Channel.
--
-- * 'clrsId' - The ID of the Channel.
--
-- * 'clrsDescription' - A short text description of the Channel.
--
-- * 'clrsEgressAccessLogs' - Undocumented member.
--
-- * 'clrsTags' - Undocumented member.
--
-- * 'clrsResponseStatus' - -- | The response status code.
configureLogsResponse ::
  -- | 'clrsResponseStatus'
  Int ->
  ConfigureLogsResponse
configureLogsResponse pResponseStatus_ =
  ConfigureLogsResponse'
    { _clrsIngressAccessLogs = Nothing,
      _clrsHlsIngest = Nothing,
      _clrsARN = Nothing,
      _clrsId = Nothing,
      _clrsDescription = Nothing,
      _clrsEgressAccessLogs = Nothing,
      _clrsTags = Nothing,
      _clrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
clrsIngressAccessLogs :: Lens' ConfigureLogsResponse (Maybe IngressAccessLogs)
clrsIngressAccessLogs = lens _clrsIngressAccessLogs (\s a -> s {_clrsIngressAccessLogs = a})

-- | Undocumented member.
clrsHlsIngest :: Lens' ConfigureLogsResponse (Maybe HlsIngest)
clrsHlsIngest = lens _clrsHlsIngest (\s a -> s {_clrsHlsIngest = a})

-- | The Amazon Resource Name (ARN) assigned to the Channel.
clrsARN :: Lens' ConfigureLogsResponse (Maybe Text)
clrsARN = lens _clrsARN (\s a -> s {_clrsARN = a})

-- | The ID of the Channel.
clrsId :: Lens' ConfigureLogsResponse (Maybe Text)
clrsId = lens _clrsId (\s a -> s {_clrsId = a})

-- | A short text description of the Channel.
clrsDescription :: Lens' ConfigureLogsResponse (Maybe Text)
clrsDescription = lens _clrsDescription (\s a -> s {_clrsDescription = a})

-- | Undocumented member.
clrsEgressAccessLogs :: Lens' ConfigureLogsResponse (Maybe EgressAccessLogs)
clrsEgressAccessLogs = lens _clrsEgressAccessLogs (\s a -> s {_clrsEgressAccessLogs = a})

-- | Undocumented member.
clrsTags :: Lens' ConfigureLogsResponse (HashMap Text (Text))
clrsTags = lens _clrsTags (\s a -> s {_clrsTags = a}) . _Default . _Map

-- | -- | The response status code.
clrsResponseStatus :: Lens' ConfigureLogsResponse Int
clrsResponseStatus = lens _clrsResponseStatus (\s a -> s {_clrsResponseStatus = a})

instance NFData ConfigureLogsResponse
