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
-- Module      : Network.AWS.Connect.CreateRoutingProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new routing profile.
module Network.AWS.Connect.CreateRoutingProfile
  ( -- * Creating a Request
    createRoutingProfile,
    CreateRoutingProfile,

    -- * Request Lenses
    crpQueueConfigs,
    crpTags,
    crpInstanceId,
    crpName,
    crpDescription,
    crpDefaultOutboundQueueId,
    crpMediaConcurrencies,

    -- * Destructuring the Response
    createRoutingProfileResponse,
    CreateRoutingProfileResponse,

    -- * Response Lenses
    crprsRoutingProfileARN,
    crprsRoutingProfileId,
    crprsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createRoutingProfile' smart constructor.
data CreateRoutingProfile = CreateRoutingProfile'
  { _crpQueueConfigs ::
      !(Maybe (List1 RoutingProfileQueueConfig)),
    _crpTags :: !(Maybe (Map Text (Text))),
    _crpInstanceId :: !Text,
    _crpName :: !Text,
    _crpDescription :: !Text,
    _crpDefaultOutboundQueueId :: !Text,
    _crpMediaConcurrencies :: ![MediaConcurrency]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateRoutingProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crpQueueConfigs' - The inbound queues associated with the routing profile. If no queue is added, the agent can only make outbound calls.
--
-- * 'crpTags' - One or more tags.
--
-- * 'crpInstanceId' - The identifier of the Amazon Connect instance.
--
-- * 'crpName' - The name of the routing profile. Must not be more than 127 characters.
--
-- * 'crpDescription' - Description of the routing profile. Must not be more than 250 characters.
--
-- * 'crpDefaultOutboundQueueId' - The default outbound queue for the routing profile.
--
-- * 'crpMediaConcurrencies' - The channels agents can handle in the Contact Control Panel (CCP) for this routing profile.
createRoutingProfile ::
  -- | 'crpInstanceId'
  Text ->
  -- | 'crpName'
  Text ->
  -- | 'crpDescription'
  Text ->
  -- | 'crpDefaultOutboundQueueId'
  Text ->
  CreateRoutingProfile
createRoutingProfile
  pInstanceId_
  pName_
  pDescription_
  pDefaultOutboundQueueId_ =
    CreateRoutingProfile'
      { _crpQueueConfigs = Nothing,
        _crpTags = Nothing,
        _crpInstanceId = pInstanceId_,
        _crpName = pName_,
        _crpDescription = pDescription_,
        _crpDefaultOutboundQueueId = pDefaultOutboundQueueId_,
        _crpMediaConcurrencies = mempty
      }

-- | The inbound queues associated with the routing profile. If no queue is added, the agent can only make outbound calls.
crpQueueConfigs :: Lens' CreateRoutingProfile (Maybe (NonEmpty RoutingProfileQueueConfig))
crpQueueConfigs = lens _crpQueueConfigs (\s a -> s {_crpQueueConfigs = a}) . mapping _List1

-- | One or more tags.
crpTags :: Lens' CreateRoutingProfile (HashMap Text (Text))
crpTags = lens _crpTags (\s a -> s {_crpTags = a}) . _Default . _Map

-- | The identifier of the Amazon Connect instance.
crpInstanceId :: Lens' CreateRoutingProfile Text
crpInstanceId = lens _crpInstanceId (\s a -> s {_crpInstanceId = a})

-- | The name of the routing profile. Must not be more than 127 characters.
crpName :: Lens' CreateRoutingProfile Text
crpName = lens _crpName (\s a -> s {_crpName = a})

-- | Description of the routing profile. Must not be more than 250 characters.
crpDescription :: Lens' CreateRoutingProfile Text
crpDescription = lens _crpDescription (\s a -> s {_crpDescription = a})

-- | The default outbound queue for the routing profile.
crpDefaultOutboundQueueId :: Lens' CreateRoutingProfile Text
crpDefaultOutboundQueueId = lens _crpDefaultOutboundQueueId (\s a -> s {_crpDefaultOutboundQueueId = a})

-- | The channels agents can handle in the Contact Control Panel (CCP) for this routing profile.
crpMediaConcurrencies :: Lens' CreateRoutingProfile [MediaConcurrency]
crpMediaConcurrencies = lens _crpMediaConcurrencies (\s a -> s {_crpMediaConcurrencies = a}) . _Coerce

instance AWSRequest CreateRoutingProfile where
  type Rs CreateRoutingProfile = CreateRoutingProfileResponse
  request = putJSON connect
  response =
    receiveJSON
      ( \s h x ->
          CreateRoutingProfileResponse'
            <$> (x .?> "RoutingProfileArn")
            <*> (x .?> "RoutingProfileId")
            <*> (pure (fromEnum s))
      )

instance Hashable CreateRoutingProfile

instance NFData CreateRoutingProfile

instance ToHeaders CreateRoutingProfile where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON CreateRoutingProfile where
  toJSON CreateRoutingProfile' {..} =
    object
      ( catMaybes
          [ ("QueueConfigs" .=) <$> _crpQueueConfigs,
            ("Tags" .=) <$> _crpTags,
            Just ("Name" .= _crpName),
            Just ("Description" .= _crpDescription),
            Just ("DefaultOutboundQueueId" .= _crpDefaultOutboundQueueId),
            Just ("MediaConcurrencies" .= _crpMediaConcurrencies)
          ]
      )

instance ToPath CreateRoutingProfile where
  toPath CreateRoutingProfile' {..} =
    mconcat ["/routing-profiles/", toBS _crpInstanceId]

instance ToQuery CreateRoutingProfile where
  toQuery = const mempty

-- | /See:/ 'createRoutingProfileResponse' smart constructor.
data CreateRoutingProfileResponse = CreateRoutingProfileResponse'
  { _crprsRoutingProfileARN ::
      !(Maybe Text),
    _crprsRoutingProfileId ::
      !(Maybe Text),
    _crprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateRoutingProfileResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crprsRoutingProfileARN' - The Amazon Resource Name (ARN) of the routing profile.
--
-- * 'crprsRoutingProfileId' - The identifier of the routing profile.
--
-- * 'crprsResponseStatus' - -- | The response status code.
createRoutingProfileResponse ::
  -- | 'crprsResponseStatus'
  Int ->
  CreateRoutingProfileResponse
createRoutingProfileResponse pResponseStatus_ =
  CreateRoutingProfileResponse'
    { _crprsRoutingProfileARN = Nothing,
      _crprsRoutingProfileId = Nothing,
      _crprsResponseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the routing profile.
crprsRoutingProfileARN :: Lens' CreateRoutingProfileResponse (Maybe Text)
crprsRoutingProfileARN = lens _crprsRoutingProfileARN (\s a -> s {_crprsRoutingProfileARN = a})

-- | The identifier of the routing profile.
crprsRoutingProfileId :: Lens' CreateRoutingProfileResponse (Maybe Text)
crprsRoutingProfileId = lens _crprsRoutingProfileId (\s a -> s {_crprsRoutingProfileId = a})

-- | -- | The response status code.
crprsResponseStatus :: Lens' CreateRoutingProfileResponse Int
crprsResponseStatus = lens _crprsResponseStatus (\s a -> s {_crprsResponseStatus = a})

instance NFData CreateRoutingProfileResponse
