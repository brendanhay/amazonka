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
-- Module      : Network.AWS.MediaLive.DescribeInput
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Produces details about an input
module Network.AWS.MediaLive.DescribeInput
    (
    -- * Creating a Request
      describeInput
    , DescribeInput
    -- * Request Lenses
    , dInputId

    -- * Destructuring the Response
    , describeInputResponse
    , DescribeInputResponse
    -- * Response Lenses
    , diirsState
    , diirsSecurityGroups
    , diirsARN
    , diirsSources
    , diirsDestinations
    , diirsName
    , diirsAttachedChannels
    , diirsId
    , diirsType
    , diirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.MediaLive.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Placeholder documentation for DescribeInputRequest
--
-- /See:/ 'describeInput' smart constructor.
newtype DescribeInput = DescribeInput'
  { _dInputId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeInput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dInputId' - Unique ID of the input
describeInput
    :: Text -- ^ 'dInputId'
    -> DescribeInput
describeInput pInputId_ = DescribeInput' {_dInputId = pInputId_}


-- | Unique ID of the input
dInputId :: Lens' DescribeInput Text
dInputId = lens _dInputId (\ s a -> s{_dInputId = a})

instance AWSRequest DescribeInput where
        type Rs DescribeInput = DescribeInputResponse
        request = get mediaLive
        response
          = receiveJSON
              (\ s h x ->
                 DescribeInputResponse' <$>
                   (x .?> "state") <*>
                     (x .?> "securityGroups" .!@ mempty)
                     <*> (x .?> "arn")
                     <*> (x .?> "sources" .!@ mempty)
                     <*> (x .?> "destinations" .!@ mempty)
                     <*> (x .?> "name")
                     <*> (x .?> "attachedChannels" .!@ mempty)
                     <*> (x .?> "id")
                     <*> (x .?> "type")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeInput where

instance NFData DescribeInput where

instance ToHeaders DescribeInput where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DescribeInput where
        toPath DescribeInput'{..}
          = mconcat ["/prod/inputs/", toBS _dInputId]

instance ToQuery DescribeInput where
        toQuery = const mempty

-- | Placeholder documentation for DescribeInputResponse
--
-- /See:/ 'describeInputResponse' smart constructor.
data DescribeInputResponse = DescribeInputResponse'
  { _diirsState            :: !(Maybe InputState)
  , _diirsSecurityGroups   :: !(Maybe [Text])
  , _diirsARN              :: !(Maybe Text)
  , _diirsSources          :: !(Maybe [InputSource])
  , _diirsDestinations     :: !(Maybe [InputDestination])
  , _diirsName             :: !(Maybe Text)
  , _diirsAttachedChannels :: !(Maybe [Text])
  , _diirsId               :: !(Maybe Text)
  , _diirsType             :: !(Maybe InputType)
  , _diirsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeInputResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diirsState' - Undocumented member.
--
-- * 'diirsSecurityGroups' - A list of IDs for all the security groups attached to the input.
--
-- * 'diirsARN' - The Unique ARN of the input (generated, immutable).
--
-- * 'diirsSources' - A list of the sources of the input (PULL-type).
--
-- * 'diirsDestinations' - A list of the destinations of the input (PUSH-type).
--
-- * 'diirsName' - The user-assigned name (This is a mutable value).
--
-- * 'diirsAttachedChannels' - A list of channel IDs that that input is attached to (currently an input can only be attached to one channel).
--
-- * 'diirsId' - The generated ID of the input (unique for user account, immutable).
--
-- * 'diirsType' - Undocumented member.
--
-- * 'diirsResponseStatus' - -- | The response status code.
describeInputResponse
    :: Int -- ^ 'diirsResponseStatus'
    -> DescribeInputResponse
describeInputResponse pResponseStatus_ =
  DescribeInputResponse'
    { _diirsState = Nothing
    , _diirsSecurityGroups = Nothing
    , _diirsARN = Nothing
    , _diirsSources = Nothing
    , _diirsDestinations = Nothing
    , _diirsName = Nothing
    , _diirsAttachedChannels = Nothing
    , _diirsId = Nothing
    , _diirsType = Nothing
    , _diirsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
diirsState :: Lens' DescribeInputResponse (Maybe InputState)
diirsState = lens _diirsState (\ s a -> s{_diirsState = a})

-- | A list of IDs for all the security groups attached to the input.
diirsSecurityGroups :: Lens' DescribeInputResponse [Text]
diirsSecurityGroups = lens _diirsSecurityGroups (\ s a -> s{_diirsSecurityGroups = a}) . _Default . _Coerce

-- | The Unique ARN of the input (generated, immutable).
diirsARN :: Lens' DescribeInputResponse (Maybe Text)
diirsARN = lens _diirsARN (\ s a -> s{_diirsARN = a})

-- | A list of the sources of the input (PULL-type).
diirsSources :: Lens' DescribeInputResponse [InputSource]
diirsSources = lens _diirsSources (\ s a -> s{_diirsSources = a}) . _Default . _Coerce

-- | A list of the destinations of the input (PUSH-type).
diirsDestinations :: Lens' DescribeInputResponse [InputDestination]
diirsDestinations = lens _diirsDestinations (\ s a -> s{_diirsDestinations = a}) . _Default . _Coerce

-- | The user-assigned name (This is a mutable value).
diirsName :: Lens' DescribeInputResponse (Maybe Text)
diirsName = lens _diirsName (\ s a -> s{_diirsName = a})

-- | A list of channel IDs that that input is attached to (currently an input can only be attached to one channel).
diirsAttachedChannels :: Lens' DescribeInputResponse [Text]
diirsAttachedChannels = lens _diirsAttachedChannels (\ s a -> s{_diirsAttachedChannels = a}) . _Default . _Coerce

-- | The generated ID of the input (unique for user account, immutable).
diirsId :: Lens' DescribeInputResponse (Maybe Text)
diirsId = lens _diirsId (\ s a -> s{_diirsId = a})

-- | Undocumented member.
diirsType :: Lens' DescribeInputResponse (Maybe InputType)
diirsType = lens _diirsType (\ s a -> s{_diirsType = a})

-- | -- | The response status code.
diirsResponseStatus :: Lens' DescribeInputResponse Int
diirsResponseStatus = lens _diirsResponseStatus (\ s a -> s{_diirsResponseStatus = a})

instance NFData DescribeInputResponse where
