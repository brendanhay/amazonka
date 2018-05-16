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
-- Module      : Network.AWS.MQ.DescribeUser
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about an ActiveMQ user.
module Network.AWS.MQ.DescribeUser
    (
    -- * Creating a Request
      describeUser
    , DescribeUser
    -- * Request Lenses
    , duUsername
    , duBrokerId

    -- * Destructuring the Response
    , describeUserResponse
    , DescribeUserResponse
    -- * Response Lenses
    , dursGroups
    , dursPending
    , dursConsoleAccess
    , dursUsername
    , dursBrokerId
    , dursResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MQ.Types
import Network.AWS.MQ.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeUser' smart constructor.
data DescribeUser = DescribeUser'
  { _duUsername :: !Text
  , _duBrokerId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'duUsername' - The username of the ActiveMQ user. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
--
-- * 'duBrokerId' - The unique ID that Amazon MQ generates for the broker.
describeUser
    :: Text -- ^ 'duUsername'
    -> Text -- ^ 'duBrokerId'
    -> DescribeUser
describeUser pUsername_ pBrokerId_ =
  DescribeUser' {_duUsername = pUsername_, _duBrokerId = pBrokerId_}


-- | The username of the ActiveMQ user. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
duUsername :: Lens' DescribeUser Text
duUsername = lens _duUsername (\ s a -> s{_duUsername = a})

-- | The unique ID that Amazon MQ generates for the broker.
duBrokerId :: Lens' DescribeUser Text
duBrokerId = lens _duBrokerId (\ s a -> s{_duBrokerId = a})

instance AWSRequest DescribeUser where
        type Rs DescribeUser = DescribeUserResponse
        request = get mq
        response
          = receiveJSON
              (\ s h x ->
                 DescribeUserResponse' <$>
                   (x .?> "groups" .!@ mempty) <*> (x .?> "pending") <*>
                     (x .?> "consoleAccess")
                     <*> (x .?> "username")
                     <*> (x .?> "brokerId")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeUser where

instance NFData DescribeUser where

instance ToHeaders DescribeUser where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DescribeUser where
        toPath DescribeUser'{..}
          = mconcat
              ["/v1/brokers/", toBS _duBrokerId, "/users/",
               toBS _duUsername]

instance ToQuery DescribeUser where
        toQuery = const mempty

-- | /See:/ 'describeUserResponse' smart constructor.
data DescribeUserResponse = DescribeUserResponse'
  { _dursGroups         :: !(Maybe [Text])
  , _dursPending        :: !(Maybe UserPendingChanges)
  , _dursConsoleAccess  :: !(Maybe Bool)
  , _dursUsername       :: !(Maybe Text)
  , _dursBrokerId       :: !(Maybe Text)
  , _dursResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeUserResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dursGroups' - The list of groups (20 maximum) to which the ActiveMQ user belongs. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
--
-- * 'dursPending' - The status of the changes pending for the ActiveMQ user.
--
-- * 'dursConsoleAccess' - Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
--
-- * 'dursUsername' - Required. The username of the ActiveMQ user. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
--
-- * 'dursBrokerId' - Required. The unique ID that Amazon MQ generates for the broker.
--
-- * 'dursResponseStatus' - -- | The response status code.
describeUserResponse
    :: Int -- ^ 'dursResponseStatus'
    -> DescribeUserResponse
describeUserResponse pResponseStatus_ =
  DescribeUserResponse'
    { _dursGroups = Nothing
    , _dursPending = Nothing
    , _dursConsoleAccess = Nothing
    , _dursUsername = Nothing
    , _dursBrokerId = Nothing
    , _dursResponseStatus = pResponseStatus_
    }


-- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
dursGroups :: Lens' DescribeUserResponse [Text]
dursGroups = lens _dursGroups (\ s a -> s{_dursGroups = a}) . _Default . _Coerce

-- | The status of the changes pending for the ActiveMQ user.
dursPending :: Lens' DescribeUserResponse (Maybe UserPendingChanges)
dursPending = lens _dursPending (\ s a -> s{_dursPending = a})

-- | Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
dursConsoleAccess :: Lens' DescribeUserResponse (Maybe Bool)
dursConsoleAccess = lens _dursConsoleAccess (\ s a -> s{_dursConsoleAccess = a})

-- | Required. The username of the ActiveMQ user. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
dursUsername :: Lens' DescribeUserResponse (Maybe Text)
dursUsername = lens _dursUsername (\ s a -> s{_dursUsername = a})

-- | Required. The unique ID that Amazon MQ generates for the broker.
dursBrokerId :: Lens' DescribeUserResponse (Maybe Text)
dursBrokerId = lens _dursBrokerId (\ s a -> s{_dursBrokerId = a})

-- | -- | The response status code.
dursResponseStatus :: Lens' DescribeUserResponse Int
dursResponseStatus = lens _dursResponseStatus (\ s a -> s{_dursResponseStatus = a})

instance NFData DescribeUserResponse where
