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
-- Module      : Network.AWS.WorkMail.DescribeUser
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information regarding the user.
--
--
module Network.AWS.WorkMail.DescribeUser
    (
    -- * Creating a Request
      describeUser
    , DescribeUser
    -- * Request Lenses
    , duOrganizationId
    , duUserId

    -- * Destructuring the Response
    , describeUserResponse
    , DescribeUserResponse
    -- * Response Lenses
    , dursEmail
    , dursState
    , dursUserId
    , dursDisabledDate
    , dursName
    , dursDisplayName
    , dursUserRole
    , dursEnabledDate
    , dursResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkMail.Types
import Network.AWS.WorkMail.Types.Product

-- | /See:/ 'describeUser' smart constructor.
data DescribeUser = DescribeUser'
  { _duOrganizationId :: !Text
  , _duUserId         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'duOrganizationId' - The identifier for the organization under which the user exists.
--
-- * 'duUserId' - The identifier for the user to be described.
describeUser
    :: Text -- ^ 'duOrganizationId'
    -> Text -- ^ 'duUserId'
    -> DescribeUser
describeUser pOrganizationId_ pUserId_ =
  DescribeUser' {_duOrganizationId = pOrganizationId_, _duUserId = pUserId_}


-- | The identifier for the organization under which the user exists.
duOrganizationId :: Lens' DescribeUser Text
duOrganizationId = lens _duOrganizationId (\ s a -> s{_duOrganizationId = a})

-- | The identifier for the user to be described.
duUserId :: Lens' DescribeUser Text
duUserId = lens _duUserId (\ s a -> s{_duUserId = a})

instance AWSRequest DescribeUser where
        type Rs DescribeUser = DescribeUserResponse
        request = postJSON workMail
        response
          = receiveJSON
              (\ s h x ->
                 DescribeUserResponse' <$>
                   (x .?> "Email") <*> (x .?> "State") <*>
                     (x .?> "UserId")
                     <*> (x .?> "DisabledDate")
                     <*> (x .?> "Name")
                     <*> (x .?> "DisplayName")
                     <*> (x .?> "UserRole")
                     <*> (x .?> "EnabledDate")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeUser where

instance NFData DescribeUser where

instance ToHeaders DescribeUser where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("WorkMailService.DescribeUser" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeUser where
        toJSON DescribeUser'{..}
          = object
              (catMaybes
                 [Just ("OrganizationId" .= _duOrganizationId),
                  Just ("UserId" .= _duUserId)])

instance ToPath DescribeUser where
        toPath = const "/"

instance ToQuery DescribeUser where
        toQuery = const mempty

-- | /See:/ 'describeUserResponse' smart constructor.
data DescribeUserResponse = DescribeUserResponse'
  { _dursEmail          :: !(Maybe Text)
  , _dursState          :: !(Maybe EntityState)
  , _dursUserId         :: !(Maybe Text)
  , _dursDisabledDate   :: !(Maybe POSIX)
  , _dursName           :: !(Maybe Text)
  , _dursDisplayName    :: !(Maybe Text)
  , _dursUserRole       :: !(Maybe UserRole)
  , _dursEnabledDate    :: !(Maybe POSIX)
  , _dursResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeUserResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dursEmail' - The email of the user.
--
-- * 'dursState' - The state of a user: enabled (registered to Amazon WorkMail) or disabled (deregistered or never registered to Amazon WorkMail).
--
-- * 'dursUserId' - The identifier for the described user.
--
-- * 'dursDisabledDate' - The date and time at which the user was disabled for Amazon WorkMail usage, in UNIX epoch time format.
--
-- * 'dursName' - The name for the user.
--
-- * 'dursDisplayName' - The display name of the user.
--
-- * 'dursUserRole' - In certain cases other entities are modeled as users. If interoperability is enabled, resources are imported into Amazon WorkMail as users. Because different Amazon WorkMail organizations rely on different directory types, administrators can distinguish between a user that is not registered to Amazon WorkMail (is disabled and has a user role) and the administrative users of the directory. The values are USER, RESOURCE, and SYSTEM_USER.
--
-- * 'dursEnabledDate' - The date and time at which the user was enabled for Amazon WorkMail usage, in UNIX epoch time format.
--
-- * 'dursResponseStatus' - -- | The response status code.
describeUserResponse
    :: Int -- ^ 'dursResponseStatus'
    -> DescribeUserResponse
describeUserResponse pResponseStatus_ =
  DescribeUserResponse'
    { _dursEmail = Nothing
    , _dursState = Nothing
    , _dursUserId = Nothing
    , _dursDisabledDate = Nothing
    , _dursName = Nothing
    , _dursDisplayName = Nothing
    , _dursUserRole = Nothing
    , _dursEnabledDate = Nothing
    , _dursResponseStatus = pResponseStatus_
    }


-- | The email of the user.
dursEmail :: Lens' DescribeUserResponse (Maybe Text)
dursEmail = lens _dursEmail (\ s a -> s{_dursEmail = a})

-- | The state of a user: enabled (registered to Amazon WorkMail) or disabled (deregistered or never registered to Amazon WorkMail).
dursState :: Lens' DescribeUserResponse (Maybe EntityState)
dursState = lens _dursState (\ s a -> s{_dursState = a})

-- | The identifier for the described user.
dursUserId :: Lens' DescribeUserResponse (Maybe Text)
dursUserId = lens _dursUserId (\ s a -> s{_dursUserId = a})

-- | The date and time at which the user was disabled for Amazon WorkMail usage, in UNIX epoch time format.
dursDisabledDate :: Lens' DescribeUserResponse (Maybe UTCTime)
dursDisabledDate = lens _dursDisabledDate (\ s a -> s{_dursDisabledDate = a}) . mapping _Time

-- | The name for the user.
dursName :: Lens' DescribeUserResponse (Maybe Text)
dursName = lens _dursName (\ s a -> s{_dursName = a})

-- | The display name of the user.
dursDisplayName :: Lens' DescribeUserResponse (Maybe Text)
dursDisplayName = lens _dursDisplayName (\ s a -> s{_dursDisplayName = a})

-- | In certain cases other entities are modeled as users. If interoperability is enabled, resources are imported into Amazon WorkMail as users. Because different Amazon WorkMail organizations rely on different directory types, administrators can distinguish between a user that is not registered to Amazon WorkMail (is disabled and has a user role) and the administrative users of the directory. The values are USER, RESOURCE, and SYSTEM_USER.
dursUserRole :: Lens' DescribeUserResponse (Maybe UserRole)
dursUserRole = lens _dursUserRole (\ s a -> s{_dursUserRole = a})

-- | The date and time at which the user was enabled for Amazon WorkMail usage, in UNIX epoch time format.
dursEnabledDate :: Lens' DescribeUserResponse (Maybe UTCTime)
dursEnabledDate = lens _dursEnabledDate (\ s a -> s{_dursEnabledDate = a}) . mapping _Time

-- | -- | The response status code.
dursResponseStatus :: Lens' DescribeUserResponse Int
dursResponseStatus = lens _dursResponseStatus (\ s a -> s{_dursResponseStatus = a})

instance NFData DescribeUserResponse where
