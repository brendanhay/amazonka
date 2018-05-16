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
-- Module      : Network.AWS.Organizations.MoveAccount
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Moves an account from its current source parent root or OU to the specified destination parent root or OU.
--
--
-- This operation can be called only from the organization's master account.
--
module Network.AWS.Organizations.MoveAccount
    (
    -- * Creating a Request
      moveAccount
    , MoveAccount
    -- * Request Lenses
    , maAccountId
    , maSourceParentId
    , maDestinationParentId

    -- * Destructuring the Response
    , moveAccountResponse
    , MoveAccountResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Organizations.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'moveAccount' smart constructor.
data MoveAccount = MoveAccount'
  { _maAccountId           :: !Text
  , _maSourceParentId      :: !Text
  , _maDestinationParentId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MoveAccount' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'maAccountId' - The unique identifier (ID) of the account that you want to move. The <http://wikipedia.org/wiki/regex regex pattern> for an account ID string requires exactly 12 digits.
--
-- * 'maSourceParentId' - The unique identifier (ID) of the root or organizational unit that you want to move the account from. The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID string requires one of the following:     * Root: a string that begins with "r-" followed by from 4 to 32 lower-case letters or digits.     * Organizational unit (OU): a string that begins with "ou-" followed by from 4 to 32 lower-case letters or digits (the ID of the root that the OU is in) followed by a second "-" dash and from 8 to 32 additional lower-case letters or digits.
--
-- * 'maDestinationParentId' - The unique identifier (ID) of the root or organizational unit that you want to move the account to. The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID string requires one of the following:     * Root: a string that begins with "r-" followed by from 4 to 32 lower-case letters or digits.     * Organizational unit (OU): a string that begins with "ou-" followed by from 4 to 32 lower-case letters or digits (the ID of the root that the OU is in) followed by a second "-" dash and from 8 to 32 additional lower-case letters or digits.
moveAccount
    :: Text -- ^ 'maAccountId'
    -> Text -- ^ 'maSourceParentId'
    -> Text -- ^ 'maDestinationParentId'
    -> MoveAccount
moveAccount pAccountId_ pSourceParentId_ pDestinationParentId_ =
  MoveAccount'
    { _maAccountId = pAccountId_
    , _maSourceParentId = pSourceParentId_
    , _maDestinationParentId = pDestinationParentId_
    }


-- | The unique identifier (ID) of the account that you want to move. The <http://wikipedia.org/wiki/regex regex pattern> for an account ID string requires exactly 12 digits.
maAccountId :: Lens' MoveAccount Text
maAccountId = lens _maAccountId (\ s a -> s{_maAccountId = a})

-- | The unique identifier (ID) of the root or organizational unit that you want to move the account from. The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID string requires one of the following:     * Root: a string that begins with "r-" followed by from 4 to 32 lower-case letters or digits.     * Organizational unit (OU): a string that begins with "ou-" followed by from 4 to 32 lower-case letters or digits (the ID of the root that the OU is in) followed by a second "-" dash and from 8 to 32 additional lower-case letters or digits.
maSourceParentId :: Lens' MoveAccount Text
maSourceParentId = lens _maSourceParentId (\ s a -> s{_maSourceParentId = a})

-- | The unique identifier (ID) of the root or organizational unit that you want to move the account to. The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID string requires one of the following:     * Root: a string that begins with "r-" followed by from 4 to 32 lower-case letters or digits.     * Organizational unit (OU): a string that begins with "ou-" followed by from 4 to 32 lower-case letters or digits (the ID of the root that the OU is in) followed by a second "-" dash and from 8 to 32 additional lower-case letters or digits.
maDestinationParentId :: Lens' MoveAccount Text
maDestinationParentId = lens _maDestinationParentId (\ s a -> s{_maDestinationParentId = a})

instance AWSRequest MoveAccount where
        type Rs MoveAccount = MoveAccountResponse
        request = postJSON organizations
        response = receiveNull MoveAccountResponse'

instance Hashable MoveAccount where

instance NFData MoveAccount where

instance ToHeaders MoveAccount where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSOrganizationsV20161128.MoveAccount" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON MoveAccount where
        toJSON MoveAccount'{..}
          = object
              (catMaybes
                 [Just ("AccountId" .= _maAccountId),
                  Just ("SourceParentId" .= _maSourceParentId),
                  Just
                    ("DestinationParentId" .= _maDestinationParentId)])

instance ToPath MoveAccount where
        toPath = const "/"

instance ToQuery MoveAccount where
        toQuery = const mempty

-- | /See:/ 'moveAccountResponse' smart constructor.
data MoveAccountResponse =
  MoveAccountResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MoveAccountResponse' with the minimum fields required to make a request.
--
moveAccountResponse
    :: MoveAccountResponse
moveAccountResponse = MoveAccountResponse'


instance NFData MoveAccountResponse where
