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
-- Module      : Network.AWS.IoT.CreateRoleAlias
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a role alias.
--
--
module Network.AWS.IoT.CreateRoleAlias
    (
    -- * Creating a Request
      createRoleAlias
    , CreateRoleAlias
    -- * Request Lenses
    , craCredentialDurationSeconds
    , craRoleAlias
    , craRoleARN

    -- * Destructuring the Response
    , createRoleAliasResponse
    , CreateRoleAliasResponse
    -- * Response Lenses
    , crarsRoleAliasARN
    , crarsRoleAlias
    , crarsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createRoleAlias' smart constructor.
data CreateRoleAlias = CreateRoleAlias'
  { _craCredentialDurationSeconds :: !(Maybe Nat)
  , _craRoleAlias                 :: !Text
  , _craRoleARN                   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateRoleAlias' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'craCredentialDurationSeconds' - How long (in seconds) the credentials will be valid.
--
-- * 'craRoleAlias' - The role alias that points to a role ARN. This allows you to change the role without having to update the device.
--
-- * 'craRoleARN' - The role ARN.
createRoleAlias
    :: Text -- ^ 'craRoleAlias'
    -> Text -- ^ 'craRoleARN'
    -> CreateRoleAlias
createRoleAlias pRoleAlias_ pRoleARN_ =
  CreateRoleAlias'
    { _craCredentialDurationSeconds = Nothing
    , _craRoleAlias = pRoleAlias_
    , _craRoleARN = pRoleARN_
    }


-- | How long (in seconds) the credentials will be valid.
craCredentialDurationSeconds :: Lens' CreateRoleAlias (Maybe Natural)
craCredentialDurationSeconds = lens _craCredentialDurationSeconds (\ s a -> s{_craCredentialDurationSeconds = a}) . mapping _Nat

-- | The role alias that points to a role ARN. This allows you to change the role without having to update the device.
craRoleAlias :: Lens' CreateRoleAlias Text
craRoleAlias = lens _craRoleAlias (\ s a -> s{_craRoleAlias = a})

-- | The role ARN.
craRoleARN :: Lens' CreateRoleAlias Text
craRoleARN = lens _craRoleARN (\ s a -> s{_craRoleARN = a})

instance AWSRequest CreateRoleAlias where
        type Rs CreateRoleAlias = CreateRoleAliasResponse
        request = postJSON ioT
        response
          = receiveJSON
              (\ s h x ->
                 CreateRoleAliasResponse' <$>
                   (x .?> "roleAliasArn") <*> (x .?> "roleAlias") <*>
                     (pure (fromEnum s)))

instance Hashable CreateRoleAlias where

instance NFData CreateRoleAlias where

instance ToHeaders CreateRoleAlias where
        toHeaders = const mempty

instance ToJSON CreateRoleAlias where
        toJSON CreateRoleAlias'{..}
          = object
              (catMaybes
                 [("credentialDurationSeconds" .=) <$>
                    _craCredentialDurationSeconds,
                  Just ("roleArn" .= _craRoleARN)])

instance ToPath CreateRoleAlias where
        toPath CreateRoleAlias'{..}
          = mconcat ["/role-aliases/", toBS _craRoleAlias]

instance ToQuery CreateRoleAlias where
        toQuery = const mempty

-- | /See:/ 'createRoleAliasResponse' smart constructor.
data CreateRoleAliasResponse = CreateRoleAliasResponse'
  { _crarsRoleAliasARN   :: !(Maybe Text)
  , _crarsRoleAlias      :: !(Maybe Text)
  , _crarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateRoleAliasResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crarsRoleAliasARN' - The role alias ARN.
--
-- * 'crarsRoleAlias' - The role alias.
--
-- * 'crarsResponseStatus' - -- | The response status code.
createRoleAliasResponse
    :: Int -- ^ 'crarsResponseStatus'
    -> CreateRoleAliasResponse
createRoleAliasResponse pResponseStatus_ =
  CreateRoleAliasResponse'
    { _crarsRoleAliasARN = Nothing
    , _crarsRoleAlias = Nothing
    , _crarsResponseStatus = pResponseStatus_
    }


-- | The role alias ARN.
crarsRoleAliasARN :: Lens' CreateRoleAliasResponse (Maybe Text)
crarsRoleAliasARN = lens _crarsRoleAliasARN (\ s a -> s{_crarsRoleAliasARN = a})

-- | The role alias.
crarsRoleAlias :: Lens' CreateRoleAliasResponse (Maybe Text)
crarsRoleAlias = lens _crarsRoleAlias (\ s a -> s{_crarsRoleAlias = a})

-- | -- | The response status code.
crarsResponseStatus :: Lens' CreateRoleAliasResponse Int
crarsResponseStatus = lens _crarsResponseStatus (\ s a -> s{_crarsResponseStatus = a})

instance NFData CreateRoleAliasResponse where
