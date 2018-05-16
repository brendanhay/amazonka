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
-- Module      : Network.AWS.IoT.UpdateRoleAlias
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a role alias.
--
--
module Network.AWS.IoT.UpdateRoleAlias
    (
    -- * Creating a Request
      updateRoleAlias
    , UpdateRoleAlias
    -- * Request Lenses
    , uraCredentialDurationSeconds
    , uraRoleARN
    , uraRoleAlias

    -- * Destructuring the Response
    , updateRoleAliasResponse
    , UpdateRoleAliasResponse
    -- * Response Lenses
    , urarsRoleAliasARN
    , urarsRoleAlias
    , urarsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateRoleAlias' smart constructor.
data UpdateRoleAlias = UpdateRoleAlias'
  { _uraCredentialDurationSeconds :: !(Maybe Nat)
  , _uraRoleARN                   :: !(Maybe Text)
  , _uraRoleAlias                 :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateRoleAlias' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uraCredentialDurationSeconds' - The number of seconds the credential will be valid.
--
-- * 'uraRoleARN' - The role ARN.
--
-- * 'uraRoleAlias' - The role alias to update.
updateRoleAlias
    :: Text -- ^ 'uraRoleAlias'
    -> UpdateRoleAlias
updateRoleAlias pRoleAlias_ =
  UpdateRoleAlias'
    { _uraCredentialDurationSeconds = Nothing
    , _uraRoleARN = Nothing
    , _uraRoleAlias = pRoleAlias_
    }


-- | The number of seconds the credential will be valid.
uraCredentialDurationSeconds :: Lens' UpdateRoleAlias (Maybe Natural)
uraCredentialDurationSeconds = lens _uraCredentialDurationSeconds (\ s a -> s{_uraCredentialDurationSeconds = a}) . mapping _Nat

-- | The role ARN.
uraRoleARN :: Lens' UpdateRoleAlias (Maybe Text)
uraRoleARN = lens _uraRoleARN (\ s a -> s{_uraRoleARN = a})

-- | The role alias to update.
uraRoleAlias :: Lens' UpdateRoleAlias Text
uraRoleAlias = lens _uraRoleAlias (\ s a -> s{_uraRoleAlias = a})

instance AWSRequest UpdateRoleAlias where
        type Rs UpdateRoleAlias = UpdateRoleAliasResponse
        request = putJSON ioT
        response
          = receiveJSON
              (\ s h x ->
                 UpdateRoleAliasResponse' <$>
                   (x .?> "roleAliasArn") <*> (x .?> "roleAlias") <*>
                     (pure (fromEnum s)))

instance Hashable UpdateRoleAlias where

instance NFData UpdateRoleAlias where

instance ToHeaders UpdateRoleAlias where
        toHeaders = const mempty

instance ToJSON UpdateRoleAlias where
        toJSON UpdateRoleAlias'{..}
          = object
              (catMaybes
                 [("credentialDurationSeconds" .=) <$>
                    _uraCredentialDurationSeconds,
                  ("roleArn" .=) <$> _uraRoleARN])

instance ToPath UpdateRoleAlias where
        toPath UpdateRoleAlias'{..}
          = mconcat ["/role-aliases/", toBS _uraRoleAlias]

instance ToQuery UpdateRoleAlias where
        toQuery = const mempty

-- | /See:/ 'updateRoleAliasResponse' smart constructor.
data UpdateRoleAliasResponse = UpdateRoleAliasResponse'
  { _urarsRoleAliasARN   :: !(Maybe Text)
  , _urarsRoleAlias      :: !(Maybe Text)
  , _urarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateRoleAliasResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urarsRoleAliasARN' - The role alias ARN.
--
-- * 'urarsRoleAlias' - The role alias.
--
-- * 'urarsResponseStatus' - -- | The response status code.
updateRoleAliasResponse
    :: Int -- ^ 'urarsResponseStatus'
    -> UpdateRoleAliasResponse
updateRoleAliasResponse pResponseStatus_ =
  UpdateRoleAliasResponse'
    { _urarsRoleAliasARN = Nothing
    , _urarsRoleAlias = Nothing
    , _urarsResponseStatus = pResponseStatus_
    }


-- | The role alias ARN.
urarsRoleAliasARN :: Lens' UpdateRoleAliasResponse (Maybe Text)
urarsRoleAliasARN = lens _urarsRoleAliasARN (\ s a -> s{_urarsRoleAliasARN = a})

-- | The role alias.
urarsRoleAlias :: Lens' UpdateRoleAliasResponse (Maybe Text)
urarsRoleAlias = lens _urarsRoleAlias (\ s a -> s{_urarsRoleAlias = a})

-- | -- | The response status code.
urarsResponseStatus :: Lens' UpdateRoleAliasResponse Int
urarsResponseStatus = lens _urarsResponseStatus (\ s a -> s{_urarsResponseStatus = a})

instance NFData UpdateRoleAliasResponse where
