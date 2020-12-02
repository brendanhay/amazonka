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
-- Module      : Network.AWS.StorageGateway.UpdateAutomaticTapeCreationPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the automatic tape creation policy of a gateway. Use this to update the policy with a new set of automatic tape creation rules. This is only supported for tape gateways.
--
--
-- By default, there is no automatic tape creation policy.
module Network.AWS.StorageGateway.UpdateAutomaticTapeCreationPolicy
  ( -- * Creating a Request
    updateAutomaticTapeCreationPolicy,
    UpdateAutomaticTapeCreationPolicy,

    -- * Request Lenses
    uatcpAutomaticTapeCreationRules,
    uatcpGatewayARN,

    -- * Destructuring the Response
    updateAutomaticTapeCreationPolicyResponse,
    UpdateAutomaticTapeCreationPolicyResponse,

    -- * Response Lenses
    uatcprsGatewayARN,
    uatcprsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'updateAutomaticTapeCreationPolicy' smart constructor.
data UpdateAutomaticTapeCreationPolicy = UpdateAutomaticTapeCreationPolicy'
  { _uatcpAutomaticTapeCreationRules ::
      !( List1
           AutomaticTapeCreationRule
       ),
    _uatcpGatewayARN ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateAutomaticTapeCreationPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uatcpAutomaticTapeCreationRules' - An automatic tape creation policy consists of a list of automatic tape creation rules. The rules determine when and how to automatically create new tapes.
--
-- * 'uatcpGatewayARN' - Undocumented member.
updateAutomaticTapeCreationPolicy ::
  -- | 'uatcpAutomaticTapeCreationRules'
  NonEmpty AutomaticTapeCreationRule ->
  -- | 'uatcpGatewayARN'
  Text ->
  UpdateAutomaticTapeCreationPolicy
updateAutomaticTapeCreationPolicy
  pAutomaticTapeCreationRules_
  pGatewayARN_ =
    UpdateAutomaticTapeCreationPolicy'
      { _uatcpAutomaticTapeCreationRules =
          _List1 # pAutomaticTapeCreationRules_,
        _uatcpGatewayARN = pGatewayARN_
      }

-- | An automatic tape creation policy consists of a list of automatic tape creation rules. The rules determine when and how to automatically create new tapes.
uatcpAutomaticTapeCreationRules :: Lens' UpdateAutomaticTapeCreationPolicy (NonEmpty AutomaticTapeCreationRule)
uatcpAutomaticTapeCreationRules = lens _uatcpAutomaticTapeCreationRules (\s a -> s {_uatcpAutomaticTapeCreationRules = a}) . _List1

-- | Undocumented member.
uatcpGatewayARN :: Lens' UpdateAutomaticTapeCreationPolicy Text
uatcpGatewayARN = lens _uatcpGatewayARN (\s a -> s {_uatcpGatewayARN = a})

instance AWSRequest UpdateAutomaticTapeCreationPolicy where
  type
    Rs UpdateAutomaticTapeCreationPolicy =
      UpdateAutomaticTapeCreationPolicyResponse
  request = postJSON storageGateway
  response =
    receiveJSON
      ( \s h x ->
          UpdateAutomaticTapeCreationPolicyResponse'
            <$> (x .?> "GatewayARN") <*> (pure (fromEnum s))
      )

instance Hashable UpdateAutomaticTapeCreationPolicy

instance NFData UpdateAutomaticTapeCreationPolicy

instance ToHeaders UpdateAutomaticTapeCreationPolicy where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "StorageGateway_20130630.UpdateAutomaticTapeCreationPolicy" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateAutomaticTapeCreationPolicy where
  toJSON UpdateAutomaticTapeCreationPolicy' {..} =
    object
      ( catMaybes
          [ Just
              ("AutomaticTapeCreationRules" .= _uatcpAutomaticTapeCreationRules),
            Just ("GatewayARN" .= _uatcpGatewayARN)
          ]
      )

instance ToPath UpdateAutomaticTapeCreationPolicy where
  toPath = const "/"

instance ToQuery UpdateAutomaticTapeCreationPolicy where
  toQuery = const mempty

-- | /See:/ 'updateAutomaticTapeCreationPolicyResponse' smart constructor.
data UpdateAutomaticTapeCreationPolicyResponse = UpdateAutomaticTapeCreationPolicyResponse'
  { _uatcprsGatewayARN ::
      !( Maybe
           Text
       ),
    _uatcprsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'UpdateAutomaticTapeCreationPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uatcprsGatewayARN' - Undocumented member.
--
-- * 'uatcprsResponseStatus' - -- | The response status code.
updateAutomaticTapeCreationPolicyResponse ::
  -- | 'uatcprsResponseStatus'
  Int ->
  UpdateAutomaticTapeCreationPolicyResponse
updateAutomaticTapeCreationPolicyResponse pResponseStatus_ =
  UpdateAutomaticTapeCreationPolicyResponse'
    { _uatcprsGatewayARN =
        Nothing,
      _uatcprsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
uatcprsGatewayARN :: Lens' UpdateAutomaticTapeCreationPolicyResponse (Maybe Text)
uatcprsGatewayARN = lens _uatcprsGatewayARN (\s a -> s {_uatcprsGatewayARN = a})

-- | -- | The response status code.
uatcprsResponseStatus :: Lens' UpdateAutomaticTapeCreationPolicyResponse Int
uatcprsResponseStatus = lens _uatcprsResponseStatus (\s a -> s {_uatcprsResponseStatus = a})

instance NFData UpdateAutomaticTapeCreationPolicyResponse
