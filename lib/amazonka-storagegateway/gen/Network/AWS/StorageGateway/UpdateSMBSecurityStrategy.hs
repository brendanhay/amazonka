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
-- Module      : Network.AWS.StorageGateway.UpdateSMBSecurityStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the SMB security strategy on a file gateway. This action is only supported in file gateways.
module Network.AWS.StorageGateway.UpdateSMBSecurityStrategy
  ( -- * Creating a Request
    updateSMBSecurityStrategy,
    UpdateSMBSecurityStrategy,

    -- * Request Lenses
    usmbssGatewayARN,
    usmbssSMBSecurityStrategy,

    -- * Destructuring the Response
    updateSMBSecurityStrategyResponse,
    UpdateSMBSecurityStrategyResponse,

    -- * Response Lenses
    usmbssrsGatewayARN,
    usmbssrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'updateSMBSecurityStrategy' smart constructor.
data UpdateSMBSecurityStrategy = UpdateSMBSecurityStrategy'
  { _usmbssGatewayARN ::
      !Text,
    _usmbssSMBSecurityStrategy ::
      !SMBSecurityStrategy
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateSMBSecurityStrategy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usmbssGatewayARN' - Undocumented member.
--
-- * 'usmbssSMBSecurityStrategy' - Specifies the type of security strategy. ClientSpecified: if you use this option, requests are established based on what is negotiated by the client. This option is recommended when you want to maximize compatibility across different clients in your environment. MandatorySigning: if you use this option, file gateway only allows connections from SMBv2 or SMBv3 clients that have signing enabled. This option works with SMB clients on Microsoft Windows Vista, Windows Server 2008 or newer. MandatoryEncryption: if you use this option, file gateway only allows connections from SMBv3 clients that have encryption enabled. This option is highly recommended for environments that handle sensitive data. This option works with SMB clients on Microsoft Windows 8, Windows Server 2012 or newer.
updateSMBSecurityStrategy ::
  -- | 'usmbssGatewayARN'
  Text ->
  -- | 'usmbssSMBSecurityStrategy'
  SMBSecurityStrategy ->
  UpdateSMBSecurityStrategy
updateSMBSecurityStrategy pGatewayARN_ pSMBSecurityStrategy_ =
  UpdateSMBSecurityStrategy'
    { _usmbssGatewayARN = pGatewayARN_,
      _usmbssSMBSecurityStrategy = pSMBSecurityStrategy_
    }

-- | Undocumented member.
usmbssGatewayARN :: Lens' UpdateSMBSecurityStrategy Text
usmbssGatewayARN = lens _usmbssGatewayARN (\s a -> s {_usmbssGatewayARN = a})

-- | Specifies the type of security strategy. ClientSpecified: if you use this option, requests are established based on what is negotiated by the client. This option is recommended when you want to maximize compatibility across different clients in your environment. MandatorySigning: if you use this option, file gateway only allows connections from SMBv2 or SMBv3 clients that have signing enabled. This option works with SMB clients on Microsoft Windows Vista, Windows Server 2008 or newer. MandatoryEncryption: if you use this option, file gateway only allows connections from SMBv3 clients that have encryption enabled. This option is highly recommended for environments that handle sensitive data. This option works with SMB clients on Microsoft Windows 8, Windows Server 2012 or newer.
usmbssSMBSecurityStrategy :: Lens' UpdateSMBSecurityStrategy SMBSecurityStrategy
usmbssSMBSecurityStrategy = lens _usmbssSMBSecurityStrategy (\s a -> s {_usmbssSMBSecurityStrategy = a})

instance AWSRequest UpdateSMBSecurityStrategy where
  type
    Rs UpdateSMBSecurityStrategy =
      UpdateSMBSecurityStrategyResponse
  request = postJSON storageGateway
  response =
    receiveJSON
      ( \s h x ->
          UpdateSMBSecurityStrategyResponse'
            <$> (x .?> "GatewayARN") <*> (pure (fromEnum s))
      )

instance Hashable UpdateSMBSecurityStrategy

instance NFData UpdateSMBSecurityStrategy

instance ToHeaders UpdateSMBSecurityStrategy where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "StorageGateway_20130630.UpdateSMBSecurityStrategy" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateSMBSecurityStrategy where
  toJSON UpdateSMBSecurityStrategy' {..} =
    object
      ( catMaybes
          [ Just ("GatewayARN" .= _usmbssGatewayARN),
            Just ("SMBSecurityStrategy" .= _usmbssSMBSecurityStrategy)
          ]
      )

instance ToPath UpdateSMBSecurityStrategy where
  toPath = const "/"

instance ToQuery UpdateSMBSecurityStrategy where
  toQuery = const mempty

-- | /See:/ 'updateSMBSecurityStrategyResponse' smart constructor.
data UpdateSMBSecurityStrategyResponse = UpdateSMBSecurityStrategyResponse'
  { _usmbssrsGatewayARN ::
      !(Maybe Text),
    _usmbssrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateSMBSecurityStrategyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usmbssrsGatewayARN' - Undocumented member.
--
-- * 'usmbssrsResponseStatus' - -- | The response status code.
updateSMBSecurityStrategyResponse ::
  -- | 'usmbssrsResponseStatus'
  Int ->
  UpdateSMBSecurityStrategyResponse
updateSMBSecurityStrategyResponse pResponseStatus_ =
  UpdateSMBSecurityStrategyResponse'
    { _usmbssrsGatewayARN = Nothing,
      _usmbssrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
usmbssrsGatewayARN :: Lens' UpdateSMBSecurityStrategyResponse (Maybe Text)
usmbssrsGatewayARN = lens _usmbssrsGatewayARN (\s a -> s {_usmbssrsGatewayARN = a})

-- | -- | The response status code.
usmbssrsResponseStatus :: Lens' UpdateSMBSecurityStrategyResponse Int
usmbssrsResponseStatus = lens _usmbssrsResponseStatus (\s a -> s {_usmbssrsResponseStatus = a})

instance NFData UpdateSMBSecurityStrategyResponse
