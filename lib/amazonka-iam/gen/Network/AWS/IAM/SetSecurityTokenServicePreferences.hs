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
-- Module      : Network.AWS.IAM.SetSecurityTokenServicePreferences
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the specified version of the global endpoint token as the token version used for the AWS account.
--
--
-- By default, AWS Security Token Service (STS) is available as a global service, and all STS requests go to a single endpoint at @https://sts.amazonaws.com@ . AWS recommends using Regional STS endpoints to reduce latency, build in redundancy, and increase session token availability. For information about Regional endpoints for STS, see <https://docs.aws.amazon.com/general/latest/gr/rande.html#sts_region AWS Regions and Endpoints> in the /AWS General Reference/ .
--
-- If you make an STS call to the global endpoint, the resulting session tokens might be valid in some Regions but not others. It depends on the version that is set in this operation. Version 1 tokens are valid only in AWS Regions that are available by default. These tokens do not work in manually enabled Regions, such as Asia Pacific (Hong Kong). Version 2 tokens are valid in all Regions. However, version 2 tokens are longer and might affect systems where you temporarily store tokens. For information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and Deactivating STS in an AWS Region> in the /IAM User Guide/ .
--
-- To view the current session token version, see the @GlobalEndpointTokenVersion@ entry in the response of the 'GetAccountSummary' operation.
module Network.AWS.IAM.SetSecurityTokenServicePreferences
  ( -- * Creating a Request
    setSecurityTokenServicePreferences,
    SetSecurityTokenServicePreferences,

    -- * Request Lenses
    sstspGlobalEndpointTokenVersion,

    -- * Destructuring the Response
    setSecurityTokenServicePreferencesResponse,
    SetSecurityTokenServicePreferencesResponse,
  )
where

import Network.AWS.IAM.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'setSecurityTokenServicePreferences' smart constructor.
newtype SetSecurityTokenServicePreferences = SetSecurityTokenServicePreferences'
  { _sstspGlobalEndpointTokenVersion ::
      GlobalEndpointTokenVersion
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SetSecurityTokenServicePreferences' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sstspGlobalEndpointTokenVersion' - The version of the global endpoint token. Version 1 tokens are valid only in AWS Regions that are available by default. These tokens do not work in manually enabled Regions, such as Asia Pacific (Hong Kong). Version 2 tokens are valid in all Regions. However, version 2 tokens are longer and might affect systems where you temporarily store tokens. For information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and Deactivating STS in an AWS Region> in the /IAM User Guide/ .
setSecurityTokenServicePreferences ::
  -- | 'sstspGlobalEndpointTokenVersion'
  GlobalEndpointTokenVersion ->
  SetSecurityTokenServicePreferences
setSecurityTokenServicePreferences pGlobalEndpointTokenVersion_ =
  SetSecurityTokenServicePreferences'
    { _sstspGlobalEndpointTokenVersion =
        pGlobalEndpointTokenVersion_
    }

-- | The version of the global endpoint token. Version 1 tokens are valid only in AWS Regions that are available by default. These tokens do not work in manually enabled Regions, such as Asia Pacific (Hong Kong). Version 2 tokens are valid in all Regions. However, version 2 tokens are longer and might affect systems where you temporarily store tokens. For information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and Deactivating STS in an AWS Region> in the /IAM User Guide/ .
sstspGlobalEndpointTokenVersion :: Lens' SetSecurityTokenServicePreferences GlobalEndpointTokenVersion
sstspGlobalEndpointTokenVersion = lens _sstspGlobalEndpointTokenVersion (\s a -> s {_sstspGlobalEndpointTokenVersion = a})

instance AWSRequest SetSecurityTokenServicePreferences where
  type
    Rs SetSecurityTokenServicePreferences =
      SetSecurityTokenServicePreferencesResponse
  request = postQuery iam
  response = receiveNull SetSecurityTokenServicePreferencesResponse'

instance Hashable SetSecurityTokenServicePreferences

instance NFData SetSecurityTokenServicePreferences

instance ToHeaders SetSecurityTokenServicePreferences where
  toHeaders = const mempty

instance ToPath SetSecurityTokenServicePreferences where
  toPath = const "/"

instance ToQuery SetSecurityTokenServicePreferences where
  toQuery SetSecurityTokenServicePreferences' {..} =
    mconcat
      [ "Action" =: ("SetSecurityTokenServicePreferences" :: ByteString),
        "Version" =: ("2010-05-08" :: ByteString),
        "GlobalEndpointTokenVersion" =: _sstspGlobalEndpointTokenVersion
      ]

-- | /See:/ 'setSecurityTokenServicePreferencesResponse' smart constructor.
data SetSecurityTokenServicePreferencesResponse = SetSecurityTokenServicePreferencesResponse'
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'SetSecurityTokenServicePreferencesResponse' with the minimum fields required to make a request.
setSecurityTokenServicePreferencesResponse ::
  SetSecurityTokenServicePreferencesResponse
setSecurityTokenServicePreferencesResponse =
  SetSecurityTokenServicePreferencesResponse'

instance NFData SetSecurityTokenServicePreferencesResponse
