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
-- Module      : Network.AWS.IoT.AttachSecurityProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a Device Defender security profile with a thing group or this account. Each thing group or account can have up to five security profiles associated with it.
module Network.AWS.IoT.AttachSecurityProfile
  ( -- * Creating a Request
    attachSecurityProfile,
    AttachSecurityProfile,

    -- * Request Lenses
    aspSecurityProfileName,
    aspSecurityProfileTargetARN,

    -- * Destructuring the Response
    attachSecurityProfileResponse,
    AttachSecurityProfileResponse,

    -- * Response Lenses
    asprsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'attachSecurityProfile' smart constructor.
data AttachSecurityProfile = AttachSecurityProfile'
  { _aspSecurityProfileName ::
      !Text,
    _aspSecurityProfileTargetARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AttachSecurityProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aspSecurityProfileName' - The security profile that is attached.
--
-- * 'aspSecurityProfileTargetARN' - The ARN of the target (thing group) to which the security profile is attached.
attachSecurityProfile ::
  -- | 'aspSecurityProfileName'
  Text ->
  -- | 'aspSecurityProfileTargetARN'
  Text ->
  AttachSecurityProfile
attachSecurityProfile
  pSecurityProfileName_
  pSecurityProfileTargetARN_ =
    AttachSecurityProfile'
      { _aspSecurityProfileName =
          pSecurityProfileName_,
        _aspSecurityProfileTargetARN = pSecurityProfileTargetARN_
      }

-- | The security profile that is attached.
aspSecurityProfileName :: Lens' AttachSecurityProfile Text
aspSecurityProfileName = lens _aspSecurityProfileName (\s a -> s {_aspSecurityProfileName = a})

-- | The ARN of the target (thing group) to which the security profile is attached.
aspSecurityProfileTargetARN :: Lens' AttachSecurityProfile Text
aspSecurityProfileTargetARN = lens _aspSecurityProfileTargetARN (\s a -> s {_aspSecurityProfileTargetARN = a})

instance AWSRequest AttachSecurityProfile where
  type Rs AttachSecurityProfile = AttachSecurityProfileResponse
  request = putJSON ioT
  response =
    receiveEmpty
      (\s h x -> AttachSecurityProfileResponse' <$> (pure (fromEnum s)))

instance Hashable AttachSecurityProfile

instance NFData AttachSecurityProfile

instance ToHeaders AttachSecurityProfile where
  toHeaders = const mempty

instance ToJSON AttachSecurityProfile where
  toJSON = const (Object mempty)

instance ToPath AttachSecurityProfile where
  toPath AttachSecurityProfile' {..} =
    mconcat
      ["/security-profiles/", toBS _aspSecurityProfileName, "/targets"]

instance ToQuery AttachSecurityProfile where
  toQuery AttachSecurityProfile' {..} =
    mconcat
      ["securityProfileTargetArn" =: _aspSecurityProfileTargetARN]

-- | /See:/ 'attachSecurityProfileResponse' smart constructor.
newtype AttachSecurityProfileResponse = AttachSecurityProfileResponse'
  { _asprsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AttachSecurityProfileResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asprsResponseStatus' - -- | The response status code.
attachSecurityProfileResponse ::
  -- | 'asprsResponseStatus'
  Int ->
  AttachSecurityProfileResponse
attachSecurityProfileResponse pResponseStatus_ =
  AttachSecurityProfileResponse'
    { _asprsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
asprsResponseStatus :: Lens' AttachSecurityProfileResponse Int
asprsResponseStatus = lens _asprsResponseStatus (\s a -> s {_asprsResponseStatus = a})

instance NFData AttachSecurityProfileResponse
