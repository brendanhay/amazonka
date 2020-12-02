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
-- Module      : Network.AWS.IoT.DetachSecurityProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a Device Defender security profile from a thing group or from this account.
module Network.AWS.IoT.DetachSecurityProfile
  ( -- * Creating a Request
    detachSecurityProfile,
    DetachSecurityProfile,

    -- * Request Lenses
    detSecurityProfileName,
    detSecurityProfileTargetARN,

    -- * Destructuring the Response
    detachSecurityProfileResponse,
    DetachSecurityProfileResponse,

    -- * Response Lenses
    detrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'detachSecurityProfile' smart constructor.
data DetachSecurityProfile = DetachSecurityProfile'
  { _detSecurityProfileName ::
      !Text,
    _detSecurityProfileTargetARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DetachSecurityProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'detSecurityProfileName' - The security profile that is detached.
--
-- * 'detSecurityProfileTargetARN' - The ARN of the thing group from which the security profile is detached.
detachSecurityProfile ::
  -- | 'detSecurityProfileName'
  Text ->
  -- | 'detSecurityProfileTargetARN'
  Text ->
  DetachSecurityProfile
detachSecurityProfile
  pSecurityProfileName_
  pSecurityProfileTargetARN_ =
    DetachSecurityProfile'
      { _detSecurityProfileName =
          pSecurityProfileName_,
        _detSecurityProfileTargetARN = pSecurityProfileTargetARN_
      }

-- | The security profile that is detached.
detSecurityProfileName :: Lens' DetachSecurityProfile Text
detSecurityProfileName = lens _detSecurityProfileName (\s a -> s {_detSecurityProfileName = a})

-- | The ARN of the thing group from which the security profile is detached.
detSecurityProfileTargetARN :: Lens' DetachSecurityProfile Text
detSecurityProfileTargetARN = lens _detSecurityProfileTargetARN (\s a -> s {_detSecurityProfileTargetARN = a})

instance AWSRequest DetachSecurityProfile where
  type Rs DetachSecurityProfile = DetachSecurityProfileResponse
  request = delete ioT
  response =
    receiveEmpty
      (\s h x -> DetachSecurityProfileResponse' <$> (pure (fromEnum s)))

instance Hashable DetachSecurityProfile

instance NFData DetachSecurityProfile

instance ToHeaders DetachSecurityProfile where
  toHeaders = const mempty

instance ToPath DetachSecurityProfile where
  toPath DetachSecurityProfile' {..} =
    mconcat
      ["/security-profiles/", toBS _detSecurityProfileName, "/targets"]

instance ToQuery DetachSecurityProfile where
  toQuery DetachSecurityProfile' {..} =
    mconcat
      ["securityProfileTargetArn" =: _detSecurityProfileTargetARN]

-- | /See:/ 'detachSecurityProfileResponse' smart constructor.
newtype DetachSecurityProfileResponse = DetachSecurityProfileResponse'
  { _detrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DetachSecurityProfileResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'detrsResponseStatus' - -- | The response status code.
detachSecurityProfileResponse ::
  -- | 'detrsResponseStatus'
  Int ->
  DetachSecurityProfileResponse
detachSecurityProfileResponse pResponseStatus_ =
  DetachSecurityProfileResponse'
    { _detrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
detrsResponseStatus :: Lens' DetachSecurityProfileResponse Int
detrsResponseStatus = lens _detrsResponseStatus (\s a -> s {_detrsResponseStatus = a})

instance NFData DetachSecurityProfileResponse
