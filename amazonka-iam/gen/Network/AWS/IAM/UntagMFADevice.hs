{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UntagMFADevice
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified tags from the IAM virtual multi-factor
-- authentication (MFA) device. For more information about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
module Network.AWS.IAM.UntagMFADevice
  ( -- * Creating a Request
    UntagMFADevice (..),
    newUntagMFADevice,

    -- * Request Lenses
    untagMFADevice_serialNumber,
    untagMFADevice_tagKeys,

    -- * Destructuring the Response
    UntagMFADeviceResponse (..),
    newUntagMFADeviceResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUntagMFADevice' smart constructor.
data UntagMFADevice = UntagMFADevice'
  { -- | The unique identifier for the IAM virtual MFA device from which you want
    -- to remove tags. For virtual MFA devices, the serial number is the same
    -- as the ARN.
    --
    -- This parameter accepts (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- that consist of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: =,.\@-
    serialNumber :: Core.Text,
    -- | A list of key names as a simple array of strings. The tags with matching
    -- keys are removed from the specified instance profile.
    tagKeys :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UntagMFADevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serialNumber', 'untagMFADevice_serialNumber' - The unique identifier for the IAM virtual MFA device from which you want
-- to remove tags. For virtual MFA devices, the serial number is the same
-- as the ARN.
--
-- This parameter accepts (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that consist of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: =,.\@-
--
-- 'tagKeys', 'untagMFADevice_tagKeys' - A list of key names as a simple array of strings. The tags with matching
-- keys are removed from the specified instance profile.
newUntagMFADevice ::
  -- | 'serialNumber'
  Core.Text ->
  UntagMFADevice
newUntagMFADevice pSerialNumber_ =
  UntagMFADevice'
    { serialNumber = pSerialNumber_,
      tagKeys = Core.mempty
    }

-- | The unique identifier for the IAM virtual MFA device from which you want
-- to remove tags. For virtual MFA devices, the serial number is the same
-- as the ARN.
--
-- This parameter accepts (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that consist of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: =,.\@-
untagMFADevice_serialNumber :: Lens.Lens' UntagMFADevice Core.Text
untagMFADevice_serialNumber = Lens.lens (\UntagMFADevice' {serialNumber} -> serialNumber) (\s@UntagMFADevice' {} a -> s {serialNumber = a} :: UntagMFADevice)

-- | A list of key names as a simple array of strings. The tags with matching
-- keys are removed from the specified instance profile.
untagMFADevice_tagKeys :: Lens.Lens' UntagMFADevice [Core.Text]
untagMFADevice_tagKeys = Lens.lens (\UntagMFADevice' {tagKeys} -> tagKeys) (\s@UntagMFADevice' {} a -> s {tagKeys = a} :: UntagMFADevice) Core.. Lens._Coerce

instance Core.AWSRequest UntagMFADevice where
  type
    AWSResponse UntagMFADevice =
      UntagMFADeviceResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull UntagMFADeviceResponse'

instance Core.Hashable UntagMFADevice

instance Core.NFData UntagMFADevice

instance Core.ToHeaders UntagMFADevice where
  toHeaders = Core.const Core.mempty

instance Core.ToPath UntagMFADevice where
  toPath = Core.const "/"

instance Core.ToQuery UntagMFADevice where
  toQuery UntagMFADevice' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("UntagMFADevice" :: Core.ByteString),
        "Version" Core.=: ("2010-05-08" :: Core.ByteString),
        "SerialNumber" Core.=: serialNumber,
        "TagKeys" Core.=: Core.toQueryList "member" tagKeys
      ]

-- | /See:/ 'newUntagMFADeviceResponse' smart constructor.
data UntagMFADeviceResponse = UntagMFADeviceResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UntagMFADeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUntagMFADeviceResponse ::
  UntagMFADeviceResponse
newUntagMFADeviceResponse = UntagMFADeviceResponse'

instance Core.NFData UntagMFADeviceResponse
