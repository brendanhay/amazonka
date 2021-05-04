{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    serialNumber :: Prelude.Text,
    -- | A list of key names as a simple array of strings. The tags with matching
    -- keys are removed from the specified instance profile.
    tagKeys :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  UntagMFADevice
newUntagMFADevice pSerialNumber_ =
  UntagMFADevice'
    { serialNumber = pSerialNumber_,
      tagKeys = Prelude.mempty
    }

-- | The unique identifier for the IAM virtual MFA device from which you want
-- to remove tags. For virtual MFA devices, the serial number is the same
-- as the ARN.
--
-- This parameter accepts (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that consist of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: =,.\@-
untagMFADevice_serialNumber :: Lens.Lens' UntagMFADevice Prelude.Text
untagMFADevice_serialNumber = Lens.lens (\UntagMFADevice' {serialNumber} -> serialNumber) (\s@UntagMFADevice' {} a -> s {serialNumber = a} :: UntagMFADevice)

-- | A list of key names as a simple array of strings. The tags with matching
-- keys are removed from the specified instance profile.
untagMFADevice_tagKeys :: Lens.Lens' UntagMFADevice [Prelude.Text]
untagMFADevice_tagKeys = Lens.lens (\UntagMFADevice' {tagKeys} -> tagKeys) (\s@UntagMFADevice' {} a -> s {tagKeys = a} :: UntagMFADevice) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest UntagMFADevice where
  type Rs UntagMFADevice = UntagMFADeviceResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull UntagMFADeviceResponse'

instance Prelude.Hashable UntagMFADevice

instance Prelude.NFData UntagMFADevice

instance Prelude.ToHeaders UntagMFADevice where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath UntagMFADevice where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UntagMFADevice where
  toQuery UntagMFADevice' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("UntagMFADevice" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "SerialNumber" Prelude.=: serialNumber,
        "TagKeys"
          Prelude.=: Prelude.toQueryList "member" tagKeys
      ]

-- | /See:/ 'newUntagMFADeviceResponse' smart constructor.
data UntagMFADeviceResponse = UntagMFADeviceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UntagMFADeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUntagMFADeviceResponse ::
  UntagMFADeviceResponse
newUntagMFADeviceResponse = UntagMFADeviceResponse'

instance Prelude.NFData UntagMFADeviceResponse
