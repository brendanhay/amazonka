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
-- Module      : Amazonka.IAM.UntagMFADevice
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified tags from the IAM virtual multi-factor
-- authentication (MFA) device. For more information about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
module Amazonka.IAM.UntagMFADevice
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUntagMFADevice' smart constructor.
data UntagMFADevice = UntagMFADevice'
  { -- | The unique identifier for the IAM virtual MFA device from which you want
    -- to remove tags. For virtual MFA devices, the serial number is the same
    -- as the ARN.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    serialNumber :: Prelude.Text,
    -- | A list of key names as a simple array of strings. The tags with matching
    -- keys are removed from the specified instance profile.
    tagKeys :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
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
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
untagMFADevice_serialNumber :: Lens.Lens' UntagMFADevice Prelude.Text
untagMFADevice_serialNumber = Lens.lens (\UntagMFADevice' {serialNumber} -> serialNumber) (\s@UntagMFADevice' {} a -> s {serialNumber = a} :: UntagMFADevice)

-- | A list of key names as a simple array of strings. The tags with matching
-- keys are removed from the specified instance profile.
untagMFADevice_tagKeys :: Lens.Lens' UntagMFADevice [Prelude.Text]
untagMFADevice_tagKeys = Lens.lens (\UntagMFADevice' {tagKeys} -> tagKeys) (\s@UntagMFADevice' {} a -> s {tagKeys = a} :: UntagMFADevice) Prelude.. Lens.coerced

instance Core.AWSRequest UntagMFADevice where
  type
    AWSResponse UntagMFADevice =
      UntagMFADeviceResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull UntagMFADeviceResponse'

instance Prelude.Hashable UntagMFADevice where
  hashWithSalt _salt UntagMFADevice' {..} =
    _salt
      `Prelude.hashWithSalt` serialNumber
      `Prelude.hashWithSalt` tagKeys

instance Prelude.NFData UntagMFADevice where
  rnf UntagMFADevice' {..} =
    Prelude.rnf serialNumber
      `Prelude.seq` Prelude.rnf tagKeys

instance Data.ToHeaders UntagMFADevice where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath UntagMFADevice where
  toPath = Prelude.const "/"

instance Data.ToQuery UntagMFADevice where
  toQuery UntagMFADevice' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("UntagMFADevice" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "SerialNumber" Data.=: serialNumber,
        "TagKeys" Data.=: Data.toQueryList "member" tagKeys
      ]

-- | /See:/ 'newUntagMFADeviceResponse' smart constructor.
data UntagMFADeviceResponse = UntagMFADeviceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UntagMFADeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUntagMFADeviceResponse ::
  UntagMFADeviceResponse
newUntagMFADeviceResponse = UntagMFADeviceResponse'

instance Prelude.NFData UntagMFADeviceResponse where
  rnf _ = ()
