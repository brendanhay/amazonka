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
-- Module      : Amazonka.IAM.CreateVirtualMFADevice
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new virtual MFA device for the Amazon Web Services account.
-- After creating the virtual MFA, use EnableMFADevice to attach the MFA
-- device to an IAM user. For more information about creating and working
-- with virtual MFA devices, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_VirtualMFA.html Using a virtual MFA device>
-- in the /IAM User Guide/.
--
-- For information about the maximum number of MFA devices you can create,
-- see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-quotas.html IAM and STS quotas>
-- in the /IAM User Guide/.
--
-- The seed information contained in the QR code and the Base32 string
-- should be treated like any other secret access information. In other
-- words, protect the seed information as you would your Amazon Web
-- Services access keys or your passwords. After you provision your virtual
-- device, you should ensure that the information is destroyed following
-- secure procedures.
module Amazonka.IAM.CreateVirtualMFADevice
  ( -- * Creating a Request
    CreateVirtualMFADevice (..),
    newCreateVirtualMFADevice,

    -- * Request Lenses
    createVirtualMFADevice_path,
    createVirtualMFADevice_tags,
    createVirtualMFADevice_virtualMFADeviceName,

    -- * Destructuring the Response
    CreateVirtualMFADeviceResponse (..),
    newCreateVirtualMFADeviceResponse,

    -- * Response Lenses
    createVirtualMFADeviceResponse_httpStatus,
    createVirtualMFADeviceResponse_virtualMFADevice,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateVirtualMFADevice' smart constructor.
data CreateVirtualMFADevice = CreateVirtualMFADevice'
  { -- | The path for the virtual MFA device. For more information about paths,
    -- see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    --
    -- This parameter is optional. If it is not included, it defaults to a
    -- slash (\/).
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of either a forward slash (\/) by itself or a string that
    -- must begin and end with forward slashes. In addition, it can contain any
    -- ASCII character from the ! (@\\u0021@) through the DEL character
    -- (@\\u007F@), including most punctuation characters, digits, and upper
    -- and lowercased letters.
    path :: Prelude.Maybe Prelude.Text,
    -- | A list of tags that you want to attach to the new IAM virtual MFA
    -- device. Each tag consists of a key name and an associated value. For
    -- more information about tagging, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
    -- in the /IAM User Guide/.
    --
    -- If any one of the tags is invalid or if you exceed the allowed maximum
    -- number of tags, then the entire request fails and the resource is not
    -- created.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the virtual MFA device. Use with path to uniquely identify a
    -- virtual MFA device.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    virtualMFADeviceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVirtualMFADevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'path', 'createVirtualMFADevice_path' - The path for the virtual MFA device. For more information about paths,
-- see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
--
-- This parameter is optional. If it is not included, it defaults to a
-- slash (\/).
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of either a forward slash (\/) by itself or a string that
-- must begin and end with forward slashes. In addition, it can contain any
-- ASCII character from the ! (@\\u0021@) through the DEL character
-- (@\\u007F@), including most punctuation characters, digits, and upper
-- and lowercased letters.
--
-- 'tags', 'createVirtualMFADevice_tags' - A list of tags that you want to attach to the new IAM virtual MFA
-- device. Each tag consists of a key name and an associated value. For
-- more information about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
--
-- If any one of the tags is invalid or if you exceed the allowed maximum
-- number of tags, then the entire request fails and the resource is not
-- created.
--
-- 'virtualMFADeviceName', 'createVirtualMFADevice_virtualMFADeviceName' - The name of the virtual MFA device. Use with path to uniquely identify a
-- virtual MFA device.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newCreateVirtualMFADevice ::
  -- | 'virtualMFADeviceName'
  Prelude.Text ->
  CreateVirtualMFADevice
newCreateVirtualMFADevice pVirtualMFADeviceName_ =
  CreateVirtualMFADevice'
    { path = Prelude.Nothing,
      tags = Prelude.Nothing,
      virtualMFADeviceName = pVirtualMFADeviceName_
    }

-- | The path for the virtual MFA device. For more information about paths,
-- see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
--
-- This parameter is optional. If it is not included, it defaults to a
-- slash (\/).
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of either a forward slash (\/) by itself or a string that
-- must begin and end with forward slashes. In addition, it can contain any
-- ASCII character from the ! (@\\u0021@) through the DEL character
-- (@\\u007F@), including most punctuation characters, digits, and upper
-- and lowercased letters.
createVirtualMFADevice_path :: Lens.Lens' CreateVirtualMFADevice (Prelude.Maybe Prelude.Text)
createVirtualMFADevice_path = Lens.lens (\CreateVirtualMFADevice' {path} -> path) (\s@CreateVirtualMFADevice' {} a -> s {path = a} :: CreateVirtualMFADevice)

-- | A list of tags that you want to attach to the new IAM virtual MFA
-- device. Each tag consists of a key name and an associated value. For
-- more information about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
--
-- If any one of the tags is invalid or if you exceed the allowed maximum
-- number of tags, then the entire request fails and the resource is not
-- created.
createVirtualMFADevice_tags :: Lens.Lens' CreateVirtualMFADevice (Prelude.Maybe [Tag])
createVirtualMFADevice_tags = Lens.lens (\CreateVirtualMFADevice' {tags} -> tags) (\s@CreateVirtualMFADevice' {} a -> s {tags = a} :: CreateVirtualMFADevice) Prelude.. Lens.mapping Lens.coerced

-- | The name of the virtual MFA device. Use with path to uniquely identify a
-- virtual MFA device.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
createVirtualMFADevice_virtualMFADeviceName :: Lens.Lens' CreateVirtualMFADevice Prelude.Text
createVirtualMFADevice_virtualMFADeviceName = Lens.lens (\CreateVirtualMFADevice' {virtualMFADeviceName} -> virtualMFADeviceName) (\s@CreateVirtualMFADevice' {} a -> s {virtualMFADeviceName = a} :: CreateVirtualMFADevice)

instance Core.AWSRequest CreateVirtualMFADevice where
  type
    AWSResponse CreateVirtualMFADevice =
      CreateVirtualMFADeviceResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateVirtualMFADeviceResult"
      ( \s h x ->
          CreateVirtualMFADeviceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "VirtualMFADevice")
      )

instance Prelude.Hashable CreateVirtualMFADevice where
  hashWithSalt _salt CreateVirtualMFADevice' {..} =
    _salt
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` virtualMFADeviceName

instance Prelude.NFData CreateVirtualMFADevice where
  rnf CreateVirtualMFADevice' {..} =
    Prelude.rnf path `Prelude.seq`
      Prelude.rnf tags `Prelude.seq`
        Prelude.rnf virtualMFADeviceName

instance Data.ToHeaders CreateVirtualMFADevice where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateVirtualMFADevice where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateVirtualMFADevice where
  toQuery CreateVirtualMFADevice' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateVirtualMFADevice" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "Path" Data.=: path,
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> tags),
        "VirtualMFADeviceName" Data.=: virtualMFADeviceName
      ]

-- | Contains the response to a successful CreateVirtualMFADevice request.
--
-- /See:/ 'newCreateVirtualMFADeviceResponse' smart constructor.
data CreateVirtualMFADeviceResponse = CreateVirtualMFADeviceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A structure containing details about the new virtual MFA device.
    virtualMFADevice :: VirtualMFADevice
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVirtualMFADeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createVirtualMFADeviceResponse_httpStatus' - The response's http status code.
--
-- 'virtualMFADevice', 'createVirtualMFADeviceResponse_virtualMFADevice' - A structure containing details about the new virtual MFA device.
newCreateVirtualMFADeviceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'virtualMFADevice'
  VirtualMFADevice ->
  CreateVirtualMFADeviceResponse
newCreateVirtualMFADeviceResponse
  pHttpStatus_
  pVirtualMFADevice_ =
    CreateVirtualMFADeviceResponse'
      { httpStatus =
          pHttpStatus_,
        virtualMFADevice = pVirtualMFADevice_
      }

-- | The response's http status code.
createVirtualMFADeviceResponse_httpStatus :: Lens.Lens' CreateVirtualMFADeviceResponse Prelude.Int
createVirtualMFADeviceResponse_httpStatus = Lens.lens (\CreateVirtualMFADeviceResponse' {httpStatus} -> httpStatus) (\s@CreateVirtualMFADeviceResponse' {} a -> s {httpStatus = a} :: CreateVirtualMFADeviceResponse)

-- | A structure containing details about the new virtual MFA device.
createVirtualMFADeviceResponse_virtualMFADevice :: Lens.Lens' CreateVirtualMFADeviceResponse VirtualMFADevice
createVirtualMFADeviceResponse_virtualMFADevice = Lens.lens (\CreateVirtualMFADeviceResponse' {virtualMFADevice} -> virtualMFADevice) (\s@CreateVirtualMFADeviceResponse' {} a -> s {virtualMFADevice = a} :: CreateVirtualMFADeviceResponse)

instance
  Prelude.NFData
    CreateVirtualMFADeviceResponse
  where
  rnf CreateVirtualMFADeviceResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf virtualMFADevice
