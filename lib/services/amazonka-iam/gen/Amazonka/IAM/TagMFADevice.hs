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
-- Module      : Amazonka.IAM.TagMFADevice
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more tags to an IAM virtual multi-factor authentication
-- (MFA) device. If a tag with the same key name already exists, then that
-- tag is overwritten with the new value.
--
-- A tag consists of a key name and an associated value. By assigning tags
-- to your resources, you can do the following:
--
-- -   __Administrative grouping and discovery__ - Attach tags to resources
--     to aid in organization and search. For example, you could search for
--     all resources with the key name /Project/ and the value
--     /MyImportantProject/. Or search for all resources with the key name
--     /Cost Center/ and the value /41200/.
--
-- -   __Access control__ - Include tags in IAM user-based and
--     resource-based policies. You can use tags to restrict access to only
--     an IAM virtual MFA device that has a specified tag attached. For
--     examples of policies that show how to use tags to control access,
--     see
--     <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_tags.html Control access using IAM tags>
--     in the /IAM User Guide/.
--
-- -   If any one of the tags is invalid or if you exceed the allowed
--     maximum number of tags, then the entire request fails and the
--     resource is not created. For more information about tagging, see
--     <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
--     in the /IAM User Guide/.
--
-- -   Amazon Web Services always interprets the tag @Value@ as a single
--     string. If you need to store an array, you can store comma-separated
--     values in the string. However, you must interpret the value in your
--     code.
module Amazonka.IAM.TagMFADevice
  ( -- * Creating a Request
    TagMFADevice (..),
    newTagMFADevice,

    -- * Request Lenses
    tagMFADevice_serialNumber,
    tagMFADevice_tags,

    -- * Destructuring the Response
    TagMFADeviceResponse (..),
    newTagMFADeviceResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newTagMFADevice' smart constructor.
data TagMFADevice = TagMFADevice'
  { -- | The unique identifier for the IAM virtual MFA device to which you want
    -- to add tags. For virtual MFA devices, the serial number is the same as
    -- the ARN.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    serialNumber :: Prelude.Text,
    -- | The list of tags that you want to attach to the IAM virtual MFA device.
    -- Each tag consists of a key name and an associated value.
    tags :: [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagMFADevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serialNumber', 'tagMFADevice_serialNumber' - The unique identifier for the IAM virtual MFA device to which you want
-- to add tags. For virtual MFA devices, the serial number is the same as
-- the ARN.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'tags', 'tagMFADevice_tags' - The list of tags that you want to attach to the IAM virtual MFA device.
-- Each tag consists of a key name and an associated value.
newTagMFADevice ::
  -- | 'serialNumber'
  Prelude.Text ->
  TagMFADevice
newTagMFADevice pSerialNumber_ =
  TagMFADevice'
    { serialNumber = pSerialNumber_,
      tags = Prelude.mempty
    }

-- | The unique identifier for the IAM virtual MFA device to which you want
-- to add tags. For virtual MFA devices, the serial number is the same as
-- the ARN.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
tagMFADevice_serialNumber :: Lens.Lens' TagMFADevice Prelude.Text
tagMFADevice_serialNumber = Lens.lens (\TagMFADevice' {serialNumber} -> serialNumber) (\s@TagMFADevice' {} a -> s {serialNumber = a} :: TagMFADevice)

-- | The list of tags that you want to attach to the IAM virtual MFA device.
-- Each tag consists of a key name and an associated value.
tagMFADevice_tags :: Lens.Lens' TagMFADevice [Tag]
tagMFADevice_tags = Lens.lens (\TagMFADevice' {tags} -> tags) (\s@TagMFADevice' {} a -> s {tags = a} :: TagMFADevice) Prelude.. Lens.coerced

instance Core.AWSRequest TagMFADevice where
  type AWSResponse TagMFADevice = TagMFADeviceResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response = Response.receiveNull TagMFADeviceResponse'

instance Prelude.Hashable TagMFADevice where
  hashWithSalt _salt TagMFADevice' {..} =
    _salt `Prelude.hashWithSalt` serialNumber
      `Prelude.hashWithSalt` tags

instance Prelude.NFData TagMFADevice where
  rnf TagMFADevice' {..} =
    Prelude.rnf serialNumber
      `Prelude.seq` Prelude.rnf tags

instance Data.ToHeaders TagMFADevice where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath TagMFADevice where
  toPath = Prelude.const "/"

instance Data.ToQuery TagMFADevice where
  toQuery TagMFADevice' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("TagMFADevice" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "SerialNumber" Data.=: serialNumber,
        "Tags" Data.=: Data.toQueryList "member" tags
      ]

-- | /See:/ 'newTagMFADeviceResponse' smart constructor.
data TagMFADeviceResponse = TagMFADeviceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagMFADeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newTagMFADeviceResponse ::
  TagMFADeviceResponse
newTagMFADeviceResponse = TagMFADeviceResponse'

instance Prelude.NFData TagMFADeviceResponse where
  rnf _ = ()
