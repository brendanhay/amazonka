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
-- Module      : Network.AWS.GuardDuty.CreateIPSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new IPSet, which is called a trusted IP list in the console
-- user interface. An IPSet is a list of IP addresses that are trusted for
-- secure communication with AWS infrastructure and applications. GuardDuty
-- doesn\'t generate findings for IP addresses that are included in IPSets.
-- Only users from the administrator account can use this operation.
module Network.AWS.GuardDuty.CreateIPSet
  ( -- * Creating a Request
    CreateIPSet (..),
    newCreateIPSet,

    -- * Request Lenses
    createIPSet_tags,
    createIPSet_clientToken,
    createIPSet_detectorId,
    createIPSet_name,
    createIPSet_format,
    createIPSet_location,
    createIPSet_activate,

    -- * Destructuring the Response
    CreateIPSetResponse (..),
    newCreateIPSetResponse,

    -- * Response Lenses
    createIPSetResponse_httpStatus,
    createIPSetResponse_ipSetId,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateIPSet' smart constructor.
data CreateIPSet = CreateIPSet'
  { -- | The tags to be added to a new IP set resource.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The idempotency token for the create request.
    clientToken :: Core.Maybe Core.Text,
    -- | The unique ID of the detector of the GuardDuty account that you want to
    -- create an IPSet for.
    detectorId :: Core.Text,
    -- | The user-friendly name to identify the IPSet.
    --
    -- Allowed characters are alphanumerics, spaces, hyphens (-), and
    -- underscores (_).
    name :: Core.Text,
    -- | The format of the file that contains the IPSet.
    format :: IpSetFormat,
    -- | The URI of the file that contains the IPSet. For example:
    -- https:\/\/s3.us-west-2.amazonaws.com\/my-bucket\/my-object-key.
    location :: Core.Text,
    -- | A Boolean value that indicates whether GuardDuty is to start using the
    -- uploaded IPSet.
    activate :: Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateIPSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createIPSet_tags' - The tags to be added to a new IP set resource.
--
-- 'clientToken', 'createIPSet_clientToken' - The idempotency token for the create request.
--
-- 'detectorId', 'createIPSet_detectorId' - The unique ID of the detector of the GuardDuty account that you want to
-- create an IPSet for.
--
-- 'name', 'createIPSet_name' - The user-friendly name to identify the IPSet.
--
-- Allowed characters are alphanumerics, spaces, hyphens (-), and
-- underscores (_).
--
-- 'format', 'createIPSet_format' - The format of the file that contains the IPSet.
--
-- 'location', 'createIPSet_location' - The URI of the file that contains the IPSet. For example:
-- https:\/\/s3.us-west-2.amazonaws.com\/my-bucket\/my-object-key.
--
-- 'activate', 'createIPSet_activate' - A Boolean value that indicates whether GuardDuty is to start using the
-- uploaded IPSet.
newCreateIPSet ::
  -- | 'detectorId'
  Core.Text ->
  -- | 'name'
  Core.Text ->
  -- | 'format'
  IpSetFormat ->
  -- | 'location'
  Core.Text ->
  -- | 'activate'
  Core.Bool ->
  CreateIPSet
newCreateIPSet
  pDetectorId_
  pName_
  pFormat_
  pLocation_
  pActivate_ =
    CreateIPSet'
      { tags = Core.Nothing,
        clientToken = Core.Nothing,
        detectorId = pDetectorId_,
        name = pName_,
        format = pFormat_,
        location = pLocation_,
        activate = pActivate_
      }

-- | The tags to be added to a new IP set resource.
createIPSet_tags :: Lens.Lens' CreateIPSet (Core.Maybe (Core.HashMap Core.Text Core.Text))
createIPSet_tags = Lens.lens (\CreateIPSet' {tags} -> tags) (\s@CreateIPSet' {} a -> s {tags = a} :: CreateIPSet) Core.. Lens.mapping Lens._Coerce

-- | The idempotency token for the create request.
createIPSet_clientToken :: Lens.Lens' CreateIPSet (Core.Maybe Core.Text)
createIPSet_clientToken = Lens.lens (\CreateIPSet' {clientToken} -> clientToken) (\s@CreateIPSet' {} a -> s {clientToken = a} :: CreateIPSet)

-- | The unique ID of the detector of the GuardDuty account that you want to
-- create an IPSet for.
createIPSet_detectorId :: Lens.Lens' CreateIPSet Core.Text
createIPSet_detectorId = Lens.lens (\CreateIPSet' {detectorId} -> detectorId) (\s@CreateIPSet' {} a -> s {detectorId = a} :: CreateIPSet)

-- | The user-friendly name to identify the IPSet.
--
-- Allowed characters are alphanumerics, spaces, hyphens (-), and
-- underscores (_).
createIPSet_name :: Lens.Lens' CreateIPSet Core.Text
createIPSet_name = Lens.lens (\CreateIPSet' {name} -> name) (\s@CreateIPSet' {} a -> s {name = a} :: CreateIPSet)

-- | The format of the file that contains the IPSet.
createIPSet_format :: Lens.Lens' CreateIPSet IpSetFormat
createIPSet_format = Lens.lens (\CreateIPSet' {format} -> format) (\s@CreateIPSet' {} a -> s {format = a} :: CreateIPSet)

-- | The URI of the file that contains the IPSet. For example:
-- https:\/\/s3.us-west-2.amazonaws.com\/my-bucket\/my-object-key.
createIPSet_location :: Lens.Lens' CreateIPSet Core.Text
createIPSet_location = Lens.lens (\CreateIPSet' {location} -> location) (\s@CreateIPSet' {} a -> s {location = a} :: CreateIPSet)

-- | A Boolean value that indicates whether GuardDuty is to start using the
-- uploaded IPSet.
createIPSet_activate :: Lens.Lens' CreateIPSet Core.Bool
createIPSet_activate = Lens.lens (\CreateIPSet' {activate} -> activate) (\s@CreateIPSet' {} a -> s {activate = a} :: CreateIPSet)

instance Core.AWSRequest CreateIPSet where
  type AWSResponse CreateIPSet = CreateIPSetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateIPSetResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "ipSetId")
      )

instance Core.Hashable CreateIPSet

instance Core.NFData CreateIPSet

instance Core.ToHeaders CreateIPSet where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateIPSet where
  toJSON CreateIPSet' {..} =
    Core.object
      ( Core.catMaybes
          [ ("tags" Core..=) Core.<$> tags,
            ("clientToken" Core..=) Core.<$> clientToken,
            Core.Just ("name" Core..= name),
            Core.Just ("format" Core..= format),
            Core.Just ("location" Core..= location),
            Core.Just ("activate" Core..= activate)
          ]
      )

instance Core.ToPath CreateIPSet where
  toPath CreateIPSet' {..} =
    Core.mconcat
      ["/detector/", Core.toBS detectorId, "/ipset"]

instance Core.ToQuery CreateIPSet where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateIPSetResponse' smart constructor.
data CreateIPSetResponse = CreateIPSetResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The ID of the IPSet resource.
    ipSetId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateIPSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createIPSetResponse_httpStatus' - The response's http status code.
--
-- 'ipSetId', 'createIPSetResponse_ipSetId' - The ID of the IPSet resource.
newCreateIPSetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'ipSetId'
  Core.Text ->
  CreateIPSetResponse
newCreateIPSetResponse pHttpStatus_ pIpSetId_ =
  CreateIPSetResponse'
    { httpStatus = pHttpStatus_,
      ipSetId = pIpSetId_
    }

-- | The response's http status code.
createIPSetResponse_httpStatus :: Lens.Lens' CreateIPSetResponse Core.Int
createIPSetResponse_httpStatus = Lens.lens (\CreateIPSetResponse' {httpStatus} -> httpStatus) (\s@CreateIPSetResponse' {} a -> s {httpStatus = a} :: CreateIPSetResponse)

-- | The ID of the IPSet resource.
createIPSetResponse_ipSetId :: Lens.Lens' CreateIPSetResponse Core.Text
createIPSetResponse_ipSetId = Lens.lens (\CreateIPSetResponse' {ipSetId} -> ipSetId) (\s@CreateIPSetResponse' {} a -> s {ipSetId = a} :: CreateIPSetResponse)

instance Core.NFData CreateIPSetResponse
