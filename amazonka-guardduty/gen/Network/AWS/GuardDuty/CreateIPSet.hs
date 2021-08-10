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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateIPSet' smart constructor.
data CreateIPSet = CreateIPSet'
  { -- | The tags to be added to a new IP set resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The idempotency token for the create request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of the detector of the GuardDuty account that you want to
    -- create an IPSet for.
    detectorId :: Prelude.Text,
    -- | The user-friendly name to identify the IPSet.
    --
    -- Allowed characters are alphanumerics, spaces, hyphens (-), and
    -- underscores (_).
    name :: Prelude.Text,
    -- | The format of the file that contains the IPSet.
    format :: IpSetFormat,
    -- | The URI of the file that contains the IPSet. For example:
    -- https:\/\/s3.us-west-2.amazonaws.com\/my-bucket\/my-object-key.
    location :: Prelude.Text,
    -- | A Boolean value that indicates whether GuardDuty is to start using the
    -- uploaded IPSet.
    activate :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'format'
  IpSetFormat ->
  -- | 'location'
  Prelude.Text ->
  -- | 'activate'
  Prelude.Bool ->
  CreateIPSet
newCreateIPSet
  pDetectorId_
  pName_
  pFormat_
  pLocation_
  pActivate_ =
    CreateIPSet'
      { tags = Prelude.Nothing,
        clientToken = Prelude.Nothing,
        detectorId = pDetectorId_,
        name = pName_,
        format = pFormat_,
        location = pLocation_,
        activate = pActivate_
      }

-- | The tags to be added to a new IP set resource.
createIPSet_tags :: Lens.Lens' CreateIPSet (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createIPSet_tags = Lens.lens (\CreateIPSet' {tags} -> tags) (\s@CreateIPSet' {} a -> s {tags = a} :: CreateIPSet) Prelude.. Lens.mapping Lens._Coerce

-- | The idempotency token for the create request.
createIPSet_clientToken :: Lens.Lens' CreateIPSet (Prelude.Maybe Prelude.Text)
createIPSet_clientToken = Lens.lens (\CreateIPSet' {clientToken} -> clientToken) (\s@CreateIPSet' {} a -> s {clientToken = a} :: CreateIPSet)

-- | The unique ID of the detector of the GuardDuty account that you want to
-- create an IPSet for.
createIPSet_detectorId :: Lens.Lens' CreateIPSet Prelude.Text
createIPSet_detectorId = Lens.lens (\CreateIPSet' {detectorId} -> detectorId) (\s@CreateIPSet' {} a -> s {detectorId = a} :: CreateIPSet)

-- | The user-friendly name to identify the IPSet.
--
-- Allowed characters are alphanumerics, spaces, hyphens (-), and
-- underscores (_).
createIPSet_name :: Lens.Lens' CreateIPSet Prelude.Text
createIPSet_name = Lens.lens (\CreateIPSet' {name} -> name) (\s@CreateIPSet' {} a -> s {name = a} :: CreateIPSet)

-- | The format of the file that contains the IPSet.
createIPSet_format :: Lens.Lens' CreateIPSet IpSetFormat
createIPSet_format = Lens.lens (\CreateIPSet' {format} -> format) (\s@CreateIPSet' {} a -> s {format = a} :: CreateIPSet)

-- | The URI of the file that contains the IPSet. For example:
-- https:\/\/s3.us-west-2.amazonaws.com\/my-bucket\/my-object-key.
createIPSet_location :: Lens.Lens' CreateIPSet Prelude.Text
createIPSet_location = Lens.lens (\CreateIPSet' {location} -> location) (\s@CreateIPSet' {} a -> s {location = a} :: CreateIPSet)

-- | A Boolean value that indicates whether GuardDuty is to start using the
-- uploaded IPSet.
createIPSet_activate :: Lens.Lens' CreateIPSet Prelude.Bool
createIPSet_activate = Lens.lens (\CreateIPSet' {activate} -> activate) (\s@CreateIPSet' {} a -> s {activate = a} :: CreateIPSet)

instance Core.AWSRequest CreateIPSet where
  type AWSResponse CreateIPSet = CreateIPSetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateIPSetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "ipSetId")
      )

instance Prelude.Hashable CreateIPSet

instance Prelude.NFData CreateIPSet

instance Core.ToHeaders CreateIPSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateIPSet where
  toJSON CreateIPSet' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            ("clientToken" Core..=) Prelude.<$> clientToken,
            Prelude.Just ("name" Core..= name),
            Prelude.Just ("format" Core..= format),
            Prelude.Just ("location" Core..= location),
            Prelude.Just ("activate" Core..= activate)
          ]
      )

instance Core.ToPath CreateIPSet where
  toPath CreateIPSet' {..} =
    Prelude.mconcat
      ["/detector/", Core.toBS detectorId, "/ipset"]

instance Core.ToQuery CreateIPSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateIPSetResponse' smart constructor.
data CreateIPSetResponse = CreateIPSetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the IPSet resource.
    ipSetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  -- | 'ipSetId'
  Prelude.Text ->
  CreateIPSetResponse
newCreateIPSetResponse pHttpStatus_ pIpSetId_ =
  CreateIPSetResponse'
    { httpStatus = pHttpStatus_,
      ipSetId = pIpSetId_
    }

-- | The response's http status code.
createIPSetResponse_httpStatus :: Lens.Lens' CreateIPSetResponse Prelude.Int
createIPSetResponse_httpStatus = Lens.lens (\CreateIPSetResponse' {httpStatus} -> httpStatus) (\s@CreateIPSetResponse' {} a -> s {httpStatus = a} :: CreateIPSetResponse)

-- | The ID of the IPSet resource.
createIPSetResponse_ipSetId :: Lens.Lens' CreateIPSetResponse Prelude.Text
createIPSetResponse_ipSetId = Lens.lens (\CreateIPSetResponse' {ipSetId} -> ipSetId) (\s@CreateIPSetResponse' {} a -> s {ipSetId = a} :: CreateIPSetResponse)

instance Prelude.NFData CreateIPSetResponse
