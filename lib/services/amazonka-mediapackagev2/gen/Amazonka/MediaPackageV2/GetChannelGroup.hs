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
-- Module      : Amazonka.MediaPackageV2.GetChannelGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the specified channel group that\'s configured in AWS
-- Elemental MediaPackage, including the channels and origin endpoints that
-- are associated with it.
module Amazonka.MediaPackageV2.GetChannelGroup
  ( -- * Creating a Request
    GetChannelGroup (..),
    newGetChannelGroup,

    -- * Request Lenses
    getChannelGroup_channelGroupName,

    -- * Destructuring the Response
    GetChannelGroupResponse (..),
    newGetChannelGroupResponse,

    -- * Response Lenses
    getChannelGroupResponse_description,
    getChannelGroupResponse_tags,
    getChannelGroupResponse_httpStatus,
    getChannelGroupResponse_channelGroupName,
    getChannelGroupResponse_arn,
    getChannelGroupResponse_egressDomain,
    getChannelGroupResponse_createdAt,
    getChannelGroupResponse_modifiedAt,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackageV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetChannelGroup' smart constructor.
data GetChannelGroup = GetChannelGroup'
  { -- | The name that describes the channel group. The name is the primary
    -- identifier for the channel group, and must be unique for your account in
    -- the AWS Region.
    channelGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetChannelGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelGroupName', 'getChannelGroup_channelGroupName' - The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
newGetChannelGroup ::
  -- | 'channelGroupName'
  Prelude.Text ->
  GetChannelGroup
newGetChannelGroup pChannelGroupName_ =
  GetChannelGroup'
    { channelGroupName =
        pChannelGroupName_
    }

-- | The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
getChannelGroup_channelGroupName :: Lens.Lens' GetChannelGroup Prelude.Text
getChannelGroup_channelGroupName = Lens.lens (\GetChannelGroup' {channelGroupName} -> channelGroupName) (\s@GetChannelGroup' {} a -> s {channelGroupName = a} :: GetChannelGroup)

instance Core.AWSRequest GetChannelGroup where
  type
    AWSResponse GetChannelGroup =
      GetChannelGroupResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetChannelGroupResponse'
            Prelude.<$> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ChannelGroupName")
            Prelude.<*> (x Data..:> "Arn")
            Prelude.<*> (x Data..:> "EgressDomain")
            Prelude.<*> (x Data..:> "CreatedAt")
            Prelude.<*> (x Data..:> "ModifiedAt")
      )

instance Prelude.Hashable GetChannelGroup where
  hashWithSalt _salt GetChannelGroup' {..} =
    _salt `Prelude.hashWithSalt` channelGroupName

instance Prelude.NFData GetChannelGroup where
  rnf GetChannelGroup' {..} =
    Prelude.rnf channelGroupName

instance Data.ToHeaders GetChannelGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetChannelGroup where
  toPath GetChannelGroup' {..} =
    Prelude.mconcat
      ["/channelGroup/", Data.toBS channelGroupName]

instance Data.ToQuery GetChannelGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetChannelGroupResponse' smart constructor.
data GetChannelGroupResponse = GetChannelGroupResponse'
  { -- | The description for your channel group.
    description :: Prelude.Maybe Prelude.Text,
    -- | The comma-separated list of tag key:value pairs assigned to the channel
    -- group.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name that describes the channel group. The name is the primary
    -- identifier for the channel group, and must be unique for your account in
    -- the AWS Region.
    channelGroupName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) associated with the resource.
    arn :: Prelude.Text,
    -- | The output domain where the source stream should be sent. Integrate the
    -- domain with a downstream CDN (such as Amazon CloudFront) or playback
    -- device.
    egressDomain :: Prelude.Text,
    -- | The date and time the channel group was created.
    createdAt :: Data.POSIX,
    -- | The date and time the channel group was modified.
    modifiedAt :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetChannelGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'getChannelGroupResponse_description' - The description for your channel group.
--
-- 'tags', 'getChannelGroupResponse_tags' - The comma-separated list of tag key:value pairs assigned to the channel
-- group.
--
-- 'httpStatus', 'getChannelGroupResponse_httpStatus' - The response's http status code.
--
-- 'channelGroupName', 'getChannelGroupResponse_channelGroupName' - The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
--
-- 'arn', 'getChannelGroupResponse_arn' - The Amazon Resource Name (ARN) associated with the resource.
--
-- 'egressDomain', 'getChannelGroupResponse_egressDomain' - The output domain where the source stream should be sent. Integrate the
-- domain with a downstream CDN (such as Amazon CloudFront) or playback
-- device.
--
-- 'createdAt', 'getChannelGroupResponse_createdAt' - The date and time the channel group was created.
--
-- 'modifiedAt', 'getChannelGroupResponse_modifiedAt' - The date and time the channel group was modified.
newGetChannelGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'channelGroupName'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'egressDomain'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'modifiedAt'
  Prelude.UTCTime ->
  GetChannelGroupResponse
newGetChannelGroupResponse
  pHttpStatus_
  pChannelGroupName_
  pArn_
  pEgressDomain_
  pCreatedAt_
  pModifiedAt_ =
    GetChannelGroupResponse'
      { description =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        channelGroupName = pChannelGroupName_,
        arn = pArn_,
        egressDomain = pEgressDomain_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        modifiedAt = Data._Time Lens.# pModifiedAt_
      }

-- | The description for your channel group.
getChannelGroupResponse_description :: Lens.Lens' GetChannelGroupResponse (Prelude.Maybe Prelude.Text)
getChannelGroupResponse_description = Lens.lens (\GetChannelGroupResponse' {description} -> description) (\s@GetChannelGroupResponse' {} a -> s {description = a} :: GetChannelGroupResponse)

-- | The comma-separated list of tag key:value pairs assigned to the channel
-- group.
getChannelGroupResponse_tags :: Lens.Lens' GetChannelGroupResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getChannelGroupResponse_tags = Lens.lens (\GetChannelGroupResponse' {tags} -> tags) (\s@GetChannelGroupResponse' {} a -> s {tags = a} :: GetChannelGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getChannelGroupResponse_httpStatus :: Lens.Lens' GetChannelGroupResponse Prelude.Int
getChannelGroupResponse_httpStatus = Lens.lens (\GetChannelGroupResponse' {httpStatus} -> httpStatus) (\s@GetChannelGroupResponse' {} a -> s {httpStatus = a} :: GetChannelGroupResponse)

-- | The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
getChannelGroupResponse_channelGroupName :: Lens.Lens' GetChannelGroupResponse Prelude.Text
getChannelGroupResponse_channelGroupName = Lens.lens (\GetChannelGroupResponse' {channelGroupName} -> channelGroupName) (\s@GetChannelGroupResponse' {} a -> s {channelGroupName = a} :: GetChannelGroupResponse)

-- | The Amazon Resource Name (ARN) associated with the resource.
getChannelGroupResponse_arn :: Lens.Lens' GetChannelGroupResponse Prelude.Text
getChannelGroupResponse_arn = Lens.lens (\GetChannelGroupResponse' {arn} -> arn) (\s@GetChannelGroupResponse' {} a -> s {arn = a} :: GetChannelGroupResponse)

-- | The output domain where the source stream should be sent. Integrate the
-- domain with a downstream CDN (such as Amazon CloudFront) or playback
-- device.
getChannelGroupResponse_egressDomain :: Lens.Lens' GetChannelGroupResponse Prelude.Text
getChannelGroupResponse_egressDomain = Lens.lens (\GetChannelGroupResponse' {egressDomain} -> egressDomain) (\s@GetChannelGroupResponse' {} a -> s {egressDomain = a} :: GetChannelGroupResponse)

-- | The date and time the channel group was created.
getChannelGroupResponse_createdAt :: Lens.Lens' GetChannelGroupResponse Prelude.UTCTime
getChannelGroupResponse_createdAt = Lens.lens (\GetChannelGroupResponse' {createdAt} -> createdAt) (\s@GetChannelGroupResponse' {} a -> s {createdAt = a} :: GetChannelGroupResponse) Prelude.. Data._Time

-- | The date and time the channel group was modified.
getChannelGroupResponse_modifiedAt :: Lens.Lens' GetChannelGroupResponse Prelude.UTCTime
getChannelGroupResponse_modifiedAt = Lens.lens (\GetChannelGroupResponse' {modifiedAt} -> modifiedAt) (\s@GetChannelGroupResponse' {} a -> s {modifiedAt = a} :: GetChannelGroupResponse) Prelude.. Data._Time

instance Prelude.NFData GetChannelGroupResponse where
  rnf GetChannelGroupResponse' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf channelGroupName
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf egressDomain
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf modifiedAt
