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
-- Module      : Amazonka.MediaPackageV2.UpdateChannelGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update the specified channel group. You can edit the description on a
-- channel group for easier identification later from the AWS Elemental
-- MediaPackage console. You can\'t edit the name of the channel group.
--
-- Any edits you make that impact the video output may not be reflected for
-- a few minutes.
module Amazonka.MediaPackageV2.UpdateChannelGroup
  ( -- * Creating a Request
    UpdateChannelGroup (..),
    newUpdateChannelGroup,

    -- * Request Lenses
    updateChannelGroup_description,
    updateChannelGroup_channelGroupName,

    -- * Destructuring the Response
    UpdateChannelGroupResponse (..),
    newUpdateChannelGroupResponse,

    -- * Response Lenses
    updateChannelGroupResponse_description,
    updateChannelGroupResponse_tags,
    updateChannelGroupResponse_httpStatus,
    updateChannelGroupResponse_channelGroupName,
    updateChannelGroupResponse_arn,
    updateChannelGroupResponse_egressDomain,
    updateChannelGroupResponse_createdAt,
    updateChannelGroupResponse_modifiedAt,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackageV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateChannelGroup' smart constructor.
data UpdateChannelGroup = UpdateChannelGroup'
  { -- | Any descriptive information that you want to add to the channel group
    -- for future identification purposes.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name that describes the channel group. The name is the primary
    -- identifier for the channel group, and must be unique for your account in
    -- the AWS Region.
    channelGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateChannelGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateChannelGroup_description' - Any descriptive information that you want to add to the channel group
-- for future identification purposes.
--
-- 'channelGroupName', 'updateChannelGroup_channelGroupName' - The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
newUpdateChannelGroup ::
  -- | 'channelGroupName'
  Prelude.Text ->
  UpdateChannelGroup
newUpdateChannelGroup pChannelGroupName_ =
  UpdateChannelGroup'
    { description = Prelude.Nothing,
      channelGroupName = pChannelGroupName_
    }

-- | Any descriptive information that you want to add to the channel group
-- for future identification purposes.
updateChannelGroup_description :: Lens.Lens' UpdateChannelGroup (Prelude.Maybe Prelude.Text)
updateChannelGroup_description = Lens.lens (\UpdateChannelGroup' {description} -> description) (\s@UpdateChannelGroup' {} a -> s {description = a} :: UpdateChannelGroup)

-- | The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
updateChannelGroup_channelGroupName :: Lens.Lens' UpdateChannelGroup Prelude.Text
updateChannelGroup_channelGroupName = Lens.lens (\UpdateChannelGroup' {channelGroupName} -> channelGroupName) (\s@UpdateChannelGroup' {} a -> s {channelGroupName = a} :: UpdateChannelGroup)

instance Core.AWSRequest UpdateChannelGroup where
  type
    AWSResponse UpdateChannelGroup =
      UpdateChannelGroupResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateChannelGroupResponse'
            Prelude.<$> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ChannelGroupName")
            Prelude.<*> (x Data..:> "Arn")
            Prelude.<*> (x Data..:> "EgressDomain")
            Prelude.<*> (x Data..:> "CreatedAt")
            Prelude.<*> (x Data..:> "ModifiedAt")
      )

instance Prelude.Hashable UpdateChannelGroup where
  hashWithSalt _salt UpdateChannelGroup' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` channelGroupName

instance Prelude.NFData UpdateChannelGroup where
  rnf UpdateChannelGroup' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf channelGroupName

instance Data.ToHeaders UpdateChannelGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateChannelGroup where
  toJSON UpdateChannelGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Description" Data..=) Prelude.<$> description]
      )

instance Data.ToPath UpdateChannelGroup where
  toPath UpdateChannelGroup' {..} =
    Prelude.mconcat
      ["/channelGroup/", Data.toBS channelGroupName]

instance Data.ToQuery UpdateChannelGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateChannelGroupResponse' smart constructor.
data UpdateChannelGroupResponse = UpdateChannelGroupResponse'
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
    -- | The output domain where the source stream is sent. Integrate the domain
    -- with a downstream CDN (such as Amazon CloudFront) or playback device.
    egressDomain :: Prelude.Text,
    -- | The date and time the channel group was created.
    createdAt :: Data.POSIX,
    -- | The date and time the channel group was modified.
    modifiedAt :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateChannelGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateChannelGroupResponse_description' - The description for your channel group.
--
-- 'tags', 'updateChannelGroupResponse_tags' - The comma-separated list of tag key:value pairs assigned to the channel
-- group.
--
-- 'httpStatus', 'updateChannelGroupResponse_httpStatus' - The response's http status code.
--
-- 'channelGroupName', 'updateChannelGroupResponse_channelGroupName' - The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
--
-- 'arn', 'updateChannelGroupResponse_arn' - The Amazon Resource Name (ARN) associated with the resource.
--
-- 'egressDomain', 'updateChannelGroupResponse_egressDomain' - The output domain where the source stream is sent. Integrate the domain
-- with a downstream CDN (such as Amazon CloudFront) or playback device.
--
-- 'createdAt', 'updateChannelGroupResponse_createdAt' - The date and time the channel group was created.
--
-- 'modifiedAt', 'updateChannelGroupResponse_modifiedAt' - The date and time the channel group was modified.
newUpdateChannelGroupResponse ::
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
  UpdateChannelGroupResponse
newUpdateChannelGroupResponse
  pHttpStatus_
  pChannelGroupName_
  pArn_
  pEgressDomain_
  pCreatedAt_
  pModifiedAt_ =
    UpdateChannelGroupResponse'
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
updateChannelGroupResponse_description :: Lens.Lens' UpdateChannelGroupResponse (Prelude.Maybe Prelude.Text)
updateChannelGroupResponse_description = Lens.lens (\UpdateChannelGroupResponse' {description} -> description) (\s@UpdateChannelGroupResponse' {} a -> s {description = a} :: UpdateChannelGroupResponse)

-- | The comma-separated list of tag key:value pairs assigned to the channel
-- group.
updateChannelGroupResponse_tags :: Lens.Lens' UpdateChannelGroupResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateChannelGroupResponse_tags = Lens.lens (\UpdateChannelGroupResponse' {tags} -> tags) (\s@UpdateChannelGroupResponse' {} a -> s {tags = a} :: UpdateChannelGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updateChannelGroupResponse_httpStatus :: Lens.Lens' UpdateChannelGroupResponse Prelude.Int
updateChannelGroupResponse_httpStatus = Lens.lens (\UpdateChannelGroupResponse' {httpStatus} -> httpStatus) (\s@UpdateChannelGroupResponse' {} a -> s {httpStatus = a} :: UpdateChannelGroupResponse)

-- | The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
updateChannelGroupResponse_channelGroupName :: Lens.Lens' UpdateChannelGroupResponse Prelude.Text
updateChannelGroupResponse_channelGroupName = Lens.lens (\UpdateChannelGroupResponse' {channelGroupName} -> channelGroupName) (\s@UpdateChannelGroupResponse' {} a -> s {channelGroupName = a} :: UpdateChannelGroupResponse)

-- | The Amazon Resource Name (ARN) associated with the resource.
updateChannelGroupResponse_arn :: Lens.Lens' UpdateChannelGroupResponse Prelude.Text
updateChannelGroupResponse_arn = Lens.lens (\UpdateChannelGroupResponse' {arn} -> arn) (\s@UpdateChannelGroupResponse' {} a -> s {arn = a} :: UpdateChannelGroupResponse)

-- | The output domain where the source stream is sent. Integrate the domain
-- with a downstream CDN (such as Amazon CloudFront) or playback device.
updateChannelGroupResponse_egressDomain :: Lens.Lens' UpdateChannelGroupResponse Prelude.Text
updateChannelGroupResponse_egressDomain = Lens.lens (\UpdateChannelGroupResponse' {egressDomain} -> egressDomain) (\s@UpdateChannelGroupResponse' {} a -> s {egressDomain = a} :: UpdateChannelGroupResponse)

-- | The date and time the channel group was created.
updateChannelGroupResponse_createdAt :: Lens.Lens' UpdateChannelGroupResponse Prelude.UTCTime
updateChannelGroupResponse_createdAt = Lens.lens (\UpdateChannelGroupResponse' {createdAt} -> createdAt) (\s@UpdateChannelGroupResponse' {} a -> s {createdAt = a} :: UpdateChannelGroupResponse) Prelude.. Data._Time

-- | The date and time the channel group was modified.
updateChannelGroupResponse_modifiedAt :: Lens.Lens' UpdateChannelGroupResponse Prelude.UTCTime
updateChannelGroupResponse_modifiedAt = Lens.lens (\UpdateChannelGroupResponse' {modifiedAt} -> modifiedAt) (\s@UpdateChannelGroupResponse' {} a -> s {modifiedAt = a} :: UpdateChannelGroupResponse) Prelude.. Data._Time

instance Prelude.NFData UpdateChannelGroupResponse where
  rnf UpdateChannelGroupResponse' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf channelGroupName
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf egressDomain
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf modifiedAt
