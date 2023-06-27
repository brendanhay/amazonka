{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.RAM.Types.ReplacePermissionAssociationsWork
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RAM.Types.ReplacePermissionAssociationsWork where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types.ReplacePermissionAssociationsWorkStatus

-- | A structure that represents the background work that RAM performs when
-- you invoke the ReplacePermissionAssociations operation.
--
-- /See:/ 'newReplacePermissionAssociationsWork' smart constructor.
data ReplacePermissionAssociationsWork = ReplacePermissionAssociationsWork'
  { -- | The date and time when this asynchronous background task was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
    -- of the managed permission that this background task is replacing.
    fromPermissionArn :: Prelude.Maybe Prelude.Text,
    -- | The version of the managed permission that this background task is
    -- replacing.
    fromPermissionVersion :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the background task associated with one
    -- ReplacePermissionAssociations request.
    id :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the status of this background task was last
    -- updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | Specifies the current status of the background tasks for the specified
    -- ID. The output is one of the following strings:
    --
    -- -   @IN_PROGRESS@
    --
    -- -   @COMPLETED@
    --
    -- -   @FAILED@
    status :: Prelude.Maybe ReplacePermissionAssociationsWorkStatus,
    -- | Specifies the reason for a @FAILED@ status. This field is present only
    -- when there @status@ is @FAILED@.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the managed permission that this background task is
    -- associating with the resource shares in place of the managed permission
    -- and version specified in @fromPermissionArn@ and
    -- @fromPermissionVersion@.
    toPermissionArn :: Prelude.Maybe Prelude.Text,
    -- | The version of the managed permission that this background task is
    -- associating with the resource shares. This is always the version that is
    -- currently the default for this managed permission.
    toPermissionVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplacePermissionAssociationsWork' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'replacePermissionAssociationsWork_creationTime' - The date and time when this asynchronous background task was created.
--
-- 'fromPermissionArn', 'replacePermissionAssociationsWork_fromPermissionArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of the managed permission that this background task is replacing.
--
-- 'fromPermissionVersion', 'replacePermissionAssociationsWork_fromPermissionVersion' - The version of the managed permission that this background task is
-- replacing.
--
-- 'id', 'replacePermissionAssociationsWork_id' - The unique identifier for the background task associated with one
-- ReplacePermissionAssociations request.
--
-- 'lastUpdatedTime', 'replacePermissionAssociationsWork_lastUpdatedTime' - The date and time when the status of this background task was last
-- updated.
--
-- 'status', 'replacePermissionAssociationsWork_status' - Specifies the current status of the background tasks for the specified
-- ID. The output is one of the following strings:
--
-- -   @IN_PROGRESS@
--
-- -   @COMPLETED@
--
-- -   @FAILED@
--
-- 'statusMessage', 'replacePermissionAssociationsWork_statusMessage' - Specifies the reason for a @FAILED@ status. This field is present only
-- when there @status@ is @FAILED@.
--
-- 'toPermissionArn', 'replacePermissionAssociationsWork_toPermissionArn' - The ARN of the managed permission that this background task is
-- associating with the resource shares in place of the managed permission
-- and version specified in @fromPermissionArn@ and
-- @fromPermissionVersion@.
--
-- 'toPermissionVersion', 'replacePermissionAssociationsWork_toPermissionVersion' - The version of the managed permission that this background task is
-- associating with the resource shares. This is always the version that is
-- currently the default for this managed permission.
newReplacePermissionAssociationsWork ::
  ReplacePermissionAssociationsWork
newReplacePermissionAssociationsWork =
  ReplacePermissionAssociationsWork'
    { creationTime =
        Prelude.Nothing,
      fromPermissionArn = Prelude.Nothing,
      fromPermissionVersion = Prelude.Nothing,
      id = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      toPermissionArn = Prelude.Nothing,
      toPermissionVersion = Prelude.Nothing
    }

-- | The date and time when this asynchronous background task was created.
replacePermissionAssociationsWork_creationTime :: Lens.Lens' ReplacePermissionAssociationsWork (Prelude.Maybe Prelude.UTCTime)
replacePermissionAssociationsWork_creationTime = Lens.lens (\ReplacePermissionAssociationsWork' {creationTime} -> creationTime) (\s@ReplacePermissionAssociationsWork' {} a -> s {creationTime = a} :: ReplacePermissionAssociationsWork) Prelude.. Lens.mapping Data._Time

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of the managed permission that this background task is replacing.
replacePermissionAssociationsWork_fromPermissionArn :: Lens.Lens' ReplacePermissionAssociationsWork (Prelude.Maybe Prelude.Text)
replacePermissionAssociationsWork_fromPermissionArn = Lens.lens (\ReplacePermissionAssociationsWork' {fromPermissionArn} -> fromPermissionArn) (\s@ReplacePermissionAssociationsWork' {} a -> s {fromPermissionArn = a} :: ReplacePermissionAssociationsWork)

-- | The version of the managed permission that this background task is
-- replacing.
replacePermissionAssociationsWork_fromPermissionVersion :: Lens.Lens' ReplacePermissionAssociationsWork (Prelude.Maybe Prelude.Text)
replacePermissionAssociationsWork_fromPermissionVersion = Lens.lens (\ReplacePermissionAssociationsWork' {fromPermissionVersion} -> fromPermissionVersion) (\s@ReplacePermissionAssociationsWork' {} a -> s {fromPermissionVersion = a} :: ReplacePermissionAssociationsWork)

-- | The unique identifier for the background task associated with one
-- ReplacePermissionAssociations request.
replacePermissionAssociationsWork_id :: Lens.Lens' ReplacePermissionAssociationsWork (Prelude.Maybe Prelude.Text)
replacePermissionAssociationsWork_id = Lens.lens (\ReplacePermissionAssociationsWork' {id} -> id) (\s@ReplacePermissionAssociationsWork' {} a -> s {id = a} :: ReplacePermissionAssociationsWork)

-- | The date and time when the status of this background task was last
-- updated.
replacePermissionAssociationsWork_lastUpdatedTime :: Lens.Lens' ReplacePermissionAssociationsWork (Prelude.Maybe Prelude.UTCTime)
replacePermissionAssociationsWork_lastUpdatedTime = Lens.lens (\ReplacePermissionAssociationsWork' {lastUpdatedTime} -> lastUpdatedTime) (\s@ReplacePermissionAssociationsWork' {} a -> s {lastUpdatedTime = a} :: ReplacePermissionAssociationsWork) Prelude.. Lens.mapping Data._Time

-- | Specifies the current status of the background tasks for the specified
-- ID. The output is one of the following strings:
--
-- -   @IN_PROGRESS@
--
-- -   @COMPLETED@
--
-- -   @FAILED@
replacePermissionAssociationsWork_status :: Lens.Lens' ReplacePermissionAssociationsWork (Prelude.Maybe ReplacePermissionAssociationsWorkStatus)
replacePermissionAssociationsWork_status = Lens.lens (\ReplacePermissionAssociationsWork' {status} -> status) (\s@ReplacePermissionAssociationsWork' {} a -> s {status = a} :: ReplacePermissionAssociationsWork)

-- | Specifies the reason for a @FAILED@ status. This field is present only
-- when there @status@ is @FAILED@.
replacePermissionAssociationsWork_statusMessage :: Lens.Lens' ReplacePermissionAssociationsWork (Prelude.Maybe Prelude.Text)
replacePermissionAssociationsWork_statusMessage = Lens.lens (\ReplacePermissionAssociationsWork' {statusMessage} -> statusMessage) (\s@ReplacePermissionAssociationsWork' {} a -> s {statusMessage = a} :: ReplacePermissionAssociationsWork)

-- | The ARN of the managed permission that this background task is
-- associating with the resource shares in place of the managed permission
-- and version specified in @fromPermissionArn@ and
-- @fromPermissionVersion@.
replacePermissionAssociationsWork_toPermissionArn :: Lens.Lens' ReplacePermissionAssociationsWork (Prelude.Maybe Prelude.Text)
replacePermissionAssociationsWork_toPermissionArn = Lens.lens (\ReplacePermissionAssociationsWork' {toPermissionArn} -> toPermissionArn) (\s@ReplacePermissionAssociationsWork' {} a -> s {toPermissionArn = a} :: ReplacePermissionAssociationsWork)

-- | The version of the managed permission that this background task is
-- associating with the resource shares. This is always the version that is
-- currently the default for this managed permission.
replacePermissionAssociationsWork_toPermissionVersion :: Lens.Lens' ReplacePermissionAssociationsWork (Prelude.Maybe Prelude.Text)
replacePermissionAssociationsWork_toPermissionVersion = Lens.lens (\ReplacePermissionAssociationsWork' {toPermissionVersion} -> toPermissionVersion) (\s@ReplacePermissionAssociationsWork' {} a -> s {toPermissionVersion = a} :: ReplacePermissionAssociationsWork)

instance
  Data.FromJSON
    ReplacePermissionAssociationsWork
  where
  parseJSON =
    Data.withObject
      "ReplacePermissionAssociationsWork"
      ( \x ->
          ReplacePermissionAssociationsWork'
            Prelude.<$> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "fromPermissionArn")
            Prelude.<*> (x Data..:? "fromPermissionVersion")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "lastUpdatedTime")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "statusMessage")
            Prelude.<*> (x Data..:? "toPermissionArn")
            Prelude.<*> (x Data..:? "toPermissionVersion")
      )

instance
  Prelude.Hashable
    ReplacePermissionAssociationsWork
  where
  hashWithSalt
    _salt
    ReplacePermissionAssociationsWork' {..} =
      _salt
        `Prelude.hashWithSalt` creationTime
        `Prelude.hashWithSalt` fromPermissionArn
        `Prelude.hashWithSalt` fromPermissionVersion
        `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` lastUpdatedTime
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` statusMessage
        `Prelude.hashWithSalt` toPermissionArn
        `Prelude.hashWithSalt` toPermissionVersion

instance
  Prelude.NFData
    ReplacePermissionAssociationsWork
  where
  rnf ReplacePermissionAssociationsWork' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf fromPermissionArn
      `Prelude.seq` Prelude.rnf fromPermissionVersion
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf toPermissionArn
      `Prelude.seq` Prelude.rnf toPermissionVersion
