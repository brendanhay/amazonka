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
-- Module      : Amazonka.AMP.Types.WorkspaceDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AMP.Types.WorkspaceDescription where

import Amazonka.AMP.Types.WorkspaceStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the properties of a workspace.
--
-- /See:/ 'newWorkspaceDescription' smart constructor.
data WorkspaceDescription = WorkspaceDescription'
  { -- | Alias of this workspace.
    alias :: Prelude.Maybe Prelude.Text,
    -- | Prometheus endpoint URI.
    prometheusEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The tags of this workspace.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Resource Name (ARN) of this workspace.
    arn :: Prelude.Text,
    -- | The time when the workspace was created.
    createdAt :: Data.POSIX,
    -- | The status of this workspace.
    status :: WorkspaceStatus,
    -- | Unique string identifying this workspace.
    workspaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkspaceDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alias', 'workspaceDescription_alias' - Alias of this workspace.
--
-- 'prometheusEndpoint', 'workspaceDescription_prometheusEndpoint' - Prometheus endpoint URI.
--
-- 'tags', 'workspaceDescription_tags' - The tags of this workspace.
--
-- 'arn', 'workspaceDescription_arn' - The Amazon Resource Name (ARN) of this workspace.
--
-- 'createdAt', 'workspaceDescription_createdAt' - The time when the workspace was created.
--
-- 'status', 'workspaceDescription_status' - The status of this workspace.
--
-- 'workspaceId', 'workspaceDescription_workspaceId' - Unique string identifying this workspace.
newWorkspaceDescription ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'status'
  WorkspaceStatus ->
  -- | 'workspaceId'
  Prelude.Text ->
  WorkspaceDescription
newWorkspaceDescription
  pArn_
  pCreatedAt_
  pStatus_
  pWorkspaceId_ =
    WorkspaceDescription'
      { alias = Prelude.Nothing,
        prometheusEndpoint = Prelude.Nothing,
        tags = Prelude.Nothing,
        arn = pArn_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        status = pStatus_,
        workspaceId = pWorkspaceId_
      }

-- | Alias of this workspace.
workspaceDescription_alias :: Lens.Lens' WorkspaceDescription (Prelude.Maybe Prelude.Text)
workspaceDescription_alias = Lens.lens (\WorkspaceDescription' {alias} -> alias) (\s@WorkspaceDescription' {} a -> s {alias = a} :: WorkspaceDescription)

-- | Prometheus endpoint URI.
workspaceDescription_prometheusEndpoint :: Lens.Lens' WorkspaceDescription (Prelude.Maybe Prelude.Text)
workspaceDescription_prometheusEndpoint = Lens.lens (\WorkspaceDescription' {prometheusEndpoint} -> prometheusEndpoint) (\s@WorkspaceDescription' {} a -> s {prometheusEndpoint = a} :: WorkspaceDescription)

-- | The tags of this workspace.
workspaceDescription_tags :: Lens.Lens' WorkspaceDescription (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
workspaceDescription_tags = Lens.lens (\WorkspaceDescription' {tags} -> tags) (\s@WorkspaceDescription' {} a -> s {tags = a} :: WorkspaceDescription) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of this workspace.
workspaceDescription_arn :: Lens.Lens' WorkspaceDescription Prelude.Text
workspaceDescription_arn = Lens.lens (\WorkspaceDescription' {arn} -> arn) (\s@WorkspaceDescription' {} a -> s {arn = a} :: WorkspaceDescription)

-- | The time when the workspace was created.
workspaceDescription_createdAt :: Lens.Lens' WorkspaceDescription Prelude.UTCTime
workspaceDescription_createdAt = Lens.lens (\WorkspaceDescription' {createdAt} -> createdAt) (\s@WorkspaceDescription' {} a -> s {createdAt = a} :: WorkspaceDescription) Prelude.. Data._Time

-- | The status of this workspace.
workspaceDescription_status :: Lens.Lens' WorkspaceDescription WorkspaceStatus
workspaceDescription_status = Lens.lens (\WorkspaceDescription' {status} -> status) (\s@WorkspaceDescription' {} a -> s {status = a} :: WorkspaceDescription)

-- | Unique string identifying this workspace.
workspaceDescription_workspaceId :: Lens.Lens' WorkspaceDescription Prelude.Text
workspaceDescription_workspaceId = Lens.lens (\WorkspaceDescription' {workspaceId} -> workspaceId) (\s@WorkspaceDescription' {} a -> s {workspaceId = a} :: WorkspaceDescription)

instance Data.FromJSON WorkspaceDescription where
  parseJSON =
    Data.withObject
      "WorkspaceDescription"
      ( \x ->
          WorkspaceDescription'
            Prelude.<$> (x Data..:? "alias")
            Prelude.<*> (x Data..:? "prometheusEndpoint")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..: "workspaceId")
      )

instance Prelude.Hashable WorkspaceDescription where
  hashWithSalt _salt WorkspaceDescription' {..} =
    _salt
      `Prelude.hashWithSalt` alias
      `Prelude.hashWithSalt` prometheusEndpoint
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` workspaceId

instance Prelude.NFData WorkspaceDescription where
  rnf WorkspaceDescription' {..} =
    Prelude.rnf alias
      `Prelude.seq` Prelude.rnf prometheusEndpoint
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf workspaceId
