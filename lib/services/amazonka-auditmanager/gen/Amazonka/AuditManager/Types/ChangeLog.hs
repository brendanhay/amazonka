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
-- Module      : Amazonka.AuditManager.Types.ChangeLog
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.ChangeLog where

import Amazonka.AuditManager.Types.ActionEnum
import Amazonka.AuditManager.Types.ObjectTypeEnum
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The record of a change within Audit Manager. For example, this could be
-- the status change of an assessment or the delegation of a control set.
--
-- /See:/ 'newChangeLog' smart constructor.
data ChangeLog = ChangeLog'
  { -- | The action that was performed.
    action :: Prelude.Maybe ActionEnum,
    -- | The time when the action was performed and the changelog record was
    -- created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The user or role that performed the action.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | The name of the object that changed. This could be the name of an
    -- assessment, control, or control set.
    objectName :: Prelude.Maybe Prelude.Text,
    -- | The object that was changed, such as an assessment, control, or control
    -- set.
    objectType :: Prelude.Maybe ObjectTypeEnum
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChangeLog' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'changeLog_action' - The action that was performed.
--
-- 'createdAt', 'changeLog_createdAt' - The time when the action was performed and the changelog record was
-- created.
--
-- 'createdBy', 'changeLog_createdBy' - The user or role that performed the action.
--
-- 'objectName', 'changeLog_objectName' - The name of the object that changed. This could be the name of an
-- assessment, control, or control set.
--
-- 'objectType', 'changeLog_objectType' - The object that was changed, such as an assessment, control, or control
-- set.
newChangeLog ::
  ChangeLog
newChangeLog =
  ChangeLog'
    { action = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      objectName = Prelude.Nothing,
      objectType = Prelude.Nothing
    }

-- | The action that was performed.
changeLog_action :: Lens.Lens' ChangeLog (Prelude.Maybe ActionEnum)
changeLog_action = Lens.lens (\ChangeLog' {action} -> action) (\s@ChangeLog' {} a -> s {action = a} :: ChangeLog)

-- | The time when the action was performed and the changelog record was
-- created.
changeLog_createdAt :: Lens.Lens' ChangeLog (Prelude.Maybe Prelude.UTCTime)
changeLog_createdAt = Lens.lens (\ChangeLog' {createdAt} -> createdAt) (\s@ChangeLog' {} a -> s {createdAt = a} :: ChangeLog) Prelude.. Lens.mapping Data._Time

-- | The user or role that performed the action.
changeLog_createdBy :: Lens.Lens' ChangeLog (Prelude.Maybe Prelude.Text)
changeLog_createdBy = Lens.lens (\ChangeLog' {createdBy} -> createdBy) (\s@ChangeLog' {} a -> s {createdBy = a} :: ChangeLog)

-- | The name of the object that changed. This could be the name of an
-- assessment, control, or control set.
changeLog_objectName :: Lens.Lens' ChangeLog (Prelude.Maybe Prelude.Text)
changeLog_objectName = Lens.lens (\ChangeLog' {objectName} -> objectName) (\s@ChangeLog' {} a -> s {objectName = a} :: ChangeLog)

-- | The object that was changed, such as an assessment, control, or control
-- set.
changeLog_objectType :: Lens.Lens' ChangeLog (Prelude.Maybe ObjectTypeEnum)
changeLog_objectType = Lens.lens (\ChangeLog' {objectType} -> objectType) (\s@ChangeLog' {} a -> s {objectType = a} :: ChangeLog)

instance Data.FromJSON ChangeLog where
  parseJSON =
    Data.withObject
      "ChangeLog"
      ( \x ->
          ChangeLog'
            Prelude.<$> (x Data..:? "action")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "createdBy")
            Prelude.<*> (x Data..:? "objectName")
            Prelude.<*> (x Data..:? "objectType")
      )

instance Prelude.Hashable ChangeLog where
  hashWithSalt _salt ChangeLog' {..} =
    _salt
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` objectName
      `Prelude.hashWithSalt` objectType

instance Prelude.NFData ChangeLog where
  rnf ChangeLog' {..} =
    Prelude.rnf action
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf objectName
      `Prelude.seq` Prelude.rnf objectType
