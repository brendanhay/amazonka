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
-- Module      : Amazonka.Batch.Types.EksEmptyDir
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.EksEmptyDir where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the configuration of a Kubernetes @emptyDir@ volume. An
-- @emptyDir@ volume is first created when a pod is assigned to a node. It
-- exists as long as that pod is running on that node. The @emptyDir@
-- volume is initially empty. All containers in the pod can read and write
-- the files in the @emptyDir@ volume. However, the @emptyDir@ volume can
-- be mounted at the same or different paths in each container. When a pod
-- is removed from a node for any reason, the data in the @emptyDir@ is
-- deleted permanently. For more information, see
-- <https://kubernetes.io/docs/concepts/storage/volumes/#emptydir emptyDir>
-- in the /Kubernetes documentation/.
--
-- /See:/ 'newEksEmptyDir' smart constructor.
data EksEmptyDir = EksEmptyDir'
  { -- | The medium to store the volume. The default value is an empty string,
    -- which uses the storage of the node.
    --
    -- [\"\"]
    --     __(Default)__ Use the disk storage of the node.
    --
    -- [\"Memory\"]
    --     Use the @tmpfs@ volume that\'s backed by the RAM of the node.
    --     Contents of the volume are lost when the node reboots, and any
    --     storage on the volume counts against the container\'s memory limit.
    medium :: Prelude.Maybe Prelude.Text,
    -- | The maximum size of the volume. By default, there\'s no maximum size
    -- defined.
    sizeLimit :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EksEmptyDir' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'medium', 'eksEmptyDir_medium' - The medium to store the volume. The default value is an empty string,
-- which uses the storage of the node.
--
-- [\"\"]
--     __(Default)__ Use the disk storage of the node.
--
-- [\"Memory\"]
--     Use the @tmpfs@ volume that\'s backed by the RAM of the node.
--     Contents of the volume are lost when the node reboots, and any
--     storage on the volume counts against the container\'s memory limit.
--
-- 'sizeLimit', 'eksEmptyDir_sizeLimit' - The maximum size of the volume. By default, there\'s no maximum size
-- defined.
newEksEmptyDir ::
  EksEmptyDir
newEksEmptyDir =
  EksEmptyDir'
    { medium = Prelude.Nothing,
      sizeLimit = Prelude.Nothing
    }

-- | The medium to store the volume. The default value is an empty string,
-- which uses the storage of the node.
--
-- [\"\"]
--     __(Default)__ Use the disk storage of the node.
--
-- [\"Memory\"]
--     Use the @tmpfs@ volume that\'s backed by the RAM of the node.
--     Contents of the volume are lost when the node reboots, and any
--     storage on the volume counts against the container\'s memory limit.
eksEmptyDir_medium :: Lens.Lens' EksEmptyDir (Prelude.Maybe Prelude.Text)
eksEmptyDir_medium = Lens.lens (\EksEmptyDir' {medium} -> medium) (\s@EksEmptyDir' {} a -> s {medium = a} :: EksEmptyDir)

-- | The maximum size of the volume. By default, there\'s no maximum size
-- defined.
eksEmptyDir_sizeLimit :: Lens.Lens' EksEmptyDir (Prelude.Maybe Prelude.Text)
eksEmptyDir_sizeLimit = Lens.lens (\EksEmptyDir' {sizeLimit} -> sizeLimit) (\s@EksEmptyDir' {} a -> s {sizeLimit = a} :: EksEmptyDir)

instance Data.FromJSON EksEmptyDir where
  parseJSON =
    Data.withObject
      "EksEmptyDir"
      ( \x ->
          EksEmptyDir'
            Prelude.<$> (x Data..:? "medium")
            Prelude.<*> (x Data..:? "sizeLimit")
      )

instance Prelude.Hashable EksEmptyDir where
  hashWithSalt _salt EksEmptyDir' {..} =
    _salt
      `Prelude.hashWithSalt` medium
      `Prelude.hashWithSalt` sizeLimit

instance Prelude.NFData EksEmptyDir where
  rnf EksEmptyDir' {..} =
    Prelude.rnf medium `Prelude.seq`
      Prelude.rnf sizeLimit

instance Data.ToJSON EksEmptyDir where
  toJSON EksEmptyDir' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("medium" Data..=) Prelude.<$> medium,
            ("sizeLimit" Data..=) Prelude.<$> sizeLimit
          ]
      )
