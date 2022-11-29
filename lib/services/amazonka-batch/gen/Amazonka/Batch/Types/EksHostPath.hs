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
-- Module      : Amazonka.Batch.Types.EksHostPath
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.EksHostPath where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies the configuration of a Kubernetes @hostPath@ volume. A
-- @hostPath@ volume mounts an existing file or directory from the host
-- node\'s filesystem into your pod. For more information, see
-- <https://kubernetes.io/docs/concepts/storage/volumes/#hostpath hostPath>
-- in the /Kubernetes documentation/.
--
-- /See:/ 'newEksHostPath' smart constructor.
data EksHostPath = EksHostPath'
  { -- | The path of the file or directory on the host to mount into containers
    -- on the pod.
    path :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EksHostPath' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'path', 'eksHostPath_path' - The path of the file or directory on the host to mount into containers
-- on the pod.
newEksHostPath ::
  EksHostPath
newEksHostPath = EksHostPath' {path = Prelude.Nothing}

-- | The path of the file or directory on the host to mount into containers
-- on the pod.
eksHostPath_path :: Lens.Lens' EksHostPath (Prelude.Maybe Prelude.Text)
eksHostPath_path = Lens.lens (\EksHostPath' {path} -> path) (\s@EksHostPath' {} a -> s {path = a} :: EksHostPath)

instance Core.FromJSON EksHostPath where
  parseJSON =
    Core.withObject
      "EksHostPath"
      (\x -> EksHostPath' Prelude.<$> (x Core..:? "path"))

instance Prelude.Hashable EksHostPath where
  hashWithSalt _salt EksHostPath' {..} =
    _salt `Prelude.hashWithSalt` path

instance Prelude.NFData EksHostPath where
  rnf EksHostPath' {..} = Prelude.rnf path

instance Core.ToJSON EksHostPath where
  toJSON EksHostPath' {..} =
    Core.object
      ( Prelude.catMaybes
          [("path" Core..=) Prelude.<$> path]
      )
