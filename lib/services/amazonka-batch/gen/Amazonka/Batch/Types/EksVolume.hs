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
-- Module      : Amazonka.Batch.Types.EksVolume
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.EksVolume where

import Amazonka.Batch.Types.EksEmptyDir
import Amazonka.Batch.Types.EksHostPath
import Amazonka.Batch.Types.EksSecret
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies an Amazon EKS volume for a job definition.
--
-- /See:/ 'newEksVolume' smart constructor.
data EksVolume = EksVolume'
  { -- | Specifies the configuration of a Kubernetes @hostPath@ volume. For more
    -- information, see
    -- <https://kubernetes.io/docs/concepts/storage/volumes/#hostpath hostPath>
    -- in the /Kubernetes documentation/.
    hostPath :: Prelude.Maybe EksHostPath,
    -- | Specifies the configuration of a Kubernetes @emptyDir@ volume. For more
    -- information, see
    -- <https://kubernetes.io/docs/concepts/storage/volumes/#emptydir emptyDir>
    -- in the /Kubernetes documentation/.
    emptyDir :: Prelude.Maybe EksEmptyDir,
    -- | Specifies the configuration of a Kubernetes @secret@ volume. For more
    -- information, see
    -- <https://kubernetes.io/docs/concepts/storage/volumes/#secret secret> in
    -- the /Kubernetes documentation/.
    secret :: Prelude.Maybe EksSecret,
    -- | The name of the volume. The name must be allowed as a DNS subdomain
    -- name. For more information, see
    -- <https://kubernetes.io/docs/concepts/overview/working-with-objects/names/#dns-subdomain-names DNS subdomain names>
    -- in the /Kubernetes documentation/.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EksVolume' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostPath', 'eksVolume_hostPath' - Specifies the configuration of a Kubernetes @hostPath@ volume. For more
-- information, see
-- <https://kubernetes.io/docs/concepts/storage/volumes/#hostpath hostPath>
-- in the /Kubernetes documentation/.
--
-- 'emptyDir', 'eksVolume_emptyDir' - Specifies the configuration of a Kubernetes @emptyDir@ volume. For more
-- information, see
-- <https://kubernetes.io/docs/concepts/storage/volumes/#emptydir emptyDir>
-- in the /Kubernetes documentation/.
--
-- 'secret', 'eksVolume_secret' - Specifies the configuration of a Kubernetes @secret@ volume. For more
-- information, see
-- <https://kubernetes.io/docs/concepts/storage/volumes/#secret secret> in
-- the /Kubernetes documentation/.
--
-- 'name', 'eksVolume_name' - The name of the volume. The name must be allowed as a DNS subdomain
-- name. For more information, see
-- <https://kubernetes.io/docs/concepts/overview/working-with-objects/names/#dns-subdomain-names DNS subdomain names>
-- in the /Kubernetes documentation/.
newEksVolume ::
  -- | 'name'
  Prelude.Text ->
  EksVolume
newEksVolume pName_ =
  EksVolume'
    { hostPath = Prelude.Nothing,
      emptyDir = Prelude.Nothing,
      secret = Prelude.Nothing,
      name = pName_
    }

-- | Specifies the configuration of a Kubernetes @hostPath@ volume. For more
-- information, see
-- <https://kubernetes.io/docs/concepts/storage/volumes/#hostpath hostPath>
-- in the /Kubernetes documentation/.
eksVolume_hostPath :: Lens.Lens' EksVolume (Prelude.Maybe EksHostPath)
eksVolume_hostPath = Lens.lens (\EksVolume' {hostPath} -> hostPath) (\s@EksVolume' {} a -> s {hostPath = a} :: EksVolume)

-- | Specifies the configuration of a Kubernetes @emptyDir@ volume. For more
-- information, see
-- <https://kubernetes.io/docs/concepts/storage/volumes/#emptydir emptyDir>
-- in the /Kubernetes documentation/.
eksVolume_emptyDir :: Lens.Lens' EksVolume (Prelude.Maybe EksEmptyDir)
eksVolume_emptyDir = Lens.lens (\EksVolume' {emptyDir} -> emptyDir) (\s@EksVolume' {} a -> s {emptyDir = a} :: EksVolume)

-- | Specifies the configuration of a Kubernetes @secret@ volume. For more
-- information, see
-- <https://kubernetes.io/docs/concepts/storage/volumes/#secret secret> in
-- the /Kubernetes documentation/.
eksVolume_secret :: Lens.Lens' EksVolume (Prelude.Maybe EksSecret)
eksVolume_secret = Lens.lens (\EksVolume' {secret} -> secret) (\s@EksVolume' {} a -> s {secret = a} :: EksVolume)

-- | The name of the volume. The name must be allowed as a DNS subdomain
-- name. For more information, see
-- <https://kubernetes.io/docs/concepts/overview/working-with-objects/names/#dns-subdomain-names DNS subdomain names>
-- in the /Kubernetes documentation/.
eksVolume_name :: Lens.Lens' EksVolume Prelude.Text
eksVolume_name = Lens.lens (\EksVolume' {name} -> name) (\s@EksVolume' {} a -> s {name = a} :: EksVolume)

instance Core.FromJSON EksVolume where
  parseJSON =
    Core.withObject
      "EksVolume"
      ( \x ->
          EksVolume'
            Prelude.<$> (x Core..:? "hostPath")
            Prelude.<*> (x Core..:? "emptyDir")
            Prelude.<*> (x Core..:? "secret")
            Prelude.<*> (x Core..: "name")
      )

instance Prelude.Hashable EksVolume where
  hashWithSalt _salt EksVolume' {..} =
    _salt `Prelude.hashWithSalt` hostPath
      `Prelude.hashWithSalt` emptyDir
      `Prelude.hashWithSalt` secret
      `Prelude.hashWithSalt` name

instance Prelude.NFData EksVolume where
  rnf EksVolume' {..} =
    Prelude.rnf hostPath
      `Prelude.seq` Prelude.rnf emptyDir
      `Prelude.seq` Prelude.rnf secret
      `Prelude.seq` Prelude.rnf name

instance Core.ToJSON EksVolume where
  toJSON EksVolume' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("hostPath" Core..=) Prelude.<$> hostPath,
            ("emptyDir" Core..=) Prelude.<$> emptyDir,
            ("secret" Core..=) Prelude.<$> secret,
            Prelude.Just ("name" Core..= name)
          ]
      )
