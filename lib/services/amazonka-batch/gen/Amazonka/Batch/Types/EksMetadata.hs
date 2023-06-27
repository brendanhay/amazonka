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
-- Module      : Amazonka.Batch.Types.EksMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.EksMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes and uniquely identifies Kubernetes resources. For example, the
-- compute environment that a pod runs in or the @jobID@ for a job running
-- in the pod. For more information, see
-- <https://kubernetes.io/docs/concepts/overview/working-with-objects/kubernetes-objects/ Understanding Kubernetes Objects>
-- in the /Kubernetes documentation/.
--
-- /See:/ 'newEksMetadata' smart constructor.
data EksMetadata = EksMetadata'
  { -- | Key-value pairs used to identify, sort, and organize cube resources. Can
    -- contain up to 63 uppercase letters, lowercase letters, numbers, hyphens
    -- (-), and underscores (_). Labels can be added or modified at any time.
    -- Each resource can have multiple labels, but each key must be unique for
    -- a given object.
    labels :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EksMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'labels', 'eksMetadata_labels' - Key-value pairs used to identify, sort, and organize cube resources. Can
-- contain up to 63 uppercase letters, lowercase letters, numbers, hyphens
-- (-), and underscores (_). Labels can be added or modified at any time.
-- Each resource can have multiple labels, but each key must be unique for
-- a given object.
newEksMetadata ::
  EksMetadata
newEksMetadata =
  EksMetadata' {labels = Prelude.Nothing}

-- | Key-value pairs used to identify, sort, and organize cube resources. Can
-- contain up to 63 uppercase letters, lowercase letters, numbers, hyphens
-- (-), and underscores (_). Labels can be added or modified at any time.
-- Each resource can have multiple labels, but each key must be unique for
-- a given object.
eksMetadata_labels :: Lens.Lens' EksMetadata (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
eksMetadata_labels = Lens.lens (\EksMetadata' {labels} -> labels) (\s@EksMetadata' {} a -> s {labels = a} :: EksMetadata) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON EksMetadata where
  parseJSON =
    Data.withObject
      "EksMetadata"
      ( \x ->
          EksMetadata'
            Prelude.<$> (x Data..:? "labels" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable EksMetadata where
  hashWithSalt _salt EksMetadata' {..} =
    _salt `Prelude.hashWithSalt` labels

instance Prelude.NFData EksMetadata where
  rnf EksMetadata' {..} = Prelude.rnf labels

instance Data.ToJSON EksMetadata where
  toJSON EksMetadata' {..} =
    Data.object
      ( Prelude.catMaybes
          [("labels" Data..=) Prelude.<$> labels]
      )
