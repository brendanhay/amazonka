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
-- Module      : Amazonka.EMRContainers.Types.EksInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRContainers.Types.EksInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The information about the EKS cluster.
--
-- /See:/ 'newEksInfo' smart constructor.
data EksInfo = EksInfo'
  { -- | The namespaces of the EKS cluster.
    namespace :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EksInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'namespace', 'eksInfo_namespace' - The namespaces of the EKS cluster.
newEksInfo ::
  EksInfo
newEksInfo = EksInfo' {namespace = Prelude.Nothing}

-- | The namespaces of the EKS cluster.
eksInfo_namespace :: Lens.Lens' EksInfo (Prelude.Maybe Prelude.Text)
eksInfo_namespace = Lens.lens (\EksInfo' {namespace} -> namespace) (\s@EksInfo' {} a -> s {namespace = a} :: EksInfo)

instance Core.FromJSON EksInfo where
  parseJSON =
    Core.withObject
      "EksInfo"
      ( \x ->
          EksInfo' Prelude.<$> (x Core..:? "namespace")
      )

instance Prelude.Hashable EksInfo where
  hashWithSalt _salt EksInfo' {..} =
    _salt `Prelude.hashWithSalt` namespace

instance Prelude.NFData EksInfo where
  rnf EksInfo' {..} = Prelude.rnf namespace

instance Core.ToJSON EksInfo where
  toJSON EksInfo' {..} =
    Core.object
      ( Prelude.catMaybes
          [("namespace" Core..=) Prelude.<$> namespace]
      )
