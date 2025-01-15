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
-- Module      : Amazonka.Route53RecoveryReadiness.Types.ReadinessCheckOutput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53RecoveryReadiness.Types.ReadinessCheckOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A readiness check.
--
-- /See:/ 'newReadinessCheckOutput' smart constructor.
data ReadinessCheckOutput = ReadinessCheckOutput'
  { -- | Name of a readiness check.
    readinessCheckName :: Prelude.Maybe Prelude.Text,
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Resource Name (ARN) associated with a readiness check.
    readinessCheckArn :: Prelude.Text,
    -- | Name of the resource set to be checked.
    resourceSet :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReadinessCheckOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'readinessCheckName', 'readinessCheckOutput_readinessCheckName' - Name of a readiness check.
--
-- 'tags', 'readinessCheckOutput_tags' - Undocumented member.
--
-- 'readinessCheckArn', 'readinessCheckOutput_readinessCheckArn' - The Amazon Resource Name (ARN) associated with a readiness check.
--
-- 'resourceSet', 'readinessCheckOutput_resourceSet' - Name of the resource set to be checked.
newReadinessCheckOutput ::
  -- | 'readinessCheckArn'
  Prelude.Text ->
  -- | 'resourceSet'
  Prelude.Text ->
  ReadinessCheckOutput
newReadinessCheckOutput
  pReadinessCheckArn_
  pResourceSet_ =
    ReadinessCheckOutput'
      { readinessCheckName =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        readinessCheckArn = pReadinessCheckArn_,
        resourceSet = pResourceSet_
      }

-- | Name of a readiness check.
readinessCheckOutput_readinessCheckName :: Lens.Lens' ReadinessCheckOutput (Prelude.Maybe Prelude.Text)
readinessCheckOutput_readinessCheckName = Lens.lens (\ReadinessCheckOutput' {readinessCheckName} -> readinessCheckName) (\s@ReadinessCheckOutput' {} a -> s {readinessCheckName = a} :: ReadinessCheckOutput)

-- | Undocumented member.
readinessCheckOutput_tags :: Lens.Lens' ReadinessCheckOutput (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
readinessCheckOutput_tags = Lens.lens (\ReadinessCheckOutput' {tags} -> tags) (\s@ReadinessCheckOutput' {} a -> s {tags = a} :: ReadinessCheckOutput) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) associated with a readiness check.
readinessCheckOutput_readinessCheckArn :: Lens.Lens' ReadinessCheckOutput Prelude.Text
readinessCheckOutput_readinessCheckArn = Lens.lens (\ReadinessCheckOutput' {readinessCheckArn} -> readinessCheckArn) (\s@ReadinessCheckOutput' {} a -> s {readinessCheckArn = a} :: ReadinessCheckOutput)

-- | Name of the resource set to be checked.
readinessCheckOutput_resourceSet :: Lens.Lens' ReadinessCheckOutput Prelude.Text
readinessCheckOutput_resourceSet = Lens.lens (\ReadinessCheckOutput' {resourceSet} -> resourceSet) (\s@ReadinessCheckOutput' {} a -> s {resourceSet = a} :: ReadinessCheckOutput)

instance Data.FromJSON ReadinessCheckOutput where
  parseJSON =
    Data.withObject
      "ReadinessCheckOutput"
      ( \x ->
          ReadinessCheckOutput'
            Prelude.<$> (x Data..:? "readinessCheckName")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "readinessCheckArn")
            Prelude.<*> (x Data..: "resourceSet")
      )

instance Prelude.Hashable ReadinessCheckOutput where
  hashWithSalt _salt ReadinessCheckOutput' {..} =
    _salt
      `Prelude.hashWithSalt` readinessCheckName
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` readinessCheckArn
      `Prelude.hashWithSalt` resourceSet

instance Prelude.NFData ReadinessCheckOutput where
  rnf ReadinessCheckOutput' {..} =
    Prelude.rnf readinessCheckName `Prelude.seq`
      Prelude.rnf tags `Prelude.seq`
        Prelude.rnf readinessCheckArn `Prelude.seq`
          Prelude.rnf resourceSet
