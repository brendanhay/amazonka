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
-- Module      : Amazonka.SageMaker.Types.ContextSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ContextSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ContextSource

-- | Lists a summary of the properties of a context. A context provides a
-- logical grouping of other entities.
--
-- /See:/ 'newContextSummary' smart constructor.
data ContextSummary = ContextSummary'
  { -- | The Amazon Resource Name (ARN) of the context.
    contextArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the context.
    contextName :: Prelude.Maybe Prelude.Text,
    -- | The type of the context.
    contextType :: Prelude.Maybe Prelude.Text,
    -- | When the context was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | When the context was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The source of the context.
    source :: Prelude.Maybe ContextSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContextSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contextArn', 'contextSummary_contextArn' - The Amazon Resource Name (ARN) of the context.
--
-- 'contextName', 'contextSummary_contextName' - The name of the context.
--
-- 'contextType', 'contextSummary_contextType' - The type of the context.
--
-- 'creationTime', 'contextSummary_creationTime' - When the context was created.
--
-- 'lastModifiedTime', 'contextSummary_lastModifiedTime' - When the context was last modified.
--
-- 'source', 'contextSummary_source' - The source of the context.
newContextSummary ::
  ContextSummary
newContextSummary =
  ContextSummary'
    { contextArn = Prelude.Nothing,
      contextName = Prelude.Nothing,
      contextType = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      source = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the context.
contextSummary_contextArn :: Lens.Lens' ContextSummary (Prelude.Maybe Prelude.Text)
contextSummary_contextArn = Lens.lens (\ContextSummary' {contextArn} -> contextArn) (\s@ContextSummary' {} a -> s {contextArn = a} :: ContextSummary)

-- | The name of the context.
contextSummary_contextName :: Lens.Lens' ContextSummary (Prelude.Maybe Prelude.Text)
contextSummary_contextName = Lens.lens (\ContextSummary' {contextName} -> contextName) (\s@ContextSummary' {} a -> s {contextName = a} :: ContextSummary)

-- | The type of the context.
contextSummary_contextType :: Lens.Lens' ContextSummary (Prelude.Maybe Prelude.Text)
contextSummary_contextType = Lens.lens (\ContextSummary' {contextType} -> contextType) (\s@ContextSummary' {} a -> s {contextType = a} :: ContextSummary)

-- | When the context was created.
contextSummary_creationTime :: Lens.Lens' ContextSummary (Prelude.Maybe Prelude.UTCTime)
contextSummary_creationTime = Lens.lens (\ContextSummary' {creationTime} -> creationTime) (\s@ContextSummary' {} a -> s {creationTime = a} :: ContextSummary) Prelude.. Lens.mapping Data._Time

-- | When the context was last modified.
contextSummary_lastModifiedTime :: Lens.Lens' ContextSummary (Prelude.Maybe Prelude.UTCTime)
contextSummary_lastModifiedTime = Lens.lens (\ContextSummary' {lastModifiedTime} -> lastModifiedTime) (\s@ContextSummary' {} a -> s {lastModifiedTime = a} :: ContextSummary) Prelude.. Lens.mapping Data._Time

-- | The source of the context.
contextSummary_source :: Lens.Lens' ContextSummary (Prelude.Maybe ContextSource)
contextSummary_source = Lens.lens (\ContextSummary' {source} -> source) (\s@ContextSummary' {} a -> s {source = a} :: ContextSummary)

instance Data.FromJSON ContextSummary where
  parseJSON =
    Data.withObject
      "ContextSummary"
      ( \x ->
          ContextSummary'
            Prelude.<$> (x Data..:? "ContextArn")
            Prelude.<*> (x Data..:? "ContextName")
            Prelude.<*> (x Data..:? "ContextType")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "Source")
      )

instance Prelude.Hashable ContextSummary where
  hashWithSalt _salt ContextSummary' {..} =
    _salt
      `Prelude.hashWithSalt` contextArn
      `Prelude.hashWithSalt` contextName
      `Prelude.hashWithSalt` contextType
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` source

instance Prelude.NFData ContextSummary where
  rnf ContextSummary' {..} =
    Prelude.rnf contextArn `Prelude.seq`
      Prelude.rnf contextName `Prelude.seq`
        Prelude.rnf contextType `Prelude.seq`
          Prelude.rnf creationTime `Prelude.seq`
            Prelude.rnf lastModifiedTime `Prelude.seq`
              Prelude.rnf source
