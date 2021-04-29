{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.Types.ContextSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ContextSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.ContextSource

-- | Lists a summary of the properties of a context. A context provides a
-- logical grouping of other entities.
--
-- /See:/ 'newContextSummary' smart constructor.
data ContextSummary = ContextSummary'
  { -- | The type of the context.
    contextType :: Prelude.Maybe Prelude.Text,
    -- | When the context was created.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The name of the context.
    contextName :: Prelude.Maybe Prelude.Text,
    -- | The source of the context.
    source :: Prelude.Maybe ContextSource,
    -- | When the context was last modified.
    lastModifiedTime :: Prelude.Maybe Prelude.POSIX,
    -- | The Amazon Resource Name (ARN) of the context.
    contextArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ContextSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contextType', 'contextSummary_contextType' - The type of the context.
--
-- 'creationTime', 'contextSummary_creationTime' - When the context was created.
--
-- 'contextName', 'contextSummary_contextName' - The name of the context.
--
-- 'source', 'contextSummary_source' - The source of the context.
--
-- 'lastModifiedTime', 'contextSummary_lastModifiedTime' - When the context was last modified.
--
-- 'contextArn', 'contextSummary_contextArn' - The Amazon Resource Name (ARN) of the context.
newContextSummary ::
  ContextSummary
newContextSummary =
  ContextSummary'
    { contextType = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      contextName = Prelude.Nothing,
      source = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      contextArn = Prelude.Nothing
    }

-- | The type of the context.
contextSummary_contextType :: Lens.Lens' ContextSummary (Prelude.Maybe Prelude.Text)
contextSummary_contextType = Lens.lens (\ContextSummary' {contextType} -> contextType) (\s@ContextSummary' {} a -> s {contextType = a} :: ContextSummary)

-- | When the context was created.
contextSummary_creationTime :: Lens.Lens' ContextSummary (Prelude.Maybe Prelude.UTCTime)
contextSummary_creationTime = Lens.lens (\ContextSummary' {creationTime} -> creationTime) (\s@ContextSummary' {} a -> s {creationTime = a} :: ContextSummary) Prelude.. Lens.mapping Prelude._Time

-- | The name of the context.
contextSummary_contextName :: Lens.Lens' ContextSummary (Prelude.Maybe Prelude.Text)
contextSummary_contextName = Lens.lens (\ContextSummary' {contextName} -> contextName) (\s@ContextSummary' {} a -> s {contextName = a} :: ContextSummary)

-- | The source of the context.
contextSummary_source :: Lens.Lens' ContextSummary (Prelude.Maybe ContextSource)
contextSummary_source = Lens.lens (\ContextSummary' {source} -> source) (\s@ContextSummary' {} a -> s {source = a} :: ContextSummary)

-- | When the context was last modified.
contextSummary_lastModifiedTime :: Lens.Lens' ContextSummary (Prelude.Maybe Prelude.UTCTime)
contextSummary_lastModifiedTime = Lens.lens (\ContextSummary' {lastModifiedTime} -> lastModifiedTime) (\s@ContextSummary' {} a -> s {lastModifiedTime = a} :: ContextSummary) Prelude.. Lens.mapping Prelude._Time

-- | The Amazon Resource Name (ARN) of the context.
contextSummary_contextArn :: Lens.Lens' ContextSummary (Prelude.Maybe Prelude.Text)
contextSummary_contextArn = Lens.lens (\ContextSummary' {contextArn} -> contextArn) (\s@ContextSummary' {} a -> s {contextArn = a} :: ContextSummary)

instance Prelude.FromJSON ContextSummary where
  parseJSON =
    Prelude.withObject
      "ContextSummary"
      ( \x ->
          ContextSummary'
            Prelude.<$> (x Prelude..:? "ContextType")
            Prelude.<*> (x Prelude..:? "CreationTime")
            Prelude.<*> (x Prelude..:? "ContextName")
            Prelude.<*> (x Prelude..:? "Source")
            Prelude.<*> (x Prelude..:? "LastModifiedTime")
            Prelude.<*> (x Prelude..:? "ContextArn")
      )

instance Prelude.Hashable ContextSummary

instance Prelude.NFData ContextSummary
