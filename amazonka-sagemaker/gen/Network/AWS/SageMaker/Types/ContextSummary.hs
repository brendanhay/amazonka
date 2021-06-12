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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.ContextSource

-- | Lists a summary of the properties of a context. A context provides a
-- logical grouping of other entities.
--
-- /See:/ 'newContextSummary' smart constructor.
data ContextSummary = ContextSummary'
  { -- | The type of the context.
    contextType :: Core.Maybe Core.Text,
    -- | When the context was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The name of the context.
    contextName :: Core.Maybe Core.Text,
    -- | The source of the context.
    source :: Core.Maybe ContextSource,
    -- | When the context was last modified.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the context.
    contextArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { contextType = Core.Nothing,
      creationTime = Core.Nothing,
      contextName = Core.Nothing,
      source = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      contextArn = Core.Nothing
    }

-- | The type of the context.
contextSummary_contextType :: Lens.Lens' ContextSummary (Core.Maybe Core.Text)
contextSummary_contextType = Lens.lens (\ContextSummary' {contextType} -> contextType) (\s@ContextSummary' {} a -> s {contextType = a} :: ContextSummary)

-- | When the context was created.
contextSummary_creationTime :: Lens.Lens' ContextSummary (Core.Maybe Core.UTCTime)
contextSummary_creationTime = Lens.lens (\ContextSummary' {creationTime} -> creationTime) (\s@ContextSummary' {} a -> s {creationTime = a} :: ContextSummary) Core.. Lens.mapping Core._Time

-- | The name of the context.
contextSummary_contextName :: Lens.Lens' ContextSummary (Core.Maybe Core.Text)
contextSummary_contextName = Lens.lens (\ContextSummary' {contextName} -> contextName) (\s@ContextSummary' {} a -> s {contextName = a} :: ContextSummary)

-- | The source of the context.
contextSummary_source :: Lens.Lens' ContextSummary (Core.Maybe ContextSource)
contextSummary_source = Lens.lens (\ContextSummary' {source} -> source) (\s@ContextSummary' {} a -> s {source = a} :: ContextSummary)

-- | When the context was last modified.
contextSummary_lastModifiedTime :: Lens.Lens' ContextSummary (Core.Maybe Core.UTCTime)
contextSummary_lastModifiedTime = Lens.lens (\ContextSummary' {lastModifiedTime} -> lastModifiedTime) (\s@ContextSummary' {} a -> s {lastModifiedTime = a} :: ContextSummary) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the context.
contextSummary_contextArn :: Lens.Lens' ContextSummary (Core.Maybe Core.Text)
contextSummary_contextArn = Lens.lens (\ContextSummary' {contextArn} -> contextArn) (\s@ContextSummary' {} a -> s {contextArn = a} :: ContextSummary)

instance Core.FromJSON ContextSummary where
  parseJSON =
    Core.withObject
      "ContextSummary"
      ( \x ->
          ContextSummary'
            Core.<$> (x Core..:? "ContextType")
            Core.<*> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "ContextName")
            Core.<*> (x Core..:? "Source")
            Core.<*> (x Core..:? "LastModifiedTime")
            Core.<*> (x Core..:? "ContextArn")
      )

instance Core.Hashable ContextSummary

instance Core.NFData ContextSummary
