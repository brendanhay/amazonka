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
-- Module      : Network.AWS.SageMaker.Types.NotebookInstanceLifecycleConfigSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.NotebookInstanceLifecycleConfigSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides a summary of a notebook instance lifecycle configuration.
--
-- /See:/ 'newNotebookInstanceLifecycleConfigSummary' smart constructor.
data NotebookInstanceLifecycleConfigSummary = NotebookInstanceLifecycleConfigSummary'
  { -- | A timestamp that tells when the lifecycle configuration was created.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | A timestamp that tells when the lifecycle configuration was last
    -- modified.
    lastModifiedTime :: Prelude.Maybe Prelude.POSIX,
    -- | The name of the lifecycle configuration.
    notebookInstanceLifecycleConfigName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the lifecycle configuration.
    notebookInstanceLifecycleConfigArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'NotebookInstanceLifecycleConfigSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'notebookInstanceLifecycleConfigSummary_creationTime' - A timestamp that tells when the lifecycle configuration was created.
--
-- 'lastModifiedTime', 'notebookInstanceLifecycleConfigSummary_lastModifiedTime' - A timestamp that tells when the lifecycle configuration was last
-- modified.
--
-- 'notebookInstanceLifecycleConfigName', 'notebookInstanceLifecycleConfigSummary_notebookInstanceLifecycleConfigName' - The name of the lifecycle configuration.
--
-- 'notebookInstanceLifecycleConfigArn', 'notebookInstanceLifecycleConfigSummary_notebookInstanceLifecycleConfigArn' - The Amazon Resource Name (ARN) of the lifecycle configuration.
newNotebookInstanceLifecycleConfigSummary ::
  -- | 'notebookInstanceLifecycleConfigName'
  Prelude.Text ->
  -- | 'notebookInstanceLifecycleConfigArn'
  Prelude.Text ->
  NotebookInstanceLifecycleConfigSummary
newNotebookInstanceLifecycleConfigSummary
  pNotebookInstanceLifecycleConfigName_
  pNotebookInstanceLifecycleConfigArn_ =
    NotebookInstanceLifecycleConfigSummary'
      { creationTime =
          Prelude.Nothing,
        lastModifiedTime = Prelude.Nothing,
        notebookInstanceLifecycleConfigName =
          pNotebookInstanceLifecycleConfigName_,
        notebookInstanceLifecycleConfigArn =
          pNotebookInstanceLifecycleConfigArn_
      }

-- | A timestamp that tells when the lifecycle configuration was created.
notebookInstanceLifecycleConfigSummary_creationTime :: Lens.Lens' NotebookInstanceLifecycleConfigSummary (Prelude.Maybe Prelude.UTCTime)
notebookInstanceLifecycleConfigSummary_creationTime = Lens.lens (\NotebookInstanceLifecycleConfigSummary' {creationTime} -> creationTime) (\s@NotebookInstanceLifecycleConfigSummary' {} a -> s {creationTime = a} :: NotebookInstanceLifecycleConfigSummary) Prelude.. Lens.mapping Prelude._Time

-- | A timestamp that tells when the lifecycle configuration was last
-- modified.
notebookInstanceLifecycleConfigSummary_lastModifiedTime :: Lens.Lens' NotebookInstanceLifecycleConfigSummary (Prelude.Maybe Prelude.UTCTime)
notebookInstanceLifecycleConfigSummary_lastModifiedTime = Lens.lens (\NotebookInstanceLifecycleConfigSummary' {lastModifiedTime} -> lastModifiedTime) (\s@NotebookInstanceLifecycleConfigSummary' {} a -> s {lastModifiedTime = a} :: NotebookInstanceLifecycleConfigSummary) Prelude.. Lens.mapping Prelude._Time

-- | The name of the lifecycle configuration.
notebookInstanceLifecycleConfigSummary_notebookInstanceLifecycleConfigName :: Lens.Lens' NotebookInstanceLifecycleConfigSummary Prelude.Text
notebookInstanceLifecycleConfigSummary_notebookInstanceLifecycleConfigName = Lens.lens (\NotebookInstanceLifecycleConfigSummary' {notebookInstanceLifecycleConfigName} -> notebookInstanceLifecycleConfigName) (\s@NotebookInstanceLifecycleConfigSummary' {} a -> s {notebookInstanceLifecycleConfigName = a} :: NotebookInstanceLifecycleConfigSummary)

-- | The Amazon Resource Name (ARN) of the lifecycle configuration.
notebookInstanceLifecycleConfigSummary_notebookInstanceLifecycleConfigArn :: Lens.Lens' NotebookInstanceLifecycleConfigSummary Prelude.Text
notebookInstanceLifecycleConfigSummary_notebookInstanceLifecycleConfigArn = Lens.lens (\NotebookInstanceLifecycleConfigSummary' {notebookInstanceLifecycleConfigArn} -> notebookInstanceLifecycleConfigArn) (\s@NotebookInstanceLifecycleConfigSummary' {} a -> s {notebookInstanceLifecycleConfigArn = a} :: NotebookInstanceLifecycleConfigSummary)

instance
  Prelude.FromJSON
    NotebookInstanceLifecycleConfigSummary
  where
  parseJSON =
    Prelude.withObject
      "NotebookInstanceLifecycleConfigSummary"
      ( \x ->
          NotebookInstanceLifecycleConfigSummary'
            Prelude.<$> (x Prelude..:? "CreationTime")
            Prelude.<*> (x Prelude..:? "LastModifiedTime")
            Prelude.<*> (x Prelude..: "NotebookInstanceLifecycleConfigName")
            Prelude.<*> (x Prelude..: "NotebookInstanceLifecycleConfigArn")
      )

instance
  Prelude.Hashable
    NotebookInstanceLifecycleConfigSummary

instance
  Prelude.NFData
    NotebookInstanceLifecycleConfigSummary
