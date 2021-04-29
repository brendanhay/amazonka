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
-- Module      : Network.AWS.SageMaker.Types.NotebookInstanceLifecycleHook
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.NotebookInstanceLifecycleHook where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains the notebook instance lifecycle configuration script.
--
-- Each lifecycle configuration script has a limit of 16384 characters.
--
-- The value of the @$PATH@ environment variable that is available to both
-- scripts is @\/sbin:bin:\/usr\/sbin:\/usr\/bin@.
--
-- View CloudWatch Logs for notebook instance lifecycle configurations in
-- log group @\/aws\/sagemaker\/NotebookInstances@ in log stream
-- @[notebook-instance-name]\/[LifecycleConfigHook]@.
--
-- Lifecycle configuration scripts cannot run for longer than 5 minutes. If
-- a script runs for longer than 5 minutes, it fails and the notebook
-- instance is not created or started.
--
-- For information about notebook instance lifestyle configurations, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance>.
--
-- /See:/ 'newNotebookInstanceLifecycleHook' smart constructor.
data NotebookInstanceLifecycleHook = NotebookInstanceLifecycleHook'
  { -- | A base64-encoded string that contains a shell script for a notebook
    -- instance lifecycle configuration.
    content :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'NotebookInstanceLifecycleHook' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'content', 'notebookInstanceLifecycleHook_content' - A base64-encoded string that contains a shell script for a notebook
-- instance lifecycle configuration.
newNotebookInstanceLifecycleHook ::
  NotebookInstanceLifecycleHook
newNotebookInstanceLifecycleHook =
  NotebookInstanceLifecycleHook'
    { content =
        Prelude.Nothing
    }

-- | A base64-encoded string that contains a shell script for a notebook
-- instance lifecycle configuration.
notebookInstanceLifecycleHook_content :: Lens.Lens' NotebookInstanceLifecycleHook (Prelude.Maybe Prelude.Text)
notebookInstanceLifecycleHook_content = Lens.lens (\NotebookInstanceLifecycleHook' {content} -> content) (\s@NotebookInstanceLifecycleHook' {} a -> s {content = a} :: NotebookInstanceLifecycleHook)

instance
  Prelude.FromJSON
    NotebookInstanceLifecycleHook
  where
  parseJSON =
    Prelude.withObject
      "NotebookInstanceLifecycleHook"
      ( \x ->
          NotebookInstanceLifecycleHook'
            Prelude.<$> (x Prelude..:? "Content")
      )

instance
  Prelude.Hashable
    NotebookInstanceLifecycleHook

instance Prelude.NFData NotebookInstanceLifecycleHook

instance Prelude.ToJSON NotebookInstanceLifecycleHook where
  toJSON NotebookInstanceLifecycleHook' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("Content" Prelude..=) Prelude.<$> content]
      )
