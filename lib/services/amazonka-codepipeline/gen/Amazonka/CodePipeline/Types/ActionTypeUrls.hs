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
-- Module      : Amazonka.CodePipeline.Types.ActionTypeUrls
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.ActionTypeUrls where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Returns information about URLs for web pages that display to customers
-- as links on the pipeline view, such as an external configuration page
-- for the action type.
--
-- /See:/ 'newActionTypeUrls' smart constructor.
data ActionTypeUrls = ActionTypeUrls'
  { -- | The URL returned to the CodePipeline console that contains a link to the
    -- page where customers can configure the external action.
    configurationUrl :: Prelude.Maybe Prelude.Text,
    -- | The URL returned to the CodePipeline console that contains a link to the
    -- page where customers can update or change the configuration of the
    -- external action.
    revisionUrlTemplate :: Prelude.Maybe Prelude.Text,
    -- | The URL returned to the CodePipeline console that provides a deep link
    -- to the resources of the external system, such as a status page. This
    -- link is provided as part of the action display in the pipeline.
    entityUrlTemplate :: Prelude.Maybe Prelude.Text,
    -- | The link to an execution page for the action type in progress. For
    -- example, for a CodeDeploy action, this link is shown on the pipeline
    -- view page in the CodePipeline console, and it links to a CodeDeploy
    -- status page.
    executionUrlTemplate :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActionTypeUrls' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationUrl', 'actionTypeUrls_configurationUrl' - The URL returned to the CodePipeline console that contains a link to the
-- page where customers can configure the external action.
--
-- 'revisionUrlTemplate', 'actionTypeUrls_revisionUrlTemplate' - The URL returned to the CodePipeline console that contains a link to the
-- page where customers can update or change the configuration of the
-- external action.
--
-- 'entityUrlTemplate', 'actionTypeUrls_entityUrlTemplate' - The URL returned to the CodePipeline console that provides a deep link
-- to the resources of the external system, such as a status page. This
-- link is provided as part of the action display in the pipeline.
--
-- 'executionUrlTemplate', 'actionTypeUrls_executionUrlTemplate' - The link to an execution page for the action type in progress. For
-- example, for a CodeDeploy action, this link is shown on the pipeline
-- view page in the CodePipeline console, and it links to a CodeDeploy
-- status page.
newActionTypeUrls ::
  ActionTypeUrls
newActionTypeUrls =
  ActionTypeUrls'
    { configurationUrl = Prelude.Nothing,
      revisionUrlTemplate = Prelude.Nothing,
      entityUrlTemplate = Prelude.Nothing,
      executionUrlTemplate = Prelude.Nothing
    }

-- | The URL returned to the CodePipeline console that contains a link to the
-- page where customers can configure the external action.
actionTypeUrls_configurationUrl :: Lens.Lens' ActionTypeUrls (Prelude.Maybe Prelude.Text)
actionTypeUrls_configurationUrl = Lens.lens (\ActionTypeUrls' {configurationUrl} -> configurationUrl) (\s@ActionTypeUrls' {} a -> s {configurationUrl = a} :: ActionTypeUrls)

-- | The URL returned to the CodePipeline console that contains a link to the
-- page where customers can update or change the configuration of the
-- external action.
actionTypeUrls_revisionUrlTemplate :: Lens.Lens' ActionTypeUrls (Prelude.Maybe Prelude.Text)
actionTypeUrls_revisionUrlTemplate = Lens.lens (\ActionTypeUrls' {revisionUrlTemplate} -> revisionUrlTemplate) (\s@ActionTypeUrls' {} a -> s {revisionUrlTemplate = a} :: ActionTypeUrls)

-- | The URL returned to the CodePipeline console that provides a deep link
-- to the resources of the external system, such as a status page. This
-- link is provided as part of the action display in the pipeline.
actionTypeUrls_entityUrlTemplate :: Lens.Lens' ActionTypeUrls (Prelude.Maybe Prelude.Text)
actionTypeUrls_entityUrlTemplate = Lens.lens (\ActionTypeUrls' {entityUrlTemplate} -> entityUrlTemplate) (\s@ActionTypeUrls' {} a -> s {entityUrlTemplate = a} :: ActionTypeUrls)

-- | The link to an execution page for the action type in progress. For
-- example, for a CodeDeploy action, this link is shown on the pipeline
-- view page in the CodePipeline console, and it links to a CodeDeploy
-- status page.
actionTypeUrls_executionUrlTemplate :: Lens.Lens' ActionTypeUrls (Prelude.Maybe Prelude.Text)
actionTypeUrls_executionUrlTemplate = Lens.lens (\ActionTypeUrls' {executionUrlTemplate} -> executionUrlTemplate) (\s@ActionTypeUrls' {} a -> s {executionUrlTemplate = a} :: ActionTypeUrls)

instance Core.FromJSON ActionTypeUrls where
  parseJSON =
    Core.withObject
      "ActionTypeUrls"
      ( \x ->
          ActionTypeUrls'
            Prelude.<$> (x Core..:? "configurationUrl")
            Prelude.<*> (x Core..:? "revisionUrlTemplate")
            Prelude.<*> (x Core..:? "entityUrlTemplate")
            Prelude.<*> (x Core..:? "executionUrlTemplate")
      )

instance Prelude.Hashable ActionTypeUrls where
  hashWithSalt _salt ActionTypeUrls' {..} =
    _salt `Prelude.hashWithSalt` configurationUrl
      `Prelude.hashWithSalt` revisionUrlTemplate
      `Prelude.hashWithSalt` entityUrlTemplate
      `Prelude.hashWithSalt` executionUrlTemplate

instance Prelude.NFData ActionTypeUrls where
  rnf ActionTypeUrls' {..} =
    Prelude.rnf configurationUrl
      `Prelude.seq` Prelude.rnf revisionUrlTemplate
      `Prelude.seq` Prelude.rnf entityUrlTemplate
      `Prelude.seq` Prelude.rnf executionUrlTemplate

instance Core.ToJSON ActionTypeUrls where
  toJSON ActionTypeUrls' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("configurationUrl" Core..=)
              Prelude.<$> configurationUrl,
            ("revisionUrlTemplate" Core..=)
              Prelude.<$> revisionUrlTemplate,
            ("entityUrlTemplate" Core..=)
              Prelude.<$> entityUrlTemplate,
            ("executionUrlTemplate" Core..=)
              Prelude.<$> executionUrlTemplate
          ]
      )
