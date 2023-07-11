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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.ActionTypeUrls where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
    -- | The URL returned to the CodePipeline console that provides a deep link
    -- to the resources of the external system, such as a status page. This
    -- link is provided as part of the action display in the pipeline.
    entityUrlTemplate :: Prelude.Maybe Prelude.Text,
    -- | The link to an execution page for the action type in progress. For
    -- example, for a CodeDeploy action, this link is shown on the pipeline
    -- view page in the CodePipeline console, and it links to a CodeDeploy
    -- status page.
    executionUrlTemplate :: Prelude.Maybe Prelude.Text,
    -- | The URL returned to the CodePipeline console that contains a link to the
    -- page where customers can update or change the configuration of the
    -- external action.
    revisionUrlTemplate :: Prelude.Maybe Prelude.Text
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
-- 'entityUrlTemplate', 'actionTypeUrls_entityUrlTemplate' - The URL returned to the CodePipeline console that provides a deep link
-- to the resources of the external system, such as a status page. This
-- link is provided as part of the action display in the pipeline.
--
-- 'executionUrlTemplate', 'actionTypeUrls_executionUrlTemplate' - The link to an execution page for the action type in progress. For
-- example, for a CodeDeploy action, this link is shown on the pipeline
-- view page in the CodePipeline console, and it links to a CodeDeploy
-- status page.
--
-- 'revisionUrlTemplate', 'actionTypeUrls_revisionUrlTemplate' - The URL returned to the CodePipeline console that contains a link to the
-- page where customers can update or change the configuration of the
-- external action.
newActionTypeUrls ::
  ActionTypeUrls
newActionTypeUrls =
  ActionTypeUrls'
    { configurationUrl = Prelude.Nothing,
      entityUrlTemplate = Prelude.Nothing,
      executionUrlTemplate = Prelude.Nothing,
      revisionUrlTemplate = Prelude.Nothing
    }

-- | The URL returned to the CodePipeline console that contains a link to the
-- page where customers can configure the external action.
actionTypeUrls_configurationUrl :: Lens.Lens' ActionTypeUrls (Prelude.Maybe Prelude.Text)
actionTypeUrls_configurationUrl = Lens.lens (\ActionTypeUrls' {configurationUrl} -> configurationUrl) (\s@ActionTypeUrls' {} a -> s {configurationUrl = a} :: ActionTypeUrls)

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

-- | The URL returned to the CodePipeline console that contains a link to the
-- page where customers can update or change the configuration of the
-- external action.
actionTypeUrls_revisionUrlTemplate :: Lens.Lens' ActionTypeUrls (Prelude.Maybe Prelude.Text)
actionTypeUrls_revisionUrlTemplate = Lens.lens (\ActionTypeUrls' {revisionUrlTemplate} -> revisionUrlTemplate) (\s@ActionTypeUrls' {} a -> s {revisionUrlTemplate = a} :: ActionTypeUrls)

instance Data.FromJSON ActionTypeUrls where
  parseJSON =
    Data.withObject
      "ActionTypeUrls"
      ( \x ->
          ActionTypeUrls'
            Prelude.<$> (x Data..:? "configurationUrl")
            Prelude.<*> (x Data..:? "entityUrlTemplate")
            Prelude.<*> (x Data..:? "executionUrlTemplate")
            Prelude.<*> (x Data..:? "revisionUrlTemplate")
      )

instance Prelude.Hashable ActionTypeUrls where
  hashWithSalt _salt ActionTypeUrls' {..} =
    _salt
      `Prelude.hashWithSalt` configurationUrl
      `Prelude.hashWithSalt` entityUrlTemplate
      `Prelude.hashWithSalt` executionUrlTemplate
      `Prelude.hashWithSalt` revisionUrlTemplate

instance Prelude.NFData ActionTypeUrls where
  rnf ActionTypeUrls' {..} =
    Prelude.rnf configurationUrl
      `Prelude.seq` Prelude.rnf entityUrlTemplate
      `Prelude.seq` Prelude.rnf executionUrlTemplate
      `Prelude.seq` Prelude.rnf revisionUrlTemplate

instance Data.ToJSON ActionTypeUrls where
  toJSON ActionTypeUrls' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("configurationUrl" Data..=)
              Prelude.<$> configurationUrl,
            ("entityUrlTemplate" Data..=)
              Prelude.<$> entityUrlTemplate,
            ("executionUrlTemplate" Data..=)
              Prelude.<$> executionUrlTemplate,
            ("revisionUrlTemplate" Data..=)
              Prelude.<$> revisionUrlTemplate
          ]
      )
