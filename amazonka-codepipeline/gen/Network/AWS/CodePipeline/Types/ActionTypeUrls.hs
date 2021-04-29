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
-- Module      : Network.AWS.CodePipeline.Types.ActionTypeUrls
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionTypeUrls where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Returns information about URLs for web pages that display to customers
-- as links on the pipeline view, such as an external configuration page
-- for the action type.
--
-- /See:/ 'newActionTypeUrls' smart constructor.
data ActionTypeUrls = ActionTypeUrls'
  { -- | The link to an execution page for the action type in progress. For
    -- example, for a CodeDeploy action, this link is shown on the pipeline
    -- view page in the CodePipeline console, and it links to a CodeDeploy
    -- status page.
    executionUrlTemplate :: Prelude.Maybe Prelude.Text,
    -- | The URL returned to the CodePipeline console that provides a deep link
    -- to the resources of the external system, such as a status page. This
    -- link is provided as part of the action display in the pipeline.
    entityUrlTemplate :: Prelude.Maybe Prelude.Text,
    -- | The URL returned to the CodePipeline console that contains a link to the
    -- page where customers can update or change the configuration of the
    -- external action.
    revisionUrlTemplate :: Prelude.Maybe Prelude.Text,
    -- | The URL returned to the CodePipeline console that contains a link to the
    -- page where customers can configure the external action.
    configurationUrl :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ActionTypeUrls' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'executionUrlTemplate', 'actionTypeUrls_executionUrlTemplate' - The link to an execution page for the action type in progress. For
-- example, for a CodeDeploy action, this link is shown on the pipeline
-- view page in the CodePipeline console, and it links to a CodeDeploy
-- status page.
--
-- 'entityUrlTemplate', 'actionTypeUrls_entityUrlTemplate' - The URL returned to the CodePipeline console that provides a deep link
-- to the resources of the external system, such as a status page. This
-- link is provided as part of the action display in the pipeline.
--
-- 'revisionUrlTemplate', 'actionTypeUrls_revisionUrlTemplate' - The URL returned to the CodePipeline console that contains a link to the
-- page where customers can update or change the configuration of the
-- external action.
--
-- 'configurationUrl', 'actionTypeUrls_configurationUrl' - The URL returned to the CodePipeline console that contains a link to the
-- page where customers can configure the external action.
newActionTypeUrls ::
  ActionTypeUrls
newActionTypeUrls =
  ActionTypeUrls'
    { executionUrlTemplate =
        Prelude.Nothing,
      entityUrlTemplate = Prelude.Nothing,
      revisionUrlTemplate = Prelude.Nothing,
      configurationUrl = Prelude.Nothing
    }

-- | The link to an execution page for the action type in progress. For
-- example, for a CodeDeploy action, this link is shown on the pipeline
-- view page in the CodePipeline console, and it links to a CodeDeploy
-- status page.
actionTypeUrls_executionUrlTemplate :: Lens.Lens' ActionTypeUrls (Prelude.Maybe Prelude.Text)
actionTypeUrls_executionUrlTemplate = Lens.lens (\ActionTypeUrls' {executionUrlTemplate} -> executionUrlTemplate) (\s@ActionTypeUrls' {} a -> s {executionUrlTemplate = a} :: ActionTypeUrls)

-- | The URL returned to the CodePipeline console that provides a deep link
-- to the resources of the external system, such as a status page. This
-- link is provided as part of the action display in the pipeline.
actionTypeUrls_entityUrlTemplate :: Lens.Lens' ActionTypeUrls (Prelude.Maybe Prelude.Text)
actionTypeUrls_entityUrlTemplate = Lens.lens (\ActionTypeUrls' {entityUrlTemplate} -> entityUrlTemplate) (\s@ActionTypeUrls' {} a -> s {entityUrlTemplate = a} :: ActionTypeUrls)

-- | The URL returned to the CodePipeline console that contains a link to the
-- page where customers can update or change the configuration of the
-- external action.
actionTypeUrls_revisionUrlTemplate :: Lens.Lens' ActionTypeUrls (Prelude.Maybe Prelude.Text)
actionTypeUrls_revisionUrlTemplate = Lens.lens (\ActionTypeUrls' {revisionUrlTemplate} -> revisionUrlTemplate) (\s@ActionTypeUrls' {} a -> s {revisionUrlTemplate = a} :: ActionTypeUrls)

-- | The URL returned to the CodePipeline console that contains a link to the
-- page where customers can configure the external action.
actionTypeUrls_configurationUrl :: Lens.Lens' ActionTypeUrls (Prelude.Maybe Prelude.Text)
actionTypeUrls_configurationUrl = Lens.lens (\ActionTypeUrls' {configurationUrl} -> configurationUrl) (\s@ActionTypeUrls' {} a -> s {configurationUrl = a} :: ActionTypeUrls)

instance Prelude.FromJSON ActionTypeUrls where
  parseJSON =
    Prelude.withObject
      "ActionTypeUrls"
      ( \x ->
          ActionTypeUrls'
            Prelude.<$> (x Prelude..:? "executionUrlTemplate")
            Prelude.<*> (x Prelude..:? "entityUrlTemplate")
            Prelude.<*> (x Prelude..:? "revisionUrlTemplate")
            Prelude.<*> (x Prelude..:? "configurationUrl")
      )

instance Prelude.Hashable ActionTypeUrls

instance Prelude.NFData ActionTypeUrls

instance Prelude.ToJSON ActionTypeUrls where
  toJSON ActionTypeUrls' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("executionUrlTemplate" Prelude..=)
              Prelude.<$> executionUrlTemplate,
            ("entityUrlTemplate" Prelude..=)
              Prelude.<$> entityUrlTemplate,
            ("revisionUrlTemplate" Prelude..=)
              Prelude.<$> revisionUrlTemplate,
            ("configurationUrl" Prelude..=)
              Prelude.<$> configurationUrl
          ]
      )
